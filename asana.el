;;; asana.el --- Quickly create, view and bulk-update Asana tasks.   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Leo Martel <leo@lpm.io>

;; Author: Leo Martel <leo@lpm.io>
;;         Renato Ferreira <renatofdds@gmail.com>
;; Maintainer: Leo Martel <leo@lpm.io>
;; Version: 0.4.0
;; Keywords: comm outlines tools
;; Homepage: https://github.com/lmartel/emacs-asana
;; Package-Requires: ((emacs "25.1"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides utilities for working with Asana (https://asana.com) tasks:
;; - `asana-create-task' and `asana-create-task-quickly' Create tasks from the minibuffer
;; - `asana-org-sync-tasks' and `asana-org-sync-task-at-point' Dump your tasks to org-mode TODOs
;; - `asana-helm' List and act on your tasks in bulk with `helm' extensions

;;; Code:

(eval-when-compile
  (require 'cl-lib)
	(require 'subr-x))

(require 'map)
(require 'url-queue)

(cl-eval-when (compile)
	(require 'org nil t)
	(require 'helm-core nil t))

;; User config variables

(defvar asana-token nil
  "Asana access token.
If nil, fall back to the environment variable `asana-token-env-var'.")

(defvar asana-token-env-var "ASANA_TOKEN"
  "Environment variable for an Asana access token.")

(defcustom asana-task-buffer-format :org
  "Default format of Asana task buffer."
  :group 'asana
  :type '(choice (const :lisp) (const :org)))

(defcustom asana-api-concurrent-requests 24
  "The number of concurrent requests to Asana API.
Asana API has a hard limit of 50."
  :group 'asana
  :type 'integer)

(defcustom asana-api-requests-per-minute 150
  "The number of concurrent requests to Asana API.
Asana free plan limit is 50. Asana premium plan limit is 1500."
  :group 'asana
  :type 'integer)

(defcustom asana-api-timeout 60
  "Number of seconds to timeout requests to Asana API."
  :group 'asana
  :type 'integer)

(defcustom asana-default-workspace nil
  "Default workspace to operate on."
  :group 'asana
  :type '(choice (const :tag "Not set" nil)
								 (cons :tag "Name and GID" (string :tag "Name") (string :tag "GID")))
	:safe (lambda (x) (or (null x) (and (consp x) (stringp (car x)) (stringp (cdr x))))))

:type 'string
:safe (lambda (x) (stringp x))

(defcustom asana-tasks-org-file
  (expand-file-name "asana.org" (or (bound-and-true-p org-directory)
																		user-emacs-directory))
  "Org file to sync Asana tasks into."
  :group 'asana
  :type 'file)

;; Internal variables

(defconst asana-api-root "https://app.asana.com/api/1.0")

(defvar asana-workspace-refs (make-hash-table :test 'equal))
(defvar asana-current-workspace nil)

;; Helpers

(define-inline asana-workspace-gid ()
	(inline-quote (or (map-elt asana-current-workspace 'gid)
										(cdr asana-default-workspace))))

(define-inline asana-workspace-name ()
	(inline-quote (or (map-elt asana-current-workspace 'name)
										(map-elt asana-current-workspace 'gid)
										(car-safe asana-default-workspace))))

(defmacro with-asana-workspace (ws &rest body)
	(declare (indent 1) (debug body))
	`(let ((asana-current-workspace ,ws))
		 ,@body))

(defun asana--log (format &rest args)
	(let ((inhibit-message))
		(apply 'message (concat "Asana: " format) args)))

(defun asana--workspace-refs-fetch (workspace-gid)
	(unless (map-elt asana-workspace-refs workspace-gid)
		(let ((hm (make-hash-table :test 'equal)))
			(message "Fetching user names...")
			(seq-doseq (user (asana-get "/users" `((workspace ,workspace-gid))))
				(map-put! hm (map-elt user 'gid) (map-elt user 'name)))
			(message "Fetching tag names...")
			(seq-doseq (tag (asana-get "/tags" `((workspace ,workspace-gid))))
				(map-put! hm (map-elt tag 'gid) (map-elt tag 'name)))
			(message "Fetching project names...")
			(seq-doseq (project (asana-get "/projects" `((workspace ,workspace-gid))))
				(map-put! hm (map-elt project 'gid) (map-elt project 'name)))
			(setf (map-elt asana-workspace-refs workspace-gid) hm))))

(defconst asana-native-json-available-p (json-available-p))

(defun asana-json-serialize (object)
  (if asana-native-json-available-p
			(condition-case-unless-debug err
					(json-serialize
					 object
					 :null-object :null
					 :false-object :false)
				(error (asana--log "JSON serialization error: %S" err)))
		(condition-case-unless-debug err
				(let ((json-null :null)
							(json-false :false))
					(json-encode object))
			(error (asana--log "JSON serialization error: %S" err)))))

(defun asana-json-read-from-point ()
  (if asana-native-json-available-p
			(condition-case-unless-debug err
					(json-parse-buffer
					 :object-type 'alist
					 :array-type 'list
					 :null-object nil
					 :false-object :false)
				(json-parse-error (asana--log "json-parse-error: %S" err)))
		(condition-case-unless-debug err
				(let ((json-object-type 'alist)
							(json-array-type 'list)
							(json-null nil)
							(json-false :json-false))
					(json-read))
			(json-error (asana--log "json-parse-error: %S" err)))))

(defun asana-get-token ()
  "Get the user's specified Asana access token."
  (or asana-token (getenv asana-token-env-var)))

(defun asana-headers-with-auth (&optional extra-headers)
  "Create http headers alist with Asana token Authorization and EXTRA-HEADERS."
  (append `(("Authorization" . ,(concat "Bearer " (asana-get-token)))) extra-headers))

;; API

(defvar url-request-method)
(defvar url-request-data)
(defvar url-request-extra-headers)
(defvar url-queue-parallel-processes)
(defvar url-queue-timeout)
(defvar url-http-end-of-headers)

(defvar asana-get-async-queue nil)
(defvar asana-get-async-queue-start-time nil)
(defvar asana-async-error-handler (lambda (_error-data)))

(define-advice url-queue-run-queue (:around (f) "asana")
  (if (and url-queue
					 (seq-some (lambda (job) (string-prefix-p asana-api-root (url-queue-url job))) url-queue))
			(let ((url-queue-parallel-processes asana-api-concurrent-requests)
						(url-queue-timeout asana-api-timeout))
				(unless asana-get-async-queue
					(setq asana-get-async-queue url-queue
								asana-get-async-queue-start-time (float-time)))
				(when (or
							 (< (- (float-time) asana-get-async-queue-start-time) 60) ; Ignore first minute
							 (< (/ (- (length asana-get-async-queue) (length url-queue))
										 (- (float-time) asana-get-async-queue-start-time))
									(/ 60.0 asana-api-requests-per-minute)))
					(funcall f)))
		(setq asana-get-async-queue nil
					asana-get-async-queue-start-time nil)
		(funcall f)))

(define-advice url-queue-start-retrieve (:around (f job) "asana")
  (if (string-prefix-p asana-api-root (url-queue-url job))
			(let ((url-request-extra-headers (asana-headers-with-auth)))
				(funcall f job))
		(funcall f job)))

(defmacro asana-url-retrieve-callback (&rest body)
  (declare (debug (def-body)))
  `(let ((error-handler asana-async-error-handler))
		 (lambda (url-retrieve-status &rest _)
			 (let ((http-error (plist-get url-retrieve-status :error)))
				 (if (not http-error)
						 (condition-case callback-error
								 ,(macroexp-progn body)
							 (error
								(message "Asana API callback-error: %S" callback-error)
								(funcall error-handler (cdr callback-error))))
					 (message "Asana HTTP error: %s" (cdr http-error))
					 (funcall error-handler (cdr http-error)))))))

(define-error 'asana-api-error "Asana API error")

(defun asana-read-response (buf)
  "Read json data from http response in BUF, throw errors as `asana-api-error'."
  (let* ((response
					(with-current-buffer buf
						(goto-char url-http-end-of-headers)
						(asana-json-read-from-point)))
				 (errors (map-elt response 'errors)))
		(if errors
				(let (messages)
					(seq-doseq (err errors) (push (map-elt err 'message) messages))
					(signal 'asana-api-error messages))
			(map-elt response 'data))))

(defun asana-request (method resource &optional query data callback)
  "Request /RESOURCE path with http METHOD, QUERY and DATA.
If CALLBACK is provided, run asynchronously.

RESOURCE is the path of Asana API.
QUERY is an alist and sent as a query string.
DATA is sent as the request body as json ((\"data\" . DATA))."
	(declare (indent 2))
  (let ((url-request-method method)
        (url-request-extra-headers (asana-headers-with-auth))
        (url-request-data (and data (asana-json-serialize `((data . ,data)))))
        (url (concat asana-api-root resource
										 (and query
													(concat "?" (url-build-query-string (map-filter (lambda (_ v) v) query)))))))
		(asana--log "%s %s %s" method url (or url-request-data ""))
    (if callback
        (url-queue-retrieve
				 url
				 (asana-url-retrieve-callback
					(funcall callback (asana-read-response (current-buffer)))))
      (asana-read-response (url-retrieve-synchronously url nil nil asana-api-timeout)))))

(defun asana-get (resource &optional query callback)
  "GET /RESOURCE as parsed json with alist QUERY as query string.
If CALLBACK is provided, run asynchronously."
	(declare (indent 1))
	(asana-request "GET" resource query nil callback))

(defun asana-post (resource &optional data callback)
  "POST /RESOURCE with body as json ((\"data\" . DATA)).
If CALLBACK is provided, run asynchronously."
	(declare (indent 1))
  (asana-request "POST" resource nil data callback))

(defun asana-put (resource &optional data callback)
  "PUT /RESOURCE with body as json ((\"data\" . DATA)).
If CALLBACK is provided, run asynchronously."
	(declare (indent 1))
  (asana-request "PUT" resource nil data callback))

(defun asana-delete (resource &optional callback)
  "DELETE /RESOURCE.
If CALLBACK is provided, run asynchronously."
	(declare (indent 1))
  (asana-request "DELETE" resource nil nil callback))

;; Tasks

(defun asana-task-get (task-gid &optional callback)
  "Get task by TASK-GID. Run asynchronously if CALLBACK provided."
  (asana-get (format "/tasks/%s" task-gid) nil callback))

(defun asana-task-stories (task-gid &optional callback)
  "Get task stories by TASK-GID. Run asynchronously if CALLBACK provided.
Task stories are comments, edit history, etc."
  (asana-get (format "/tasks/%s/stories" task-gid) nil callback))

(defun asana-task-move-to-section (task-gid section-gid)
  "Move task with TASK-GID to section SECTION-GID."
  (asana-post (format "/sections/%s/addTask" section-gid)
		`((task . ,task-gid))
    (lambda (_) (asana--log "Moved task %s to section %s" task-gid section-gid))))

(defun asana-task-complete (task-gid)
  "Complete an Asana task by TASK-GID."
  (asana-put
			(format "/tasks/%s" task-gid)
		'((completed . t))
		(lambda (data)
			(let ((task-name (map-elt data 'name)))
				(if (assoc 'completed data)
						(message "`%s' completed." task-name)
					(message "Unknown error: couldn't complete `%s'" task-name))))))

(defun asana-task-delete (task-gid)
  "Delete an Asana task by TASK-GID."
  (asana-delete (format "/tasks/%s" task-gid)
		(lambda (_) (message "Task deleted."))))

(defun asana-task-browse (task-gid)
  "Browse to an Asana task by TASK-ID using `browse-url'."
  (browse-url (format "https://app.asana.com/0/%s/%s" (asana-workspace-gid) task-gid)))

(defun asana-task-display (task-gid)
  "Display TASK and STORIES in a buffer with format `asana-task-buffer-format'."
  (let ((task (asana-task-get task-gid))
				(stories (asana-task-stories task-gid)))
		(switch-to-buffer
		 (get-buffer-create
			(format
			 "*Asana Task %s/%s*"
			 (map-nested-elt task '(workspace name))
			 (map-elt task 'name))))
		(let ((inhibit-read-only t) (pt (point)))
			(erase-buffer)
			(pcase asana-task-buffer-format
				(:lisp
				 (asana-task-insert task stories)
				 (emacs-lisp-mode))
				(:org
				 (defvar org-startup-folded)
				 (require 'org)
				 (let ((org-startup-folded 'showeverything))
					 (org-mode)
					 (org-insert-heading-respect-content)
					 (asana-org-task-insert task stories))))
			(goto-char pt)
			(view-mode))))

(defun asana-task-insert (task stories)
  "Insert a lisp-formatted Asana TASK with STORIES into the current buffer."
  (insert "\xc\n;;; ===== TASK =====\n\n")
  (insert (pp task))
  (insert "\n\n\xc\n;;; ===== COMMENTS =====\n\n")
  (insert (pp stories)))

(defun asana-tasks-fetch-data (tasks callback)
  "Fetch tasks data and call CALLBACK."
  (let* ((tasks (map-into tasks 'hash-table))
				 (fetched 0)
				 (to-fetch (* 2 (map-length tasks)))
				 (reporter (make-progress-reporter "Fetching tasks..." 0 to-fetch)))
		(cl-flet ((check-done
							 ()
							 (cl-incf fetched)
							 (progress-reporter-update reporter fetched)
							 (when (>= fetched to-fetch)
								 (progress-reporter-done reporter)
								 (funcall callback (map-into tasks 'alist)))))
			(seq-doseq (task-gid (map-keys tasks))
				(let ((asana-async-error-handler
							 (lambda (_)
								 (map-delete tasks task-gid)
								 (check-done))))
					(asana-task-get
					 task-gid
					 (lambda (props)
						 (when (map-contains-key tasks task-gid)
							 (setf (map-elt (map-elt tasks task-gid) 'props) props))
						 (check-done)))
					(asana-task-stories
					 task-gid
					 (lambda (stories)
						 (when (map-contains-key tasks task-gid)
							 (setf (map-elt (map-elt tasks task-gid) 'stories) stories))
						 (check-done))))))))

;; Org

(defun asana-org-task-insert (task stories)
  "Insert an org-formatted Asana TASK with STORIES into the current buffer."
	(require 'org)
  (let ((workspace-gid (map-nested-elt task '(workspace gid)))
				(task-gid (map-elt task 'gid))
				(closed (eq (map-elt task 'completed) t))
        (has-schedule (map-elt task 'start_on))
        (has-deadline (map-elt task 'due_on))
        (has-parent (map-elt task 'parent))
				(tags (map-elt task 'tags))
        (notes (map-elt task 'notes))
        (liked (eq (map-elt task 'liked) t))
        (hearted (eq (map-elt task 'hearted) t)))
		(asana--workspace-refs-fetch workspace-gid)
    (insert
     (format
      "%s%s%s\n"
      (map-elt task 'name)
			(if has-parent
					(concat " ❬ " (map-nested-elt task '(parent name)))
				"")
      (if tags
          (format
					 "   %s:"
					 (apply 'concat
									(seq-map (lambda (tag) (concat ":" (string-replace " " "_" (map-elt tag 'name)))) tags)))
				"")))
		(if tags (org-align-tags))
    (insert
     (format
      "%s%s%s"
      (if closed
          (format "CLOSED: %s%s"
                  (format-time-string
                   "[%Y-%m-%d %a %H:%M]"
                   (date-to-time (map-elt task 'completed_at)))
                  (if (or has-schedule has-deadline) " "  "\n")) "")
      (if has-schedule
          (format "SCHEDULED: %s%s"
                  (format-time-string
                   "<%Y-%m-%d %a>"
                   (date-to-time (concat (map-elt task 'start_on) " 00:00")))
                  (if has-deadline " "  "\n")) "")
      (if has-deadline
          (format "DEADLINE: %s\n"
                  (format-time-string
                   "<%Y-%m-%d %a>"
                   (date-to-time (concat (map-elt task 'due_on) " 00:00")))) "")))
    (insert ":PROPERTIES:\n")
    (insert
     (format
      ":CREATED_AT: %s\n"
      (format-time-string
       "[%Y-%m-%d %a %H:%M]"
       (date-to-time (map-elt task 'created_at)))))
    (insert
     (format
      ":MODIFIED_AT: %s\n"
      (format-time-string
       "[%Y-%m-%d %a %H:%M]"
       (date-to-time (map-elt task 'modified_at)))))
    (insert
     (format ":ASANA_ID: %s-%s\n" workspace-gid task-gid))
    (insert
     (format ":ASANA_URL: [[https://app.asana.com/0/%s/%s]]\n" workspace-gid task-gid))
    (insert (format ":WORKSPACE: %s\n" (map-nested-elt task '(workspace name))))
    (insert (format ":ASSIGNEE: %s\n" (map-nested-elt task '(assignee name))))
		(when has-parent
			(insert
			 (format
				":PARENT_ID: %s-%s\n"
				workspace-gid
				(map-nested-elt task '(parent gid)))))
    (when hearted
			(insert (format ":HEARTS: %d\n" (map-elt task 'num_hearts))))
    (when liked
			(insert (format ":LIKES: %d\n" (map-elt task 'num_likes))))
		(when-let* ((projects (map-elt task 'projects))
								(not (null projects)))
			(insert ":PROJECTS: ")
			(seq-map (lambda (p) (insert (format "%s, " (map-elt p 'name))))
							 projects)
			(insert "\n"))
    (when-let* ((memberships (map-elt task 'memberships))
								(not (null memberships)))
			(insert ":MEMBERSHIPS: ")
			(seq-doseq (m memberships)
				(insert (format "%s, " (map-nested-elt m '(project name)))))
			(insert "\n"))
		(when-let* ((followers (map-elt task 'followers))
								(not (null followers)))
			(insert ":FOLLOWERS: ")
			(seq-doseq (f followers)
				(insert (format "%s, " (map-elt f 'name))))
			(insert "\n"))
		(when-let* ((custom_fields (seq-filter (lambda (f) (map-elt f 'enabled))
																					 (map-elt task 'custom_fields)))
								((not (null custom_fields))))
			(seq-doseq (f custom_fields)
				(when-let ((v (map-elt f 'display_value)))
					(insert (format ":CUSTOM_%s: \"%s\"\n"
													(string-replace " " "_" (upcase (map-elt f 'name)))
													(string-replace ", " "\" \"" v))))))
    (insert ":END:\n")
    (insert ":LOGBOOK:\n")
    (seq-doseq (entry (reverse stories))
      (insert
       (format
        "- %s (%s)\n"
        (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time (map-elt entry 'created_at)))
        (map-elt entry 'type)))
      (let ((content-start (point))
						(txt (map-elt entry 'text))
						(fill-prefix "  ")
						desc link idx gid)
				(while (string-match "https://app.asana.com/0/\\([^/]+\\)/?\\w+" txt idx)
					(setq gid (match-string 1 txt)
								desc (or (map-nested-elt asana-workspace-refs (list workspace-gid gid)) gid))
					(put-text-property 0 (length desc) 'invisible t desc) ;; prevents filling links at eol
					(setq
					 link (org-link-make-string (format "http://app.asana.com/0/%s" gid) desc)
					 txt (replace-match link t t txt)
					 idx (+ (match-beginning 0) (length link))))
				(insert
				 (format "  %s" (replace-regexp-in-string "\n" "\n  " txt t)))
				(fill-region content-start (point)))
			(insert "\n"))
		(insert ":END:\n")
		(when notes
			(newline-and-indent)
			(dolist (p (split-string
									(subst-char-in-string
									 ?\u00a0 ?\u0020 ; Replace non-breaking-space with space
									 (decode-coding-string notes 'utf-8-unix) t)
									"[\n\r]" t "[\n\r ]*"))
				(unless (string-prefix-p "-" p)
					(newline))
				(insert p)
				(call-interactively #'org-fill-paragraph)
				(newline 2)
				(delete-blank-lines)))
		(newline 2)
		(delete-blank-lines)))

(defvar org-log-done)
(defun asana-org-task-sync (task stories)
  "Write one TASK and its STORIES to the current org-mode buffer."
	(eval-and-compile (require 'org))
  (save-excursion
    (let* ((existing
            (org-find-property
             "ASANA_ID"
             (format "%s-%s"
                     (map-nested-elt task '(workspace gid))
                     (map-elt task 'gid))))
           id todo-state)
      (cond (existing
						 (goto-char existing)
						 (setq id (org-id-get-create))
						 (setq todo-state (org-get-todo-state))
						 (save-excursion (org-insert-heading-respect-content))
						 (delete-region
							existing
							(save-excursion (org-end-of-subtree t t) (point)))
						 (end-of-line))
						(t (org-insert-subheading nil)))
			(save-excursion
				(asana-org-task-insert task stories))
			(if (eq (map-elt task 'completed) t)
					(let (org-log-done)
						(org-todo (or (car-safe (member todo-state org-done-keywords))
													'done)))
				(org-todo (or (car-safe (member todo-state org-not-done-keywords))
											'nextset)))
      (when id (org-entry-put (point) "ID" id)))))

(defun asana-org-tasks-digest (tasks)
  "Dump TASKS into an org buffer backed by `asana-tasks-org-file'."
	(eval-and-compile
		(require 'org)
		(require 'org-element)
		(require 'org-indent))
  (switch-to-buffer (find-file asana-tasks-org-file))
	(goto-char (point-min))
	(org-element-cache-reset)
	(org-cycle '(64))
  (unless (re-search-forward "^* Asana" nil t)
		(goto-char (point-max))
		(newline 2)
		(insert "* Asana"))
  (seq-doseq (task (map-values tasks))
		(asana-org-task-sync (map-elt task 'props) (map-elt task 'stories)))
  (org-indent-indent-buffer)
  (org-sort-entries nil ?r nil nil "CREATED_AT")
  (org-content 2))

;; Helm

(defvar asana-helm-selected-workspace nil)
(define-inline asana-helm-workspace-gid ()
	(inline-quote
	 (or (map-elt asana-helm-selected-workspace 'gid)
			 (map-elt asana-current-workspace 'gid)
			 (cdr asana-default-workspace))))

(defvar asana-helm-selected-project nil)
(define-inline asana-helm-selected-project-gid ()
	(inline-quote (map-elt asana-helm-selected-project 'gid)))

(defvar asana-helm-show-completed-tasks nil)

(define-inline asana-helm-selected-project-name ()
	(inline-quote (map-elt asana-helm-selected-project 'name)))

(defmacro asana-helm-with-candidate-gid (&rest body)
	(declare (indent 0) (debug (def-body)))
  `(lambda (candidate &rest _)
		 (let ((gid (map-elt candidate 'gid)))
			 ,@body)))

(defmacro asana-helm-foreach-candidate (var &rest body)
  "Lambda wihch runs BODY for each helm marked candidate bound to VAR."
	(declare (indent 1) (debug (symbolp def-body)))
  `(lambda (&rest _)
		 (dolist (,var (helm-marked-candidates))
			 ,@body)))

(cl-defun asana-helm-resource
		(
		 path
		 &rest plist
		 &key
		 name
		 query
		 (fields (list "name"))
		 (display (lambda (r) (map-elt r 'name)))
		 (action (helm-make-actions "Select" #'identity))
		 &allow-other-keys
		 )
	"Helm Asana resource from `asana-api-root'/PATH.

Supported keywords:

- :query is an alist encoded as url query string
- :fields is a list merged into opt_fields query param
  Defaults to (\"name\")
- :display is a function from resource to candidate display.
  Defaults to 'name' property.
- :coerce is a function from resource to returned candidate value.
  Defaults to 'gid' property.

Other keywords are proxied to `helm-make-source', which see.

Returns list of selected candidates values."
	(declare (indent 1))
	(let ((resources (list (cons (format "Fetching /%s..." path) ()))))
		(apply
		 #'helm-make-source
		 (format "Asana: %s" (or name path))
		 'helm-source-sync
		 :volatile t
		 :fuzzy-match t
		 :candidates (lambda () resources)
		 :action action
		 :action-transformer #'asana-helm--action-transformer
		 :init
		 (lambda ()
			 (asana-get (concat "/" path)
				 (map-merge 'alist `((opt_fields ,(string-join (cons "resource_type" fields) ",")))
										query)
				 (lambda (response)
					 (setq resources (mapcar (lambda (resource) (cons (funcall display resource) resource))
																	 response))
					 (helm-update))))
		 (seq-reduce
			(lambda (pl k) (map-delete pl k))
			[:name :query :actions :fields :display]
			plist))))

(defun asana-helm-resource-gid (&rest args)
	(declare (indent 1))
	(map-elt (helm (apply #'asana-helm-resource args)) 'gid))

(defun asana-helm-tasks-move-to-section (task-gids)
  "Prompt section, then move a list of Asana tasks to it by their TASK-GIDS."
	(unless (asana-helm-selected-project-gid)
		(setq asana-helm-selected-project (helm (asana-helm-resource "projects"
																							:query `((workspace ,(asana-workspace-gid)))))))
	(when-let ((project-gid (asana-helm-selected-project-gid))
						 (section-gid (asana-helm-resource-gid (format "projects/%s/sections" project-gid))))
		(dolist (task-gid task-gids)
			(asana-task-move-to-section task-gid section-gid))))

(defun asana-helm--action-transformer (actions candidate)
	(pcase (map-elt candidate 'resource_type)
		("workspace" actions)
		("project" actions)
		((or "task" "subtask")
		 (append
			actions
			(helm-make-actions
			 "Open in Browser" (asana-helm-with-candidate-gid (asana-task-browse gid))
			 "Mark Completed" (asana-helm-with-candidate-gid (asana-task-complete gid))
			 "Delete" (asana-helm-with-candidate-gid (asana-task-delete gid))
			 "Move marked Tasks" (lambda (_) (asana-helm-tasks-move-to-section (mapcar (lambda (c) (map-elt c 'gid)) (helm-marked-candidates))))
			 "Complete marked Tasks" (asana-helm-foreach-candidate c (asana-task-complete (map-elt c 'gid)))
			 "Delete marked Tasks" (asana-helm-foreach-candidate c (asana-task-delete (map-elt c 'gid))))
			))
		(_ actions)))

(defmacro asana-helm-define-exit-actions (keymap key def &rest bindings)
  "In KEYMAP, for each KEY/DEF in BINDINGS define key sequence KEY as DEF."
  (declare (indent defun))
  (let ((ret '(progn)))
    (while key
      (push
       `(define-key ,keymap ,key
          (lambda ()
            (interactive)
            (helm-exit-and-execute-action ,def)))
       ret)
      (setq key (pop bindings)
            def (pop bindings)))
    (reverse ret)))

(defun asana-helm--task-format (task)
	(let ((max-len (- (window-width) 25)))
		(format "%s   %20s"
						(truncate-string-to-width (map-elt task 'name) max-len 0 ?\s "…")
						(propertize (or (map-elt task 'due_on) "") 'face 'warning))))

(with-eval-after-load 'helm-core
	(defvar asana-helm-tasks-keymap
		(let ((map (make-sparse-keymap)))
			(set-keymap-parent map helm-map)
			(asana-helm-define-exit-actions map
				(kbd "C-b") (asana-helm-with-candidate-gid (asana-task-browse gid))
				(kbd "<C-return>") (asana-helm-with-candidate-gid (asana-task-complete gid))
				(kbd "<C-backspace>") (asana-helm-with-candidate-gid (asana-task-delete gid))
				(kbd "<M-return>") (asana-helm-foreach-candidate c (asana-task-complete c))
				(kbd "<M-backspace>") (asana-helm-foreach-candidate c (asana-task-delete c))
				(kbd "C-]") (lambda (_)
											(setq asana-helm-show-completed-tasks (not asana-helm-show-completed-tasks))
											(asana-helm))
				(kbd "C-:") (lambda (_)
											(asana-helm-tasks-move-to-section
											 (mapcar (lambda (c) (map-elt c 'gid)) (helm-marked-candidates)))))
			map))

	(cl-defun asana-helm--task-make-source (&rest plist &key
																								(query)
																								(keymap asana-helm-tasks-keymap)
																								(display 'asana-helm--task-format)
																								(fields '("name" "due_on"))
																								action
																								&allow-other-keys)
		(apply
		 #'asana-helm-resource
		 "tasks"
		 :query (map-merge 'alist (if asana-helm-show-completed-tasks nil '((completed_since "now"))) query)
		 :keymap keymap
		 :display display
		 :fields fields
		 :action (or action (helm-make-actions "Show" (asana-helm-with-candidate-gid (asana-task-display gid))))
	 plist)))

;; Interactive

;;;###autoload
(defun asana-default-workspace-change (workspace)
  "Set and save `asana-default-workspace' as WORKSPACE."
	(interactive
	 (let* ((wss (map-elt (asana-get "/users/me") 'workspaces))
					(candidates (mapcar (lambda (workspace) `(,(map-elt workspace 'name) . ,workspace))
															wss)))
		 (list
			(map-elt
			 candidates
			 (completing-read "Workspace: " candidates nil t)))))
	(when workspace
		(customize-save-variable
		 'asana-default-workspace
		 (cons (map-elt workspace 'name) (map-elt workspace 'gid)))))

;;;###autoload
(defun asana-workspace-change (workspace)
  "Set `asana-current-workspace' as WORKSPACE."
	(interactive
	 (let* ((wss (map-elt (asana-get "/users/me") 'workspaces))
					(candidates (mapcar (lambda (workspace) `(,(map-elt workspace 'name) . ,workspace))
															wss)))
		 (list
			(map-elt
			 candidates
			 (completing-read "Workspace: " candidates nil t)))))
	(when workspace
		(setq asana-current-workspace workspace)))

;;;###autoload
(defun asana-create-task (task-name &optional description)
  "Create task with TASK-NAME and optional DESCRIPTION.
If called interactively, ask for both."
  (interactive "sCreate Asana Task: \nsTask Description: ")
  (asana-post "/tasks"
		`((workspace . ,(asana-workspace-gid))
      (assignee . "me")
      (name . ,task-name)
      (notes . ,(or description "")))
    (lambda (data)
      (let ((task-name (map-elt data 'name)))
        (if task-name
            (message "Created task: `%s'." task-name)
          (message "Unknown error: couldn't create task."))))))

;;;###autoload
(defun asana-create-task-quickly (task-name)
  "Create a task TASK-NAME with no description."
  (interactive "sQuick-Create Asana Task: ")
  (asana-create-task task-name))

;;;###autoload
(defun asana-org-sync-tasks (&optional query)
  "One-way-sync all user own open tasks to `asana-tasks-org-file'.
Update previously downloaded tasks in-place according to org tags search QUERY;
Append newly discovered tasks.

Slow for large projects!"
  (interactive)
	(require 'org)
	(unless (asana-workspace-gid)
		(call-interactively #'asana-workspace-change))
  (message "Fetching tasks...")
	(asana-get "/tasks"
		`((workspace ,(asana-workspace-gid))
			(opt_fields "name")
			(assignee "me")
			(completed_since "now"))
		(lambda (tasks)
			(switch-to-buffer (find-file asana-tasks-org-file))
			(let* ((existent
							(mapcar (lambda (id) (list (string-trim-left id ".+-")))
											(remove nil
															(let ((org-trust-scanner-tags t))
																(org-map-entries
																 (lambda () (org-entry-get nil "ASANA_ID"))
																 (format "+ASANA_ID={.}+%s" (or query "")))))))
						 (gids
							(append existent (mapcar (lambda (task) (list (map-elt task 'gid)))
																			 tasks))))
				(asana-tasks-fetch-data gids #'asana-org-tasks-digest)))))

;;;###autoload
(defun asana-org-sync-task-at-point ()
  "Sync task at point with Asana by ASANA_ID property"
  (interactive)
	(let ((aid (org-entry-get nil "ASANA_ID")))
		(if (not aid)
				(message "No ASANA_ID property at current point")
			(with-asana-workspace (string-trim-right aid "-.+")
				(let* ((task-gid (string-trim-left aid ".+-"))
							 (props (asana-task-get task-gid))
							 (stories (asana-task-stories task-gid)))
					(org-narrow-to-subtree)
					(asana-org-task-sync props stories)
					(widen)
					(org-element-cache-reset)
					(org-indent-indent-buffer))))))


;;;###autoload
(defun asana-helm ()
  "Asana helm source.
Prompts for a workspace if none yet selected"
  (interactive)
	(unless (asana-workspace-gid)
		(call-interactively #'asana-workspace-change))
	(let ((sources
				 (list
					(asana-helm-resource "projects"
						:query `((workspace ,(asana-workspace-gid)))
						:action (helm-make-actions "Select Project"
																			 (lambda (c) (setq asana-helm-selected-project c)
																				 (asana-helm))))
					(asana-helm-resource "workspaces"
						:action (helm-make-actions "Select Workspace"
																			 (lambda (c) (setq asana-helm-selected-workspace c)
																				 (asana-helm)))))))
		(when-let ((project-gid (asana-helm-selected-project-gid)))
			(push
			 (asana-helm--task-make-source
				:name (format "Tasks in %s" (asana-helm-selected-project-name))
				:query `((project ,(asana-helm-selected-project-gid))))
			 sources))
		(helm :sources sources)))

;;;###autoload
(defun asana-helm-my-tasks ()
  "Asana helm source for \"My Tasks\".
Prompts for a workspace if none yet selected"
  (interactive)
	(unless (asana-workspace-gid)
		(call-interactively #'asana-workspace-change))
  (helm
	 :buffer (format "*helm-asana: My Tasks in %s*" (asana-workspace-name))
	 :sources (asana-helm--task-make-source
	 					 :name (format "My Tasks in %s" (asana-workspace-name))
	 					 :query `((workspace ,(asana-workspace-gid))
	 										(assignee "me")
	 										(completed_since "now")))))

(provide 'asana)

;;; asana.el ends here
