;;; asana.el --- Quickly create, view and bulk-update Asana tasks.   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Leo Martel <leo@lpm.io>

;; Author: Leo Martel <leo@lpm.io>
;;         Renato Ferreira <renatofdds@gmail.com>
;; Maintainer: Leo Martel <leo@lpm.io>
;; Version: 0.3.0
;; Keywords: comm outlines tools
;; Homepage: https://github.com/lmartel/emacs-asana
;; Package-Requires: ((emacs "25.1") (helm "1.9"))

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
;; - Create tasks from the minibuffer
;; - Dump your tasks to org-mode TODOs
;; - List and act on your tasks in bulk with `helm' extensions
;;
;; The minor mode `asana-mode' provides a keymap, or just call the interactive `asana-*'
;; functions directly with M-x.

;;; Code:

(eval-when-compile
  (require 'cl-seq))
(require 'helm)
(require 'json)
(require 'map)
(require 'url)
(require 'url-http)
(require 'url-queue)

;; User config variables

(defcustom asana-keymap-prefix "C-c a"
  "Keymap prefix for asana commands."
  :group 'asana
  :type 'string)

(defvar asana-token-env-var "ASANA_TOKEN"
  "Name of environment variable to check for an Asana personal access token. If nil or not found, fall back to `asana-token'.")
(defvar asana-token nil
  "If non-nil, use this instead of checking the environment variable `asana-token-env-var'.")

(defcustom asana-task-buffer-format :org
  "Default format of Asana task buffer."
  :group 'asana
  :type '(choice (const :lisp) (const :org)))

(defcustom asana-api-concurrent-requests 45
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

(defvar org-directory)
(defcustom asana-tasks-org-file
  (expand-file-name "asana.org" (or (and (boundp 'org-directory) org-directory)
																		user-emacs-directory))
  "Org file to sync Asana tasks into."
  :group 'asana
  :type 'file)

;; Internal variables

(defconst asana-api-root "https://app.asana.com/api/1.0")

(defvar asana-my-user-task-list-gid nil)
(defvar asana-selected-workspace-gid nil)
(defvar asana-selected-workspace-name nil)
(defvar asana-selected-section nil)
(defvar asana-selected-task-gids nil)
(defvar asana-task-cache nil)
(defvar asana-section-cache nil)

;; Helpers

(defun asana-get-token ()
  "Get the user's specified Asana access token."
  (or asana-token (getenv asana-token-env-var)))

;; From https://github.com/bbatsov/projectile/blob/master/helm-projectile.el#L77
(defmacro asana-define-key (keymap key def &rest bindings)
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

(defmacro asana-helm-map-marked (candidate-func)
  "Map CANDIDATE-FUNC over the marked helm candidates."
  `(lambda (_)
     (mapcar ,candidate-func (helm-marked-candidates))))

(defmacro asana-helm-exec-marked (candidates-func)
  "Call CANDIDATES-FUNC and on the list of marked helm candidates."
  `(lambda (_)
     (funcall ,candidates-func (helm-marked-candidates))))

(defmacro asana-assocdr (key alist)
  "Pull the value for KEY out of ALIST."
  `(cdr (assoc ,key ,alist)))

(defun asana-kbd (keyseq)
  "Create a prefixed kbd for KEYSEQ."
  (kbd (concat asana-keymap-prefix " " keyseq)))

(defun asana-compose (a b)
  "Compose functions A and B."
  (lambda (&rest args)
    (funcall a (apply b args))))

(defun asana-filter-later (tasks)
  "Filter out tasks marked as later from a list of TASKS."
  (cl-remove-if (lambda (task) (equal (asana-assocdr 'assignee_status task) "later")) tasks))

(defun asana-fold-sections (tasks)
  "Remove section placeholders from a list of TASKS and prefix task names with [SECTION] instead."
  (let ((prev-section (asana-assocdr 'name asana-selected-section)))
    (setq asana-selected-section nil)
    (let ((tasks-with-sections
           (mapcar
            (lambda (task)
              (let ((task-name (asana-assocdr 'name task)))
                (cond ((string-suffix-p ":" task-name)
                       (setq asana-selected-section task)
                       (add-to-list 'asana-section-cache (asana-section-helm-data task))
                       nil)
                      (asana-selected-section
                       (cons `(name . ,(concat "[" (asana-assocdr 'name asana-selected-section) "] " task-name)) task))
                      (t
                       task))))
            tasks)))
      (setq asana-selected-section prev-section)
      (cl-remove-if 'null tasks-with-sections))))

(defun asana-headers-with-auth (&optional extra-headers)
  "Create an HTTP headers list from the configured Asana token, appending EXTRA-HEADERS if any."
  (append `(("Authorization" . ,(concat "Bearer " (asana-get-token)))) extra-headers))

(define-error 'asana-api-error "Asana API error")

(defvar url-http-end-of-headers)
(defun asana-read-response (buf)
  "Read the raw Asana API response from BUF, surfacing errors if any and returning the data payload otherwise."
  (let* ((json-array-type 'list)
				 (response
					(json-read-from-string
					 (with-current-buffer buf
             (set-buffer-multibyte t)
						 (goto-char url-http-end-of-headers)
						 (delete-region (point-min) (point))
						 (buffer-string))))
				 (errors (map-elt response 'errors)))
		(when errors
			(let (messages)
				(seq-doseq (err (cdr errors)) (push (map-elt err 'message) messages))
				(signal 'asana-api-error messages)))
		(map-elt response 'data)))

(defvar asana-get-async-error-handler (lambda (_error-data)))
(defmacro asana-url-retrieve-callback (&rest body)
  (declare (debug def-body))
  `(let ((error-handler asana-get-async-error-handler))
		 (lambda (url-retrieve-status &rest _)
			 (let ((http-error (plist-get url-retrieve-status :error)))
				 (if (not http-error)
						 (condition-case callback-error
								 ,(macroexp-progn body)
							 (error
								(message "Asana API error: %S" callback-error)
								(funcall error-handler (cdr callback-error))))
					 (message "Asana HTTP error: %s" http-error)
					 (funcall error-handler http-error))))))

;; API

(defvar url-request-extra-headers)
(defvar url-request-method)
(defvar url-request-data)
(defvar url-queue-parallel-processes)
(defvar url-queue-timeout)
(defvar asana-get-async-queue nil)
(defvar asana-get-async-queue-start-time nil)
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

(defun asana-get-async (url &optional callback)
  "Internal asynchronous API fetcher"
  (url-queue-retrieve
	 url
	 (asana-url-retrieve-callback
		(funcall callback (asana-read-response (current-buffer))))))

(defun asana-get-sync (url)
  "Internal synchronous API fetcher"
  (save-excursion
		(let ((url-request-extra-headers (asana-headers-with-auth)))
			(asana-read-response (url-retrieve-synchronously url nil nil asana-api-timeout)))))

(defun asana-get (resource &optional params callback)
  "Send an HTTP GET to the Asana API for RESOURCE with PARAMS as the query string.
If CALLBACK is provided, it is called after completion as (funcall CALLBACK DATA).
DATA is a list parsed from the JSON API response."
  (let ((url (concat asana-api-root resource "?"
                     (mapconcat
											(lambda (param) (concat (car param) "=" (cdr param)))
											params
											"&"))))
		(if callback
				(asana-get-async url callback)
			(asana-get-sync url))))

(defun asana-request (method resource params callback)
  "Send a HTTP METHOD request to the Asana RESOURCE API. Include PARAMS as a JSON data blob. If CALLBACK is provided, run asynchronously and call it on completion."
  (let ((url-request-method method)
        (url-request-extra-headers (asana-headers-with-auth))
        (url-request-data (json-encode `(("data" . ,params))))
        (url (concat asana-api-root resource)))
    (if callback
        (url-queue-retrieve
				 url
				 (asana-url-retrieve-callback
					(lambda (&rest _) (funcall callback (asana-read-response (current-buffer))))))
      (asana-read-response (url-retrieve-synchronously url nil nil asana-api-timeout)))))

(defun asana-post (resource &optional params callback)
  "Send a HTTP POST request to the Asana RESOURCE API. Include PARAMS as a JSON data blob. If CALLBACK is provided, run asynchronously."
  (asana-request "POST" resource params callback))

(defun asana-put (resource &optional params callback)
  "Send a HTTP PUT request to the Asana RESOURCE API. Include PARAMS as a JSON data blob. If CALLBACK is provided, run asynchronously."
  (asana-request "PUT" resource params callback))

(defun asana-delete (resource &optional params callback)
  "Send a HTTP DELETE request to the Asana RESOURCE API. Include PARAMS as a JSON data blob. If CALLBACK is provided, run asynchronously."
  (asana-request "DELETE" resource params callback))

(defun asana-get-workspaces (&optional callback)
  "Get all Asana workspaces. If CALLBACK is provided, run asynchronously."
  (asana-assocdr 'workspaces (asana-get "/users/me" nil callback)))

;; unused
(defun asana-get-sections (&optional callback)
  "Get all Asana sections within the current project. If CALLBACK is provided, run asynchronously."
  (asana-get (concat "/projects/" asana-my-user-task-list-gid "/sections")
             callback))

(defun asana-get-my-open-tasks (&optional callback)
  "Get all Asana tasks assigned-to-me in the current Workspace. If CALLBACK is provided, run asynchronously."
  (asana-get "/tasks" `(("workspace" . ,asana-selected-workspace-gid)
                        ("opt_fields" . "gid,name,assignee_status")
                        ("assignee" . "me")
                        ("completed_since" . "now"))
             callback))

(defun asana-get-task (task-gid &optional callback)
  "Get an Asana task by TASK-GID. If CALLBACK is provided, run asynchronously."
  (asana-get (concat "/tasks/" task-gid) nil callback))

(defun asana-get-task-stories (task-gid &optional callback)
  "Get the stories (comments, edit history, etc) for an Asana task by TASK-GID. If CALLBACK is provided, run asynchronously."
  (asana-get (concat "/tasks/" task-gid "/stories") nil callback))

;; Caching

(defun asana-clear-task-cache ()
  "Clear the Asana task cache."
  (setq asana-task-cache nil))

(defun asana-invalidate-task-cache ()
  "Clear and re-populate the Asana task cache."
	(setq asana-section-cache nil)
  (asana-get-my-open-tasks
	 (lambda (tasks)
     (setq asana-task-cache (mapcar 'asana-task-helm-data (asana-fold-sections (asana-filter-later tasks))))
     (and helm-alive-p (helm-update)))))

;; Helm

(defun asana-task-helm-source ()
  "Build a Helm source from My Asana Tasks in the current workspace."
  `((name . ,(concat "My Asana Tasks in " asana-selected-workspace-name))
    (candidates . ,(lambda () asana-task-cache))
    (volatile)
    (action . (("Select `RET'" . asana-task-get-and-display)
               ("Browse (open in Asana) `C-b'" . asana-task-browse)
               ("Move to section `C-:'" . asana-task-move-to-section)
               ("Complete `C-RET'" . asana-task-complete)
               ("Delete `C-DEL'" . asana-task-delete)
               ("Move marked Tasks `M-:'" . ,(asana-helm-exec-marked 'asana-tasks-move-to-section))
               ("Complete marked Tasks `M-RET'" . ,(asana-helm-map-marked 'asana-task-complete))
               ("Delete marked Tasks `M-DEL'" . ,(asana-helm-map-marked 'asana-task-delete))))
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (set-keymap-parent map helm-map)
                 (asana-define-key map
                   (kbd "C-b") 'asana-task-browse
                   (kbd "C-:") 'asana-task-move-to-section
                   (kbd "<C-return>") 'asana-task-complete
                   (kbd "<C-backspace>") 'asana-task-delete
                   (kbd "M-:") (asana-helm-exec-marked 'asana-tasks-move-to-section)
                   (kbd "<M-return>") (asana-helm-map-marked 'asana-task-complete)
                   (kbd "<M-backspace>") (asana-helm-map-marked 'asana-task-delete))
                 map))))

(defun asana-section-helm-source ()
  "Build a Helm source from the Sections of the current Asana project."
  `((name . "Sections")
    (candidates . ,(lambda () asana-section-cache))
    (volatile)
    (action . (("Select `RET'" . asana-section-select)))))

(defun asana-task-helm-data (task)
  "Build a (name . gid) Helm data element from an Asana TASK."
  `(,(asana-assocdr 'name task) . ,(asana-assocdr 'gid task)))

(defun asana-section-helm-data (section)
  "Build a (name . data) Helm data element from an Asana SECTION."
  `(,(asana-assocdr 'name section) . ,section))

(defun asana-task-get-and-display (task-gid)
  "Fetch task and stories by TASK-ID then display it."
  (asana-display-task (asana-get-task task-gid) (asana-get-task-stories task-gid)))

(defvar org-startup-folded)
(declare-function org-insert-heading-respect-content "org")
(defun asana-display-task (task stories)
  "Display the selected TASK and its STORIES in a special buffer, in the format specified by `asana-task-buffer-format'."
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
       (asana-task-insert-as-lisp task stories)
       (emacs-lisp-mode))
      (:org
       (let ((org-startup-folded 'showeverything))
         (org-mode)
         (org-insert-heading-respect-content)
         (asana-task-insert-as-org task stories))))
    (goto-char pt)
    (view-mode)))

(defun asana-task-insert-as-lisp (task stories)
  "Insert a lisp-formatted Asana TASK with STORIES into the current buffer."
  (insert "\xc\n;;; ===== TASK =====\n\n")
  (insert (pp task))
  (insert "\n\n\xc\n;;; ===== COMMENTS =====\n\n")
  (insert (pp stories)))

(defun asana-task-insert-as-org (task stories)
  "Insert an org-formatted Asana TASK with STORIES into the current buffer."
  (eval-and-compile
		(require 'org))
  (let ((closed (eq (map-elt task 'completed) t))
        (has-schedule (map-elt task 'start_on))
        (has-deadline (map-elt task 'due_on))
				(tags (map-elt task 'tags))
        (notes (map-elt task 'notes))
        (liked (eq (map-elt task 'liked) t))
        (hearted (eq (map-elt task 'hearted) t)))
    (insert
     (format
      "%s%s\n"
      (map-elt task 'name)
      (if tags
          (format
					 "   %s:"
					 (apply 'concat
									(seq-map (lambda (tag) (concat ":" (map-elt tag 'name))) tags)))
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
     (format
      ":ASANA_ID: %s-%s\n"
      (map-nested-elt task '(workspace gid))
      (map-elt task 'gid)))
    (insert
     (format
      ":ASANA_URL: [[https://app.asana.com/0/%s/%s]]\n"
      (map-nested-elt task '(workspace gid))
      (map-elt task 'gid)))
    (insert (format ":WORKSPACE: %s\n" (map-nested-elt task '(workspace name))))
    (insert (format ":ASSIGNEE: %s\n" (map-nested-elt task '(assignee name))))
    (insert (format ":ASSIGNEE_STATUS: %s\n" (map-elt task 'assignee_status)))
    (when hearted (insert (format ":HEARTS: %d\n" (map-elt task 'num_hearts))))
    (when liked (insert (format ":LIKES: %d\n" (map-elt task 'num_likes))))
    (insert ":PROJECTS: ")
    (seq-map (lambda (p)
               (insert (format "%s, "(map-elt p 'name))))
             (map-elt task 'projects))
    (insert "\n")
    (insert ":MEMBERSHIPS: ")
    (seq-doseq (m (map-elt task 'memberships))
      (insert (format "%s, "(map-nested-elt m '(project name)))))
    (insert "\n")
    (insert ":FOLLOWERS: ")
    (seq-doseq (f (map-elt task 'followers))
      (insert (format "%s, " (map-elt f 'name))))
    (insert "\n")
    (seq-doseq (f (map-elt task 'custom_fields))
      (when (map-elt f 'enabled)
        (insert (format ":CUSTOM_%s: %s\n"
                        (replace-regexp-in-string
                         " " "_" (upcase (substring (map-elt f 'name) 1)))
                        (map-nested-elt f '(enum_value name))))))
    (insert ":END:\n")
    (insert ":LOGBOOK:\n")
    (seq-doseq (entry (reverse stories))
      (insert
       (format
        "- %s (%s)\n"
        (format-time-string "[%Y-%m-%d %a %H:%M]" (date-to-time (map-elt entry 'created_at)))
        (map-elt entry 'type)))
      (let ((p (point)) (fill-prefix "  "))
        (insert
         (format "  %s: %s"
                 (map-nested-elt entry '(created_by name))
                 (replace-regexp-in-string "\n" "\n  " (map-elt entry 'text))))
        (fill-region p (point) nil nil nil))
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

(defun asana-task-browse (task-gid)
  "Browse to an Asana task by TASK-ID using `browse-url'."
  (browse-url (concat "https://app.asana.com/0/"
                      asana-selected-workspace-gid
                      "/"
                      task-gid)))

(defun asana-task-move-to-section (task-gid)
  "Move one task to a section by TASK-ID."
  (asana-tasks-move-to-section (list task-gid)))

(defun asana-tasks-move-to-section (task-gids)
  "Prompt to select a section, then move a list of Asana tasks to it by their TASK-IDS."
  (setq asana-selected-task-gids task-gids)
  (helm :sources (asana-section-helm-source)
        :buffer "*asana-helm*")
  (setq asana-selected-task-gids nil))

(defun asana-tasks-fetch-data (tasks callback)
  "Fetch tasks data and call CALLBACK."
  (let* ((tasks (map-into tasks 'hash-table))
				 (fetched 0)
				 (to-fetch (* 2 (map-length tasks)))
				 (reporter (make-progress-reporter "Fetching tasks..." 0 to-fetch)))
		(cl-flet ((check-done
							 nil
							 (cl-incf fetched)
							 (progress-reporter-update reporter fetched)
							 (when (= fetched to-fetch)
								 (progress-reporter-done reporter)
								 (funcall callback (map-into tasks 'list)))))
			(seq-doseq (task-gid (map-keys tasks))
				(let ((asana-get-async-error-handler
							 (lambda (_)
								 (map-delete tasks task-gid)
								 (check-done))))
					(asana-get-task
					 task-gid
					 (lambda (props)
						 (when (map-contains-key tasks task-gid)
							 (map-put (map-elt tasks task-gid) 'props props))
						 (check-done)))
					(asana-get-task-stories
					 task-gid
					 (lambda (stories)
						 (when (map-contains-key tasks task-gid)
							 (map-put (map-elt tasks task-gid) 'stories stories))
						 (check-done))))))))

(defun asana-org-sync-tasks (&optional query)
  "One-way-sync all user own open tasks to `asana-tasks-org-file'.
Update previously downloaded tasks in-place according to org tags search QUERY;
Append newly discovered tasks.

Slow for large projects!"
  (interactive)
	(eval-and-compile
		(require 'org)
		(require 'org-indent))
  (message "Fetching tasks...")
  (asana-get-my-open-tasks
   (lambda (tasks)
		 (switch-to-buffer (find-file asana-tasks-org-file))
		 (let* ((existent
						 (mapcar (lambda (id) (list (string-trim-left id ".+-")))
										 (remove nil (org-map-entries
																	(lambda () (org-entry-get nil "ASANA_ID"))
																	(format "+ASANA_ID={.}+%s" (or query ""))))))
						(gids
						 (append existent (mapcar (lambda (task) (list (map-elt task 'gid)))
																			(asana-fold-sections tasks)))))
			 (asana-tasks-fetch-data gids #'asana-tasks-org-digest)))))

(defun asana-org-sync-task-at-point ()
  "Sync task at point with Asana by ASANA_ID property"
  (interactive)
	(eval-and-compile
		(require 'org)
		(require 'org-indent))
	(let ((aid (org-entry-get nil "ASANA_ID")))
		(if (not aid)
				(message "No ASANA_ID property at current point")
			(let* ((task-gid (string-trim-left aid ".+-"))
						 (asana-selected-workspace-gid (string-trim-right aid "-.+"))
						 (props (asana-get-task task-gid))
						 (stories (asana-get-task-stories task-gid)))
				(org-narrow-to-subtree)
				(asana-task-org-sync props stories)
				(widen)
				(org-element-cache-reset)
				(org-indent-indent-buffer)))))

(defun asana-tasks-org-digest (tasks)
  "Dump TASKS into an org buffer backed by `asana-tasks-org-file'."
  (eval-and-compile
		(require 'org)
		(require 'org-indent))
  (switch-to-buffer (find-file asana-tasks-org-file))
	(goto-char (point-min))
  (unless (re-search-forward "^* Asana" nil t)
		(goto-char (point-max))
		(newline 2)
		(insert "* Asana"))
  (seq-doseq (task tasks)
    (asana-task-org-sync (map-elt task 'props) (map-elt task 'stories)))
  (org-element-cache-reset)
  (org-indent-indent-buffer)
  (org-sort-entries nil ?r nil nil "CREATED_AT")
  (org-content 2))

(defvar org-log-done)
(defun asana-task-org-sync (task stories)
  "Write one TASK and its STORIES to the current buffer in org format."
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
						 (org-cut-special)
						 (end-of-line))
						(t (goto-char (point-max))
							 (org-insert-heading-respect-content)))
			(asana-task-insert-as-org task stories)
      (org-back-to-heading)
			(if (eq (map-elt task 'completed) t)
					(let (org-log-done)
						(org-todo (or (car-safe (member todo-state org-done-keywords))
													'done)))
				(org-todo (or (car-safe (member todo-state org-not-done-keywords))
											'nextset)))
      (when id (org-entry-put (point) "ID" id)))))

(defun asana-section-select (section)
  "Select SECTION, moving the previously selected tasks to it."
  (dolist (task-gid asana-selected-task-gids)
    (asana-put (concat "/tasks/" task-gid)
							 `(("assignee_status" . ,(asana-assocdr 'assignee_status section))))
		(asana-post (concat "/tasks/" task-gid "/addProject")
								`(("project" . ,asana-my-user-task-list-gid)
									("section" . ,(asana-assocdr 'gid section))) ; TODO in theory you can pass assignee_status here instead, but Asana API bug prevents this.
								(lambda (data)
									(let ((task-name (asana-assocdr 'name data)))
										(if data
												(message "Unknown error: couldn't move `%s'." task-name)
											(message "`%s' moved." task-name)))))))

(defun asana-task-complete (task-gid)
  "Complete an Asana task by TASK-ID."
  (asana-put (concat "/tasks/" task-gid)
             '(("completed" . t))
             (lambda (data)
               (let ((task-name (asana-assocdr 'name data)))
                 (if (assoc 'completed data)
                     (message "`%s' completed." task-name)
                   (message "Unknown error: couldn't complete `%s'" task-name))))))

(defun asana-task-delete (task-gid)
  "Delete an Asana task by TASK-ID."
  (asana-delete (concat "/tasks/" task-gid)
                nil
                (lambda (data)
                  (if data
                      (message "Unknown error: couldn't delete task.")
                    (message "Task deleted.")))))

(defun asana-workspace-helm-source ()
  "Build a Helm source to select an Asana workspace."
  `((name . "Asana Workspaces")
    (candidates . ,(mapcar 'asana-workspace-helm-data (asana-get-workspaces)))
    (action . (("Select" . asana-workspace-select)))))

(defun asana-workspace-helm-data (workspace)
  "Build a (name . data) helm data element from WORKSPACE."
  `(,(asana-assocdr 'name workspace) . ,workspace))

(defun asana-workspace-select (workspace)
  "Select WORKSPACE by gid and save the selection with customize."
  (let* ((data (asana-get "/users/me/user_task_list" `(("workspace" . ,(asana-assocdr 'gid workspace))))))
    (customize-save-variable 'asana-my-user-task-list-gid (asana-assocdr 'gid data))
    (customize-save-variable 'asana-selected-workspace-gid (asana-assocdr 'gid workspace))
		(customize-save-variable 'asana-selected-workspace-name (asana-assocdr 'name workspace))
    (asana-helm)))

;; Interactive

(define-minor-mode asana-mode
  "Minor mode providing key bindings for asana-* comnmands."
  nil
  " ⸫"
  `((,(asana-kbd "<return>") . asana-helm)
    (,(asana-kbd "a") . asana-helm)
    (,(asana-kbd "A") . asana-helm-change-workspace)
    (,(asana-kbd "c") . asana-create-task-quickly)
    (,(asana-kbd "C") . asana-create-task))
  :group 'asana)

(define-globalized-minor-mode global-asana-mode asana-mode asana-mode
  :require 'asana)

(defun asana-create-task (task-name &optional description)
  "Create task TASK-NAME with optional DESCRIPTION. If called interactively, ask for both."
  (interactive "sCreate Asana Task: \nsTask Description: ")
  (asana-post "/tasks" `(("name" . ,task-name)
                         ("notes" . ,(or description ""))
                         ("assignee" . "me")
                         ("workspace" . asana-selected-workspace-gid))
              (lambda (data)
                (let ((task-name (asana-assocdr 'name data)))
                  (if task-name
                      (message "Created task: `%s'." task-name)
                    (message "Unknown error: couldn't create task."))))))

(defun asana-create-task-quickly (task-name)
  "Create a task TASK-NAME with no description."
  (interactive "sQuick-Create Asana Task: ")
  (asana-create-task task-name))

(defun asana-helm ()
  "Entrypoint for the Asana helm source. Select a workspace if none yet selected, then load the My Tasks list."
  (interactive)
  (if asana-selected-workspace-gid
      (progn (asana-invalidate-task-cache)
             (helm :sources (asana-task-helm-source)
                   :buffer "*asana-helm*"))
    (helm :sources (asana-workspace-helm-source)
          :buffer "*asana-helm*")))

(defun asana-helm-change-workspace ()
  "Change the active workspace used by Asana commands. Alternatively, customize the `asana-selected-workspace' variable."
  (interactive)
  (customize-save-variable 'asana-selected-workspace-gid nil)
  (customize-save-variable 'asana-selected-workspace-name nil)
  (asana-clear-task-cache)
  (asana-helm))

(provide 'asana)
;;; asana.el ends here
