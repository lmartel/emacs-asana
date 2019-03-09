;;; asana.el --- Quickly create, view and bulk-update Asana tasks.   -*- lexical-binding: t -*-

;; Copyright (C) 2015-2018 Leo Martel <leo@lpm.io>

;; Author: Leo Martel <leo@lpm.io>
;;         Renato Ferreira <renatofdds@gmail.com>
;; Maintainer: Leo Martel <leo@lpm.io>
;; Version: 0.2.1
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

(defvar org-directory)
(defcustom asana-tasks-org-file (expand-file-name "asana.org" org-directory)
  "Org file to sync Asana tasks into."
  :group 'asana
  :type 'file)

;; Internal variables

(defconst asana-api-root "https://app.asana.com/api/1.0")

(defvar asana-my-tasks-project-id nil)
(defvar asana-selected-workspace nil)
(defvar asana-selected-section nil)
(defvar asana-selected-task-ids nil)
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

(defmacro asana-map-marked (candidate-func)
  "Map CANDIDATE-FUNC over the marked helm candidates."
  `(lambda (_)
     (mapcar ,candidate-func (helm-marked-candidates))))

(defmacro asana-exec-marked (candidates-func)
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

(defvar url-http-end-of-headers)
(defun asana-read-response (buf)
  "Read the raw Asana API response from BUF, surfacing errors if any and returning the data payload otherwise."
  (let* ((json-array-type 'list)
         (response (json-read-from-string (with-current-buffer buf
                                            (goto-char url-http-end-of-headers)
                                            (delete-region (point-min) (point))
                                            (buffer-string))))
         (errs (assoc 'errors response)))
    (if errs
        (error (concat "Asana API error: " (mapconcat (lambda (err) (asana-assocdr 'message err)) (cdr errs) "\n")))
      (asana-assocdr 'data response))))

(defun asana-read-response-async (_)
  "Read an Asana API response from the async results buffer."
  (asana-read-response (current-buffer)))

;; API

(defun asana-get (resource &optional params callback)
  "Send an HTTP GET to the Asana API for RESOURCE. Include PARAMS in the query string. If CALLBACK is provided, run asynchronously and call it on completion."
  (let ((url-request-method "GET")
        (url-request-extra-headers (asana-headers-with-auth))
        (url (concat asana-api-root
                     resource
                     "?"
                     (mapconcat (lambda (param)
                                  (concat (url-hexify-string (car param))
                                          "="
                                          (url-hexify-string (cdr param))))
                                params
                                "&"))))
    (if callback
        (url-retrieve url (asana-compose callback 'asana-read-response-async))
      (asana-read-response (url-retrieve-synchronously url)))))

(defun asana-request (method resource params callback)
  "Send a HTTP METHOD request to the Asana RESOURCE API. Include PARAMS as a JSON data blob. If CALLBACK is provided, run asynchronously and call it on completion."
  (let ((url-request-method method)
        (url-request-extra-headers (asana-headers-with-auth))
        (url-request-data (json-encode `(("data" . ,params))))
        (url (concat asana-api-root resource)))
    (if callback
        (url-retrieve url (asana-compose callback 'asana-read-response-async))
      (asana-read-response (url-retrieve-synchronously url)))))

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
  (asana-get (concat "/projects/" asana-my-tasks-project-id "/sections") `(("limit" . "100"))
             callback))

(defun asana-get-tasks (&optional callback)
  "Get all Asana tasks (limit 100) assigned-to-me in the current Workspace. If CALLBACK is provided, run asynchronously."
  (asana-get "/tasks" `(("workspace" . ,(number-to-string (asana-assocdr 'id asana-selected-workspace)))
                        ("opt_fields" . "id,name,assignee_status")
                        ("limit" . "100")
                        ("assignee" . "me")
                        ("completed_since" . "now"))
             callback))

(defun asana-get-task (task-id &optional callback)
  "Get an Asana task by TASK-ID. If CALLBACK is provided, run asynchronously."
  (asana-get (concat "/tasks/" (number-to-string task-id)) nil callback))

(defun asana-get-task-stories (task-id &optional callback)
  "Get the stories (comments, edit history, etc) for an Asana task by TASK-ID. If CALLBACK is provided, run asynchronously."
  (asana-get (concat "/tasks/" (number-to-string task-id) "/stories") nil callback))

;; Caching

(defun asana-clear-task-cache ()
  "Clear the Asana task cache."
  (setq asana-task-cache nil))

(defun asana-invalidate-task-cache ()
  "Clear and re-populate the Asana task cache."
  (asana-get-tasks (lambda (tasks)
                     (setq asana-task-cache (mapcar 'asana-task-helm-data (asana-fold-sections (asana-filter-later tasks))))
                     (and helm-alive-p (helm-update))))
  (setq asana-section-cache nil))

;; Helm

(defun asana-task-helm-source ()
  "Build a Helm source from My Asana Tasks in the current workspace."
  `((name . ,(concat "My Asana Tasks in " (asana-assocdr 'name asana-selected-workspace)))
    (candidates . ,(lambda () asana-task-cache))
    (volatile)
    (action . (("Select `RET'" . asana-task-select)
               ("Browse (open in Asana) `C-b'" . asana-task-browse)
               ("Move to section `C-:'" . asana-task-move-to-section)
               ("Complete `C-RET'" . asana-task-complete)
               ("Delete `C-DEL'" . asana-task-delete)
               ("Move marked Tasks `M-:'" . ,(asana-exec-marked 'asana-tasks-move-to-section))
               ("Complete marked Tasks `M-RET'" . ,(asana-map-marked 'asana-task-complete))
               ("Delete marked Tasks `M-DEL'" . ,(asana-map-marked 'asana-task-delete))))
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (set-keymap-parent map helm-map)
                 (asana-define-key map
                   (kbd "C-b") 'asana-task-browse
                   (kbd "C-:") 'asana-task-move-to-section
                   (kbd "<C-return>") 'asana-task-complete
                   (kbd "<C-backspace>") 'asana-task-delete
                   (kbd "M-:") (asana-exec-marked 'asana-tasks-move-to-section)
                   (kbd "<M-return>") (asana-map-marked 'asana-task-complete)
                   (kbd "<M-backspace>") (asana-map-marked 'asana-task-delete))
                 map))))

(defun asana-section-helm-source ()
  "Build a Helm source from the Sections of the current Asana project."
  `((name . "Sections")
    (candidates . ,(lambda () asana-section-cache))
    (volatile)
    (action . (("Select `RET'" . asana-section-select)))))

(defun asana-task-helm-data (task)
  "Build a (name . id) Helm data element from an Asana TASK."
  `(,(asana-assocdr 'name task) . ,(asana-assocdr 'id task)))

(defun asana-section-helm-data (section)
  "Build a (name . data) Helm data element from an Asana SECTION."
  `(,(asana-assocdr 'name section) . ,section))

(defun asana-task-select (task-id)
  "Select (display) a task by TASK-ID."
  (let ((task-id task-id))
    (asana-get-task
     task-id
     (lambda (task)
       (asana-display-task task (asana-get-task-stories task-id))))))

(defmacro asana-recode (&rest body)
  "Run BODY then recode the newly-added buffer content."
  `(let ((pt (point)))
     ,@body
     (recode-region pt (point) 'utf-8-unix 'utf-8-unix)))

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
       (asana-recode
        (asana-task-insert-as-lisp task stories))
       (emacs-lisp-mode))
      (:org
       (let ((org-startup-folded 'showeverything))
         (org-mode)
         (org-insert-heading-respect-content)
         (asana-recode
          (asana-task-insert-as-org task stories)))))
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
  (let ((closed (eql (map-elt task 'completed) :json-true))
        (has-schedule (map-elt task 'start_on))
        (has-deadline (map-elt task 'due_on))
		(tags (map-elt task 'tags))
        (notes (map-elt task 'notes))
        (liked (eql (map-elt task 'liked) :json-true))
        (hearted (eql (map-elt task 'hearted) :json-true)))
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
      (map-nested-elt task '(workspace id))
      (map-elt task 'id)))
    (insert
     (format
      ":ASANA_URL: [[https://app.asana.com/0/%s/%s]]\n"
      (map-nested-elt task '(workspace id))
      (map-elt task 'id)))
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
				  "[\n]" t "[\n ]*"))
		(unless (string-prefix-p "-" p)
		  (newline))
		(insert p)
		(call-interactively #'org-fill-paragraph)
		(newline 2)
		(delete-blank-lines)))
	(newline 2)
	(delete-blank-lines)))

(defun asana-task-browse (task-id)
  "Browse to an Asana task by TASK-ID using `browse-url'."
  (browse-url (concat "https://app.asana.com/0/"
                      (number-to-string (asana-assocdr 'id asana-selected-workspace))
                      "/"
                      (number-to-string task-id))))

(defun asana-task-move-to-section (task-id)
  "Move one task to a section by TASK-ID."
  (asana-tasks-move-to-section (list task-id)))

(defun asana-tasks-move-to-section (task-ids)
  "Prompt to select a section, then move a list of Asana tasks to it by their TASK-IDS."
  (setq asana-selected-task-ids task-ids)
  (helm :sources (asana-section-helm-source)
        :buffer "*asana-helm*")
  (setq asana-selected-task-ids nil))

(defun asana-tasks-fetch-data (tasks callback)
  "Fetch tasks data and call CALLBACK."
  (let* ((fetched 0)
		 (to-fetch (* 2 (length tasks)))
		 (reporter (make-progress-reporter "Fetching tasks..." 0 to-fetch)))
	(cl-flet ((check-done
			   nil
			   (progress-reporter-update reporter (setq fetched (1+ fetched)))
			   (when (>= fetched to-fetch)
				 (progress-reporter-done reporter)
				 (funcall callback tasks))))
	  (seq-doseq (task-id (map-keys tasks))
		(asana-get-task
		 task-id
		 (lambda (props)
		   (map-put (map-elt tasks task-id) 'props props)
		   (check-done)))
		(asana-get-task-stories
		 task-id
		 (lambda (stories)
		   (map-put (map-elt tasks task-id) 'stories stories)
		   (check-done)))))))

(defun asana-org-sync-tasks ()
  "One-way-sync all available Asana tasks to `asana-tasks-org-file'. Update previously downloaded tasks in-place; append newly discovered tasks. Slow for large projects!"
  (interactive)
  (message "Fetching tasks...")
  (asana-get-tasks
   (lambda (tasks)
	 (asana-tasks-fetch-data
	  (seq-group-by (lambda (task) (map-elt task 'id))
					(asana-fold-sections (asana-filter-later tasks)))
	  #'asana-tasks-org-digest))))

(defun asana-tasks-org-digest (tasks)
  "Dump TASKS into an org buffer backed by `asana-tasks-org-file'."
  (eval-and-compile
    (require 'org)
    (require 'org-indent))
  (switch-to-buffer (find-file asana-tasks-org-file))
  (seq-doseq (task tasks)
    (asana-task-org-sync (map-elt task 'props) (map-elt task 'stories)))
  (org-indent-indent-buffer)
  (goto-char (point-min))
  (org-sort-entries nil ?c)
  (org-global-cycle '(4)))

(defun asana-task-org-sync (task stories)
  "Write one TASK and its STORIES to the current buffer in org format."
  (save-excursion
    (let* ((existing
            (org-find-property
             "ASANA_ID"
             (format "%s-%s"
                     (map-nested-elt task '(workspace id))
                     (map-elt task 'id))))
           id todo-state)
      (if existing
          (progn
            (goto-char existing)
            (setq id (org-id-get-create))
            (setq todo-state (org-get-todo-state))
            (org-cut-special))
        (goto-char (point-max)))
      (asana-recode
       (org-insert-heading-respect-content)
       (asana-task-insert-as-org task stories))
      (org-back-to-heading)
      (org-todo (or todo-state 'nextset))
      (when id (org-entry-put (point) "ID" id)))))

(defun asana-section-select (section)
  "Select SECTION, moving the previously selected tasks to it."
  (dolist (task-id asana-selected-task-ids)
    (let ((task-sid (number-to-string task-id)))
      (asana-put (concat "/tasks/" task-sid)
                 `(("assignee_status" . ,(asana-assocdr 'assignee_status section))))
      (asana-post (concat "/tasks/" task-sid "/addProject")
                  `(("project" . ,asana-my-tasks-project-id)
                    ("section" . ,(asana-assocdr 'id section))) ; TODO in theory you can pass assignee_status here instead, but Asana API bug prevents this.
                  (lambda (data)
                    (let ((task-name (asana-assocdr 'name data)))
                      (if data
                          (message "Unknown error: couldn't move `%s'." task-name)
                        (message "`%s' moved." task-name))))))))

(defun asana-task-complete (task-id)
  "Complete an Asana task by TASK-ID."
  (asana-put (concat "/tasks/" (number-to-string task-id))
             '(("completed" . t))
             (lambda (data)
               (let ((task-name (asana-assocdr 'name data)))
                 (if (assoc 'completed data)
                     (message "`%s' completed." task-name)
                   (message "Unknown error: couldn't complete `%s'" task-name))))))

(defun asana-task-delete (task-id)
  "Delete an Asana task by TASK-ID."
  (asana-delete (concat "/tasks/" (number-to-string task-id))
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
  "Select WORKSPACE by id and save the selection with customize."
  (let* ((workspace-id (number-to-string (asana-assocdr 'id workspace)))
         (data (asana-get "/users/me" `(("workspace" . ,workspace-id)
                                        ("opt_fields" . "atm_id")))))
    (customize-save-variable 'asana-my-tasks-project-id (asana-assocdr 'atm_id data))
    (customize-save-variable 'asana-selected-workspace workspace)
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
                         ("workspace" . ,(number-to-string (asana-assocdr 'id asana-selected-workspace))))
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
  (if asana-selected-workspace
      (progn (asana-invalidate-task-cache)
             (helm :sources (asana-task-helm-source)
                   :buffer "*asana-helm*"))
    (helm :sources (asana-workspace-helm-source)
          :buffer "*asana-helm*")))

(defun asana-helm-change-workspace ()
  "Change the active workspace used by Asana commands. Alternatively, customize the `asana-selected-workspace' variable."
  (interactive)
  (customize-save-variable 'asana-selected-workspace nil)
  (asana-clear-task-cache)
  (asana-helm))

(provide 'asana)
;;; asana.el ends here
