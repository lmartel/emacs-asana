;;; -*- lexical-binding: t -*-

(eval-when-compile
  (require 'cl-seq))
(require 'map)
(require 'json)
(require 'url)
(require 'url-http)
(require 'helm)

;; User config variables

(defcustom asana-keymap-prefix "C-c a"
  "Keymap prefix"
  :group 'asana
  :type 'string)

(defcustom asana-token (or (getenv "ASANA_TOKEN") "")
  "Token"
  :group 'asana
  :type 'string)

(defcustom asana-task-buffer-format :lisp
  "Format of task buffer"
  :group 'asana
  :type '(choice (const :lisp) (const :org)))

(defvar org-directory)
(defcustom asana-tasks-org-file (expand-file-name "Asana.org" org-directory)
  "Org file to sync with"
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

;; From https://github.com/bbatsov/projectile/blob/master/helm-projectile.el#L77
(defmacro asana-define-key (keymap key def &rest bindings)
  "In KEYMAP, define key sequence KEY1 as DEF1, KEY2 as DEF2 ..."
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
  `(lambda (_)
     (mapcar ,candidate-func (helm-marked-candidates))))

(defmacro asana-exec-marked (candidates-func)
  `(lambda (_)
     (funcall ,candidates-func (helm-marked-candidates))))

(defmacro asana-assocdr (key alist)
  `(cdr (assoc ,key ,alist)))

(defun asana-kbd (keyseq)
  (kbd (concat asana-keymap-prefix " " keyseq)))

(defun asana-compose (a b)
  (let ((a a)
        (b b))
    (lambda (&rest args)
      (funcall a (apply b args)))))

(defun asana-filter-later (tasks)
  (cl-remove-if (lambda (task) (equal (asana-assocdr 'assignee_status task) "later")) tasks))

(defun asana-fold-sections (tasks)
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
  (append `(("Authorization" . ,(concat "Bearer " asana-token))) extra-headers))

(defvar url-http-end-of-headers)
(defun asana-read-response (buf)
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
  (asana-read-response (current-buffer)))

;; API

(defun asana-get (resource &optional params callback) ; TODO: refactor HTTP funcs with macro
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
  (let ((url-request-method method)
        (url-request-extra-headers (asana-headers-with-auth))
        (url-request-data (json-encode `(("data" . ,params))))
        (url (concat asana-api-root resource)))
    (if callback
        (url-retrieve url (asana-compose callback 'asana-read-response-async))
      (asana-read-response (url-retrieve-synchronously url)))))

(defun asana-post (resource &optional params callback)
  (asana-request "POST" resource params callback))

(defun asana-put (resource &optional params callback)
  (asana-request "PUT" resource params callback))

(defun asana-delete (resource &optional params callback)
  (asana-request "DELETE" resource params callback))


(defun asana-get-workspaces (&optional callback)
  (asana-assocdr 'workspaces (asana-get "/users/me" nil callback)))

;; unused
(defun asana-get-sections (&optional callback)
  (asana-get (concat "/projects/" asana-my-tasks-project-id "/sections") `(("limit" . "100"))
             callback))

(defun asana-get-tasks (&optional callback)
  (asana-get "/tasks" `(("workspace" . ,(number-to-string (asana-assocdr 'id asana-selected-workspace)))
                        ("opt_fields" . "id,name,assignee_status")
                        ("limit" . "100")
                        ("assignee" . "me")
                        ("completed_since" . "now"))
             callback))

(defun asana-get-task (task-id &optional callback)
  (asana-get (concat "/tasks/" (number-to-string task-id)) nil callback))

(defun asana-get-task-stories (task-id &optional callback)
  (asana-get (concat "/tasks/" (number-to-string task-id) "/stories") nil callback))

;; Caching

(defun asana-clear-task-cache ()
  (setq asana-task-cache nil))

(defun asana-invalidate-task-cache ()
  (asana-get-tasks (lambda (tasks)
                     (setq asana-task-cache (mapcar 'asana-task-helm-data (asana-fold-sections (asana-filter-later tasks))))
                     (and helm-alive-p (helm-update))))
  (setq asana-section-cache nil))

;; Helm

(defun asana-task-helm-source ()
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
  `((name . "Sections")
    (candidates . ,(lambda () asana-section-cache))
    (volatile)
    (action . (("Select `RET'" . asana-section-select)))))

(defun asana-task-helm-data (task)
  `(,(asana-assocdr 'name task) . ,(asana-assocdr 'id task)))

(defun asana-section-helm-data (section)
  `(,(asana-assocdr 'name section) . ,section))

(defun asana-task-select (task-id)
  (let ((task-id task-id))
    (asana-get-task
	 task-id
	 (lambda (task)
	   (asana-display-task task (asana-get-task-stories task-id))))))

(defmacro asana-recode (&rest body)
  `(let ((pt (point)))
	 ,@body
	 (recode-region pt (point) 'utf-8-unix 'utf-8-unix)))

(defvar org-startup-folded)
(declare-function org-insert-heading-respect-content "org")
(defun asana-display-task (task stories)
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
  (insert "\xc\n;;; ===== TASK =====\n\n")
  (insert (pp task))
  (insert "\n\n\xc\n;;; ===== COMMENTS =====\n\n")
  (insert (pp stories)))

(defun asana-task-insert-as-org (task stories)
  (let ((closed (eql (map-elt task 'completed) :json-true))
		(has-schedule (map-elt task 'start_on))
		(has-deadline (map-elt task 'due_on))
		(liked (eql (map-elt task 'liked) :json-true))
		(hearted (eql (map-elt task 'hearted) :json-true)))
	(insert
	 (format
	  "%s%s\n"
	  (map-elt task 'name)
	  (if (map-elt task 'tags)
		  (format "%70s"
				  (seq-doseq (tag (map-elt task 'tags)) (concat ":" tag))) "")))
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
				   "[%Y-%m-%d %a]"
				   (date-to-time (concat (map-elt task 'start_on) " 00:00")))
				  (if has-deadline " "  "\n")) "")
	  (if has-deadline
		  (format "DEADLINE: %s\n"
				  (format-time-string
				   "[%Y-%m-%d %a]"
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
	(insert ":END:")))

(defun asana-task-browse (task-id)
  (browse-url (concat "https://app.asana.com/0/"
                      (number-to-string (asana-assocdr 'id asana-selected-workspace))
                      "/"
                      (number-to-string task-id))))

(defun asana-task-move-to-section (task-id)
  (asana-tasks-move-to-section (list task-id)))

(defun asana-tasks-move-to-section (task-ids)
  (setq asana-selected-task-ids task-ids)
  (helm :sources (asana-section-helm-source)
        :buffer "*helm-asana*")
  (setq asana-selected-task-ids nil))

(defun asana-tasks-map (callback)
  (asana-invalidate-task-cache)
  (let ((tasks (list)))
	(seq-doseq (task-id (map-values asana-task-cache))
	  (asana-get-task
	   task-id
	   (lambda (props)
		 (map-put tasks (map-elt props 'id)
				  `((props . ,props)
					(stories . ,(asana-get-task-stories (map-elt props 'id)))))
		 (when (eq (length tasks) (length asana-task-cache))
		   (funcall callback tasks)))))))

(defun asana-org-sync-tasks ()
  (interactive)
  (asana-tasks-map #'asana-tasks-org-digest))

(defun asana-tasks-org-digest (tasks)
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
  (dolist (task-id asana-selected-task-ids)
    (let ((task-sid (number-to-string task-id)))
      (asana-put (concat "/tasks/" task-sid)
                 `(("assignee_status" . ,(asana-assocdr 'assignee_status section))))
      (asana-post (concat "/tasks/" task-sid "/addProject")
                  `(("project" . ,asana-my-tasks-project-id)
                    ("section" . ,(asana-assocdr 'id section))) ; TODO in theory you can pass assignee_status here instead
                  (lambda (data)
                    (if data
                        (message "Unknown error: couldn't move task.")
                      (message "Task moved."))))))) ; TODO pass in task name

(defun asana-task-complete (task-id)
  (asana-put (concat "/tasks/" (number-to-string task-id))
             '(("completed" . t))
             (lambda (data)
               (let ((task-name (asana-assocdr 'name data)))
                 (if (assoc 'completed data)
                     (message "`%s' completed." task-name)
                   (message "Unknown error: couldn't complete `%s'" task-name))))))

(defun asana-task-delete (task-id)
  (asana-delete (concat "/tasks/" (number-to-string task-id))
                nil
                (lambda (data)
                  (if data
                      (message "Unknown error: couldn't delete task.")
                    (message "Task deleted.")))))

(defun asana-workspace-helm-source ()
  `((name . "Asana Workspaces")
    (candidates . ,(mapcar 'asana-workspace-helm-data (asana-get-workspaces)))
    (action . (("Select" . asana-workspace-select)))))

(defun asana-workspace-helm-data (workspace)
  `(,(asana-assocdr 'name workspace) . ,workspace))

(defun asana-workspace-select (workspace)
  (let* ((workspace-id (number-to-string (asana-assocdr 'id workspace)))
         (data (asana-get "/users/me" `(("workspace" . ,workspace-id)
                                        ("opt_fields" . "atm_id")))))
    (customize-save-variable 'asana-my-tasks-project-id (asana-assocdr 'atm_id data))
    (customize-save-variable 'asana-selected-workspace workspace)
    (helm-asana)))

;; Interactive

(define-minor-mode asana-mode
  "TODO docstring"
  nil
  " ⸫"
  `((,(asana-kbd "<return>") . helm-asana)
    (,(asana-kbd "a") . helm-asana)
    (,(asana-kbd "A") . helm-asana-change-workspace)
    (,(asana-kbd "c") . asana-create-task-quickly)
    (,(asana-kbd "C") . asana-create-task))
  :group 'asana)

(define-globalized-minor-mode global-asana-mode asana-mode asana-mode)

(defun asana-create-task (task-name &optional description)
  "TODO docstring"
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
  "TODO docstring"
  (interactive "sQuick-Create Asana Task: ")
  (asana-create-task task-name))

(defun helm-asana ()
  "TODO docstring"
  (interactive)
  (if asana-selected-workspace
      (progn (asana-invalidate-task-cache)
             (helm :sources (asana-task-helm-source)
                   :buffer "*helm-asana*"))
    (helm :sources (asana-workspace-helm-source)
          :buffer "*helm-asana*")))

(defun helm-asana-change-workspace ()
  "TODO docstring"
  (interactive)
  (customize-save-variable 'asana-selected-workspace nil)
  (asana-clear-task-cache)
  (helm-asana))

(provide 'asana)
