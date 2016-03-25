(require 'json)
(require 'url)
(require 'url-http)

;; User config variables

(defvar asana-keymap-prefix "C-c a")
(exec-path-from-shell-copy-env "ASANA_TOKEN")
(exec-path-from-shell-copy-env "ASANA_ATM_ID")

;; Internal variables

(defconst asana-api-root "https://app.asana.com/api/1.0")
(defconst asana-token (getenv "ASANA_TOKEN"))
(defconst asana-my-tasks-project-id (getenv "ASANA_ATM_ID")) ; TODO way to get this thru API?

(defvar asana-selected-workspace-id nil)
(defvar asana-selected-workspace-name nil)
(defvar asana-selected-section-name nil)
(defvar asana-selected-task-id nil)
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

(defmacro asana-exec-marked (candidate-func)
  `(lambda (unused-selection)
     (mapcar ,candidate-func (helm-marked-candidates))))

(defun asana-kbd (keyseq)
  (kbd (concat asana-keymap-prefix " " keyseq)))

(defun asana-compose (a b)
  (lexical-let ((a a)
                (b b))
    (lambda (&rest args)
      (funcall a (apply b args)))))

(defun asana-filter-later (tasks)
  (remove-if (lambda (task) (equal (cdr (assoc 'assignee_status task)) "later")) tasks))

(defun asana-fold-sections (tasks)
  (let ((prev-section asana-selected-section-name))
    (setq asana-selected-section-name nil)
    (let ((tasks-with-sections (mapcar (lambda (task)
                                         (let ((task-name (cdr (assoc 'name task))))
                                           (cond ((string-suffix-p ":" task-name)
                                                  (setq asana-selected-section-name task-name)
                                                  nil)
                                                 (asana-selected-section-name
                                                  (cons `(name . ,(concat "[" asana-selected-section-name "] " task-name)) task))
                                                 (t
                                                  task))))
                                       tasks)))
      (setq asana-selected-section-name prev-section)
      (remove-if 'null tasks-with-sections)))) ; filter nil

(defun asana-headers-with-auth (&optional extra-headers)
  (append `(("Authorization" . ,(concat "Bearer " asana-token))) extra-headers))

(defun asana-read-response (buf)
  (let* ((json-array-type 'list)
         (response (json-read-from-string (with-current-buffer buf
                                            (goto-char url-http-end-of-headers)
                                            (delete-region (point-min) (point))
                                            (buffer-string))))
         (errs (assoc 'errors response)))
    (if errs
        (error (concat "Asana API error: " (mapconcat (lambda (err) (cdr (assoc 'message err))) (cdr errs) "\n")))
      (cdr (assoc 'data response)))))

(defun asana-read-response-async (status)
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
  (cdr (assoc 'workspaces (asana-get "/users/me" nil callback))))

(defun asana-get-sections (&optional callback)
  (asana-get (concat "/projects/" asana-my-tasks-project-id "/sections") `(("limit" . "100"))
             callback))

(defun asana-get-tasks (&optional callback)
  (asana-get "/tasks" `(("workspace" . ,(number-to-string asana-selected-workspace-id))
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
                     (setq asana-task-cache (mapcar 'asana-item-helm-data (asana-fold-sections (asana-filter-later tasks))))
                     (and helm-alive-p (helm-update))))
  (asana-get-sections (lambda (sections)
                        (setq asana-section-cache (mapcar 'asana-item-helm-data sections))
                        (and helm-alive-p (helm-update)))))

;; Helm

(defun asana-task-helm-source ()
  `((name . ,(concat "My Asana Tasks in " asana-selected-workspace-name))
    (candidates . ,(lambda () asana-task-cache))
    (volatile)
    (action . (("Select `RET'" . asana-task-select)
               ("Browse (open in Asana) `C-b'" . asana-task-browse)
               ("Move to section `C-:'" . asana-task-move-to-section)
               ("Complete `C-RET'" . asana-task-complete)
               ("Delete `C-DEL'" . asana-task-delete)
               ("Complete marked Tasks `M-RET'" . ,(asana-exec-marked 'asana-task-complete))
               ("Delete marked Tasks `M-DEL'" . ,(asana-exec-marked 'asana-task-delete))))
    (keymap . ,(let ((map (make-sparse-keymap)))
                 (set-keymap-parent map helm-map)
                 (asana-define-key map
                   (kbd "C-b") 'asana-task-browse
                   (kbd "C-:") 'asana-task-move-to-section
                   (kbd "<C-return>") 'asana-task-complete
                   (kbd "<C-backspace>") 'asana-task-delete
                   (kbd "<M-return>") (asana-exec-marked 'asana-task-complete)
                   (kbd "<M-backspace>") (asana-exec-marked 'asana-task-delete))
                 map))))

(defun asana-section-helm-source ()
  `((name . "Sections")
    (candidates . ,(lambda () asana-section-cache))
    (volatile)
    (action . (("Select `RET'" . asana-section-select)))))

(defun asana-item-helm-data (task)
  `(,(cdr (assoc 'name task)) . ,(cdr (assoc 'id task))))

(defun asana-task-select (task-id)
  (lexical-let ((task-id task-id))
    (asana-get-task task-id (lambda (result)
                              (with-output-to-temp-buffer "*Task*"
                                (with-current-buffer "*Task*"
                                  (cl-prettyprint result)
                                  (insert "\n\n===== COMMENTS =====\n")
                                  (cl-prettyprint (asana-get-task-stories task-id))))))))

(defun asana-task-browse (task-id)
  (browse-url (concat "https://app.asana.com/0/"
                      (number-to-string asana-selected-workspace-id)
                      "/"
                      (number-to-string task-id))))

(defun asana-task-move-to-section (task-id)
  (setq asana-selected-task-id task-id)
  (helm :sources (asana-section-helm-source)
        :buffer "*helm-asana*")
  (setq asana-selected-task-id nil))

(defun asana-section-select (section-id)
  (asana-post (concat "/tasks/" (number-to-string asana-selected-task-id) "/addProject")
              `(("project" . ,asana-my-tasks-project-id)
                ("section" . ,section-id))
              (lambda (data)
                (if data
                    (message "Unknown error: couldn't move task.")
                  (message "Task moved.")))))

(defun asana-task-complete (task-id)
  (asana-put (concat "/tasks/" (number-to-string task-id))
             '(("completed" . t))
             (lambda (data)
               (let ((task-name (cdr (assoc 'name data))))
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
  `(,(cdr (assoc 'name workspace)) . ,workspace))

(defun asana-workspace-select (workspace)
  (customize-save-variable 'asana-selected-workspace-id (cdr (assoc 'id workspace)))
  (customize-save-variable 'asana-selected-workspace-name (cdr (assoc 'name workspace)))
  (helm-asana))

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
                         ("workspace" . ,(number-to-string asana-selected-workspace-id)))
              (lambda (data)
                (let ((task-name (cdr (assoc 'name data))))
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
  (if asana-selected-workspace-id
      (progn (asana-invalidate-task-cache)
             (helm :sources (asana-task-helm-source)
                   :buffer "*helm-asana*"))
    (helm :sources (asana-workspace-helm-source)
          :buffer "*helm-asana*")))

(defun helm-asana-change-workspace ()
  "TODO docstring"
  (interactive)
  (customize-save-variable 'asana-selected-workspace-id nil)
  (asana-clear-task-cache)
  (helm-asana))

(provide 'asana)
