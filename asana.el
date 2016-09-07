(require 'json)
(require 'url)
(require 'url-http)

;; User config variables

(defvar asana-keymap-prefix "C-c a")
(exec-path-from-shell-copy-env "ASANA_TOKEN")

;; Internal variables

(defconst asana-api-root "https://app.asana.com/api/1.0")
(defconst asana-token (getenv "ASANA_TOKEN"))

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
  `(lambda (unused-selection)
     (mapcar ,candidate-func (helm-marked-candidates))))

(defmacro asana-exec-marked (candidates-func)
  `(lambda (unused-selection)
     (funcall ,candidates-func (helm-marked-candidates))))



(defmacro asana-assocdr (key alist)
  `(cdr (assoc ,key ,alist)))

(defun asana-kbd (keyseq)
  (kbd (concat asana-keymap-prefix " " keyseq)))

(defun asana-compose (a b)
  (lexical-let ((a a)
                (b b))
    (lambda (&rest args)
      (funcall a (apply b args)))))

(defun asana-filter-later (tasks)
  (remove-if (lambda (task) (equal (asana-assocdr 'assignee_status task) "later")) tasks))

(defun asana-fold-sections (tasks)
  (let ((prev-section (asana-assocdr 'name asana-selected-section)))
    (setq asana-selected-section nil)
    (let ((tasks-with-sections (mapcar (lambda (task)
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
      (remove-if 'null tasks-with-sections))))

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
        (error (concat "Asana API error: " (mapconcat (lambda (err) (asana-assocdr 'message err)) (cdr errs) "\n")))
      (asana-assocdr 'data response))))

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
  (lexical-let ((task-id task-id))
    (asana-get-task task-id (lambda (result)
                              (with-output-to-temp-buffer "*Task*"
                                (with-current-buffer "*Task*"
                                  (cl-prettyprint result)
                                  (insert "\n\n===== COMMENTS =====\n")
                                  (cl-prettyprint (asana-get-task-stories task-id))))))))

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
  (customize-save-variable 'asana-selected-workspace workspace)
  (asana-get "/users/me" `(("workspace" . ,(number-to-string (asana-assocdr 'id asana-selected-workspace)))
                           ("opt_fields" . "atm_id"))
             (lambda (data)
               (customize-save-variable 'asana-my-tasks-project-id (asana-assocdr 'atm_id data))))
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
