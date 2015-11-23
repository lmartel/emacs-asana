(require 'json)
(require 'url)
(require 'url-http)

(exec-path-from-shell-copy-env "ASANA_TOKEN")

(defconst asana-api-root "https://app.asana.com/api/1.0")
(defconst asana-token (getenv "ASANA_TOKEN"))
(defvar asana-workspace-id nil)

;; Helpers

(setq debug-on-error t)

(defun asana-compose (a b)
  (lexical-let ((a a)
                (b b))
    (lambda (&rest args)
      (funcall a (apply b args)))))

;; API

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

(defun asana-put (resource &optional params callback)
  (let ((url-request-method "PUT")
        (url-request-extra-headers (asana-headers-with-auth))
        (url-request-data (json-encode `(("data" . ,params))))
        (url (concat asana-api-root resource)))
    (if callback
        (url-retrieve url (asana-compose callback 'asana-read-response-async))
      (asana-read-response (url-retrieve-synchronously url)))))

(defun asana-get (resource &optional params callback)
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

(defun asana-get-workspaces (&optional callback)
  (cdr (assoc 'workspaces (asana-get "/users/me" nil callback))))

(defun asana-get-tasks (&optional callback)
  (remove-if (lambda (task) (equal (cdr (assoc 'assignee_status task)) "later"))
             (asana-get "/tasks" `(("workspace" . ,(number-to-string asana-workspace-id))
                                   ("opt_fields" . "id,name,assignee_status")
                                   ("limit" . "100")
                                   ("assignee" . "me")
                                   ("completed_since" . "now"))
                        callback)))

(defun asana-get-task (task-id &optional callback)
  (asana-get (concat "/tasks/" (number-to-string task-id)) nil callback))

(defun asana-get-task-stories (task-id &optional callback)
  (asana-get (concat "/tasks/" (number-to-string task-id) "/stories") nil callback))

;; Helm

(defun asana-task-helm-source ()
  `((name . ,(concat "My Asana Tasks in " asana-workspace-name))
    (candidates . ,(mapcar 'asana-task-helm-data (asana-get-tasks)))
    (action . (("Select" . asana-task-select)
               ("Open in Asana" . asana-task-browse)
               ("Complete" . asana-task-complete)
               ("Delete" . asana-task-delete)))))

(defun asana-task-helm-data (task)
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
                      (number-to-string asana-workspace-id)
                      "/"
                      (number-to-string task-id))))

(defun asana-task-complete (task-id)
  (asana-put (concat "/tasks/" (number-to-string task-id))
             '(("completed" . t))
             (lambda (response)
               (let ((task-name (cdr (assoc 'name response))))
                 (if (assoc 'completed response)
                     (message "`%s' completed." task-name)
                   (message "Unknown error: couldn't complete `%s'" task-name))))))

(defun asana-task-delete (task-id)
  (print task-id)) ; TODO


(defun asana-workspace-helm-source ()
  `((name . "Asana Workspaces")
    (candidates . ,(mapcar 'asana-workspace-helm-data (asana-get-workspaces)))
    (action . (("Select" . asana-workspace-select)))))

(defun asana-workspace-helm-data (workspace)
  `(,(cdr (assoc 'name workspace)) . ,workspace))

(defun asana-workspace-select (workspace)
  (customize-save-variable 'asana-workspace-id (cdr (assoc 'id workspace)))
  (customize-save-variable 'asana-workspace-name (cdr (assoc 'name workspace)))
  (helm-asana))

;; Interactive

(defun helm-asana ()
  "TODO docstring"
  (interactive)
  (if asana-workspace-id
      (helm :sources (asana-task-helm-source)
            :buffer "*helm-asana*")
    (helm :sources (asana-workspace-helm-source)
          :buffer "*helm-asana*")))

(defun helm-asana-change-workspace ()
  "TODO docstring"
  (interactive)
  (customize-save-variable 'asana-workspace-id nil)
  (helm-asana))

(provide 'asana)
