(require 'json)
(require 'url)
(require 'url-http)

(exec-path-from-shell-copy-env "ASANA_TOKEN")

(defconst asana-api-root "https://app.asana.com/api/1.0")
(defconst asana-token (getenv "ASANA_TOKEN"))
(defvar asana-user-id nil)
(defvar asana-workspace-id nil)

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
        (error (concat "Asana API error: " (mapconcat #'(lambda (err) (cdr (assoc 'message err))) (cdr errs) "\n")))
      (cdr (assoc 'data response)))))

(defun asana-get (resource &optional params)
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
    (asana-read-response (url-retrieve-synchronously url))))

(defun asana-get-workspaces ()
  (let ((data (asana-get "/users/me")))
    (customize-save-variable 'asana-user-id (cdr (assoc 'id data)))
    (cdr (assoc 'workspaces data))))

(defun asana-get-tasks ()
  (asana-get "/tasks" `(("assignee" . ,(number-to-string asana-user-id))
                        ("workspace" . ,(number-to-string asana-workspace-id))
                        ("limit" . "100")
                        ("completed_since" . "now"))))

;; Helm

(defun asana-task-helm-source ()
  `((name . ,(concat "My Asana Tasks in " asana-workspace-name))
    (candidates . ,(mapcar #'asana-task-helm-data (asana-get-tasks)))
    (action . (("Select" . asana-task-select)
               ("Open in Asana" . asana-task-browse)
               ("Complete" . asana-task-complete)
               ("Delete" . asana-task-delete)))))

(defun asana-task-helm-data (task)
  `(,(cdr (assoc 'name task)) . ,(cdr (assoc 'id task))))

(defun asana-task-select (task-id)
  (print task-id)) ; TODO

(defun asana-task-browse (task-id)
  (print task-id)) ; TODO

(defun asana-task-complete (task-id)
  (print task-id)) ; TODO

(defun asana-task-delete (task-id)
  (print task-id)) ; TODO


(defun asana-workspace-helm-source ()
  `((name . "Asana Workspaces")
    (candidates . ,(mapcar #'asana-workspace-helm-data (asana-get-workspaces)))
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
