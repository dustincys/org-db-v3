;;; org-db-v3-server.el --- Server management -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions to start, stop, and check the org-db server.

;;; Code:

;; (require 'org-db-v3)

(defcustom org-db-v3-python-command "uv"
  "Command to run Python (uv, python3, etc)."
  :type 'string
  :group 'org-db-v3)

(defcustom org-db-v3-server-directory
  (expand-file-name "python" (file-name-directory (or load-file-name buffer-file-name)))
  "Directory containing the Python server code."
  :type 'directory
  :group 'org-db-v3)

(defun org-db-v3-start-server ()
  "Start the org-db server."
  (interactive)
  (if (org-db-v3-server-running-p)
      (message "org-db server already running")
    (let* ((default-directory org-db-v3-server-directory)
           (process-name "org-db-server")
           (buffer-name "*org-db-server*")
           (cmd (list org-db-v3-python-command "run" "uvicorn"
                     "org_db_server.main:app" "--reload"
                     "--host" org-db-v3-server-host
                     "--port" (number-to-string org-db-v3-server-port))))

      (setq org-db-v3-server-process
            (make-process
             :name process-name
             :buffer buffer-name
             :command cmd
             :sentinel #'org-db-v3-server-sentinel))

      ;; Wait a bit for server to start
      (sleep-for 2)

      (if (org-db-v3-server-running-p)
          (message "org-db server started on %s:%d"
                   org-db-v3-server-host org-db-v3-server-port)
        (error "Failed to start org-db server. Check *org-db-server* buffer")))))

(defun org-db-v3-stop-server ()
  "Stop the org-db server."
  (interactive)
  (when (and org-db-v3-server-process
             (process-live-p org-db-v3-server-process))
    (kill-process org-db-v3-server-process)
    (setq org-db-v3-server-process nil)
    (message "org-db server stopped")))

(defun org-db-v3-server-sentinel (process event)
  "Sentinel for server PROCESS EVENT."
  (when (string-match-p "\\(finished\\|exited\\)" event)
    (message "org-db server process %s" event)
    (setq org-db-v3-server-process nil)))

(defun org-db-v3-ensure-server ()
  "Ensure server is running, start if needed."
  (unless (org-db-v3-server-running-p)
    (when org-db-v3-auto-start-server
      (org-db-v3-start-server))))

(provide 'org-db-v3-server)
;;; org-db-v3-server.el ends here
