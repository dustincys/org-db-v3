;;; org-db-v3-client.el --- HTTP client for org-db -*- lexical-binding: t; -*-

;;; Commentary:
;; Async HTTP client using plz.el to communicate with the server.

;;; Code:

(require 'plz)
(require 'json)
;; (require 'org-db-v3)
(require 'org-db-v3-parse)
(require 'org-db-v3-server)

(defvar org-db-v3-index-queue nil
  "Queue of files pending indexing.")

(defvar org-db-v3-indexing-in-progress nil
  "Whether indexing is currently in progress.")

(defun org-db-v3-add-to-queue (filename)
  "Add FILENAME to indexing queue."
  (unless (member filename org-db-v3-index-queue)
    (push filename org-db-v3-index-queue)))

(defun org-db-v3-index-file-async (filename)
  "Index FILENAME asynchronously."
  (org-db-v3-ensure-server)

  (with-current-buffer (find-file-noselect filename)
    (let ((json-data (org-db-v3-parse-buffer-to-json)))
      (plz 'post (concat (org-db-v3-server-url) "/api/index/file")
        :headers '(("Content-Type" . "application/json"))
        :body json-data
        :as #'json-read
        :then (lambda (response)
                (message "Indexed %s (%d headlines)"
                         filename
                         (alist-get 'headlines_count response)))
        :else (lambda (error)
                (message "Error indexing %s: %s" filename error))))))

(defun org-db-v3-process-queue ()
  "Process the indexing queue."
  (when (and org-db-v3-index-queue
             (not org-db-v3-indexing-in-progress)
             (current-idle-time))
    (setq org-db-v3-indexing-in-progress t)
    (let ((filename (pop org-db-v3-index-queue)))
      (when (file-exists-p filename)
        (org-db-v3-index-file-async filename))
      (setq org-db-v3-indexing-in-progress nil))))

(defvar org-db-v3-idle-timer nil
  "Timer for processing the queue.")

(defun org-db-v3-start-queue-processing ()
  "Start the idle timer for queue processing."
  (unless org-db-v3-idle-timer
    (setq org-db-v3-idle-timer
          (run-with-idle-timer 5 t #'org-db-v3-process-queue))))

(defun org-db-v3-stop-queue-processing ()
  "Stop the idle timer."
  (when org-db-v3-idle-timer
    (cancel-timer org-db-v3-idle-timer)
    (setq org-db-v3-idle-timer nil)))

(provide 'org-db-v3-client)
;;; org-db-v3-client.el ends here
