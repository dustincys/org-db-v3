;;; Directory Local Variables for org-db-v3 project
;;; For more information see (info "(emacs) Directory Variables")

((nil . ((eval . (progn
                   ;; Set project root for convenience
                   (setq-local org-db-v3-project-root
                               (locate-dominating-file default-directory ".dir-locals.el"))

                   ;; Use local database in db/ directory
                   (setenv "ORG_DB_DB_PATH"
                           (expand-file-name "db/org-db-v3.db" org-db-v3-project-root))

                   ;; Use a different port to avoid conflicts with global instance
                   (setq-local org-db-v3-server-port 8766)
                   (setenv "ORG_DB_PORT" "8766")

                   ;; Point to the python directory in this project
                   (setq-local org-db-v3-server-directory
                               (expand-file-name "python" org-db-v3-project-root))

                   ;; Enable debug mode for development
                   (setq-local org-db-v3-debug t)

                   ;; Auto-start server when in this project
                   (setq-local org-db-v3-auto-start-server t)

                   ;; Message to confirm settings loaded
                   (message "org-db-v3 local settings loaded: DB in db/, port 8766"))))))

;; Org-mode specific settings
(org-mode . ((eval . (progn
                       ;; Enable all indexing features for testing
                       (setq-local org-db-v3-index-linked-files t)
                       (setq-local org-db-v3-index-images t)
                       (setq-local org-db-v3-index-audio-files t)))))

;; Python mode settings
(python-mode . ((eval . (setenv "ORG_DB_DB_PATH"
                                (expand-file-name "db/org-db-v3.db"
                                                  (locate-dominating-file default-directory ".dir-locals.el"))))))

;; Emacs Lisp mode settings
(emacs-lisp-mode . ((eval . (setq-local flycheck-disabled-checkers '(emacs-lisp-checkdoc))))))
