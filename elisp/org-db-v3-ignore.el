;;; org-db-v3-ignore.el --- Ignore pattern management for org-db -*- lexical-binding: t; -*-

;;; Commentary:
;; Functions for managing ignore patterns to exclude files from indexing.

;;; Code:

(require 'plz)
(require 'json)

(defvar org-db-v3-ignore-patterns-cache nil
  "Cached list of active ignore patterns.")

(defvar org-db-v3-ignore-patterns-cache-time nil
  "Time when ignore patterns were last cached.")

(defcustom org-db-v3-ignore-cache-ttl 60
  "Time in seconds to cache ignore patterns before refreshing."
  :type 'integer
  :group 'org-db-v3)

;;;###autoload
(defun org-db-v3-get-ignore-patterns (&optional active-only callback)
  "Get ignore patterns from server.
If ACTIVE-ONLY is non-nil (default), only return active patterns.
If CALLBACK is provided, make async request and call CALLBACK with results.
Otherwise make synchronous request and return results."
  (interactive)
  (org-db-v3-ensure-server)

  (let ((url (format "%s/api/ignore-patterns?active_only=%s"
                     (org-db-v3-server-url)
                     (if (or (null active-only) active-only) "true" "false"))))
    (if callback
        ;; Async request
        (plz 'get url
          :as #'json-read
          :then callback
          :else (lambda (error)
                  (message "Error fetching ignore patterns: %s" error)))
      ;; Sync request
      (condition-case err
          (let ((response (plz 'get url :as #'json-read)))
            ;; Convert vector to list for compatibility with dolist/mapcar
            (append (alist-get 'patterns response) nil))
        (error
         (message "Error fetching ignore patterns: %S" err)
         nil)))))

;;;###autoload
(defun org-db-v3-add-ignore-pattern (pattern &optional description pattern-type)
  "Add an ignore PATTERN with optional DESCRIPTION.
PATTERN-TYPE can be 'glob (default) or 'regex."
  (interactive "sIgnore pattern: \nsDescription (optional): ")
  (org-db-v3-ensure-server)

  (let ((request-data (json-encode
                       `((pattern . ,pattern)
                         (description . ,(if (string-empty-p description) nil description))
                         (pattern_type . ,(or pattern-type "glob"))
                         (is_active . t)))))
    (plz 'post (concat (org-db-v3-server-url) "/api/ignore-patterns")
      :headers '(("Content-Type" . "application/json"))
      :body request-data
      :as #'json-read
      :then (lambda (response)
              (message "Added ignore pattern: %s" pattern)
              ;; Invalidate cache
              (setq org-db-v3-ignore-patterns-cache nil))
      :else (lambda (error)
              (message "Error adding ignore pattern: %s" error)))))

;;;###autoload
(defun org-db-v3-delete-ignore-pattern (pattern-id)
  "Delete ignore pattern with PATTERN-ID."
  (interactive
   (let* ((patterns (org-db-v3-get-ignore-patterns nil))
          (choices (mapcar (lambda (p)
                            (cons (format "%s - %s"
                                        (alist-get 'pattern p)
                                        (or (alist-get 'description p) ""))
                                  (alist-get 'rowid p)))
                          patterns))
          (selected (completing-read "Delete pattern: " choices nil t)))
     (list (cdr (assoc selected choices)))))

  (org-db-v3-ensure-server)

  (when (yes-or-no-p (format "Delete ignore pattern %d? " pattern-id))
    (plz 'delete (format "%s/api/ignore-patterns/%d"
                        (org-db-v3-server-url)
                        pattern-id)
      :as #'json-read
      :then (lambda (response)
              (message "Deleted ignore pattern %d" pattern-id)
              ;; Invalidate cache
              (setq org-db-v3-ignore-patterns-cache nil))
      :else (lambda (error)
              (message "Error deleting ignore pattern: %s" error)))))

;;;###autoload
(defun org-db-v3-toggle-ignore-pattern (pattern-id current-state)
  "Toggle ignore pattern PATTERN-ID.
CURRENT-STATE is the current is_active value (0 or 1)."
  (org-db-v3-ensure-server)

  (let ((new-state (if (= current-state 1) 0 1))
        (request-data (json-encode `((is_active . ,(if (= current-state 1) :json-false t))))))
    (plz 'put (format "%s/api/ignore-patterns/%d"
                     (org-db-v3-server-url)
                     pattern-id)
      :headers '(("Content-Type" . "application/json"))
      :body request-data
      :as #'json-read
      :then (lambda (response)
              (message "%s ignore pattern %d"
                      (if (= new-state 1) "Enabled" "Disabled")
                      pattern-id)
              ;; Invalidate cache
              (setq org-db-v3-ignore-patterns-cache nil))
      :else (lambda (error)
              (message "Error toggling ignore pattern: %s" error)))))

;;;###autoload
(defun org-db-v3-test-pattern (pattern filepath &optional pattern-type)
  "Test if PATTERN matches FILEPATH.
PATTERN-TYPE can be 'glob (default) or 'regex.
Returns t if pattern matches, nil otherwise."
  (org-db-v3-ensure-server)

  (let ((request-data (json-encode
                       `((pattern . ,pattern)
                         (filepath . ,filepath)
                         (pattern_type . ,(or pattern-type "glob"))))))
    (condition-case err
        (let ((response (plz 'post (concat (org-db-v3-server-url) "/api/ignore-patterns/test")
                          :headers '(("Content-Type" . "application/json"))
                          :body request-data
                          :as #'json-read)))
          (eq (alist-get 'matches response) t))
      (error
       (message "Error testing pattern: %S" err)
       nil))))

;;;###autoload
(defun org-db-v3-reload-ignore-patterns ()
  "Reload ignore patterns from database into server memory.
Also clears the client-side cache."
  (interactive)
  (org-db-v3-ensure-server)

  (plz 'post (concat (org-db-v3-server-url) "/api/ignore-patterns/reload")
    :as #'json-read
    :then (lambda (response)
            (let ((count (alist-get 'count response)))
              (message "Reloaded %d ignore pattern%s"
                      count
                      (if (= count 1) "" "s")))
            ;; Invalidate cache
            (setq org-db-v3-ignore-patterns-cache nil))
    :else (lambda (error)
            (message "Error reloading ignore patterns: %s" error))))

;;;###autoload
(defun org-db-v3-list-ignore-patterns ()
  "Display all ignore patterns in a buffer."
  (interactive)
  (org-db-v3-ensure-server)

  (org-db-v3-get-ignore-patterns
   nil  ; Get both active and inactive
   (lambda (response)
     ;; Convert vector to list for compatibility with dolist
     (let ((patterns (append (alist-get 'patterns response) nil))
           (buf (get-buffer-create "*org-db Ignore Patterns*")))
       (with-current-buffer buf
         (let ((inhibit-read-only t))
           (erase-buffer)
           (insert (propertize "org-db Ignore Patterns\n" 'face 'bold)
                   (propertize (make-string 40 ?=) 'face 'shadow)
                   "\n\n")

           (if (zerop (length patterns))
               (insert "No ignore patterns defined.\n")
             (dolist (pattern patterns)
               (let ((id (alist-get 'rowid pattern))
                     (pat (alist-get 'pattern pattern))
                     (type (alist-get 'pattern_type pattern))
                     (active (= (alist-get 'is_active pattern) 1))
                     (desc (alist-get 'description pattern))
                     (last-used (alist-get 'last_used_at pattern)))

                 (insert (propertize (format "[%s] " (if active "✓" " "))
                                   'face (if active 'success 'shadow))
                        (propertize pat 'face (if active 'default 'shadow))
                        (propertize (format " (%s)" type) 'face 'font-lock-comment-face)
                        "\n")

                 (when desc
                   (insert (propertize (format "    %s\n" desc)
                                     'face 'font-lock-doc-face)))

                 (when last-used
                   (insert (propertize (format "    Last used: %s\n" last-used)
                                     'face 'shadow)))

                 (insert (propertize (format "    ID: %d\n" id)
                                   'face 'shadow))
                 (insert "\n"))))

           (insert "\n"
                   (propertize "Commands:\n" 'face 'bold)
                   "  a - Add pattern\n"
                   "  d - Delete pattern\n"
                   "  t - Toggle pattern\n"
                   "  r - Reload patterns\n"
                   "  g - Refresh list\n"
                   "  q - Quit\n")

           (goto-char (point-min)))

         (org-db-v3-ignore-patterns-mode))

       (display-buffer buf)))))

(defvar org-db-v3-ignore-patterns-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "a") #'org-db-v3-add-ignore-pattern)
    (define-key map (kbd "d") #'org-db-v3-delete-ignore-pattern)
    (define-key map (kbd "r") #'org-db-v3-reload-ignore-patterns)
    (define-key map (kbd "g") #'org-db-v3-list-ignore-patterns)
    (define-key map (kbd "q") #'quit-window)
    map)
  "Keymap for `org-db-v3-ignore-patterns-mode'.")

(define-derived-mode org-db-v3-ignore-patterns-mode special-mode "org-db-ignore"
  "Major mode for managing org-db ignore patterns."
  (setq buffer-read-only t))

(defun org-db-v3--get-cached-patterns ()
  "Get cached ignore patterns or fetch from server if cache expired."
  (let ((now (float-time)))
    (when (or (null org-db-v3-ignore-patterns-cache)
              (null org-db-v3-ignore-patterns-cache-time)
              (> (- now org-db-v3-ignore-patterns-cache-time)
                 org-db-v3-ignore-cache-ttl))
      (setq org-db-v3-ignore-patterns-cache
            (org-db-v3-get-ignore-patterns t))
      (setq org-db-v3-ignore-patterns-cache-time now))
    org-db-v3-ignore-patterns-cache))

(defun org-db-v3--should-ignore-file (filename)
  "Check if FILENAME should be ignored based on cached patterns.
Returns non-nil if file should be ignored."
  (let ((patterns (org-db-v3--get-cached-patterns)))
    (when patterns
      (catch 'matched
        (dolist (pattern patterns)
          (let ((pat (alist-get 'pattern pattern))
                (type (alist-get 'pattern_type pattern)))
            ;; Simple client-side glob matching
            (when (and (string= type "glob")
                      (org-db-v3--simple-glob-match filename pat))
              (throw 'matched t))))
        nil))))

(defun org-db-v3--simple-glob-match (filename pattern)
  "Simple glob pattern matching for common cases.
FILENAME is the file to check, PATTERN is the glob pattern.
This is a simplified client-side check; server does full matching."
  (cond
   ;; Pattern ends with /** - directory matching
   ((string-suffix-p "/**" pattern)
    (let ((dir (substring pattern 0 -3)))
      (string-match-p (regexp-quote dir) filename)))

   ;; Pattern is *.ext - extension matching
   ((string-prefix-p "*." pattern)
    (string-suffix-p (substring pattern 1) filename))

   ;; Pattern contains no wildcards - exact component matching
   ((not (string-match-p "[*?[]" pattern))
    (string-match-p (regexp-quote pattern) filename))

   ;; For complex patterns, assume might match (server will check properly)
   (t nil)))

(provide 'org-db-v3-ignore)
;;; org-db-v3-ignore.el ends here
