(defcustom hobo-public-path "C:/Users/josh/Code/hobo/public"
  "Path to HOBO public assets directory."
  :group 'hobo
  :type 'string)

(defcustom hobo-server-bind-address "127.0.0.1:3000"
  "Address for the HOBO server to listen on."
  :group 'hobo
  :type 'string)

(defcustom hobo-ignored-buffers '("*hobo logs*")
  "Buffers that shouldn't be synced."
  :group 'hobo
  :type '(repeat string))

(require 'hobors)

(defvar hobo-logger-process nil)

(defun hobo-init-logger ()
  (when (not hobo-logger-process)
    (let ((addr (hobors--init-logger))
          (log-buffer (get-buffer-create "*hobo logs*")))
      (with-current-buffer log-buffer
        (unless (eq major-mode 'special-mode)
          (special-mode))
        (setq buffer-read-only t)
        (set (make-local-variable 'window-point-insertion-type) t))
      (setq hobo-logger-process
            (make-network-process
             :name "hobo logger"
             :buffer log-buffer
             :host (nth 0 addr)
             :service (nth 1 addr)
             :filter #'hobo-logger-filter)))))

(defun hobo-logger-filter (proc string)
  (when (buffer-live-p (process-buffer proc))
    (with-current-buffer (process-buffer proc)
      (let ((inhibit-read-only t)
            (moving (= (point) (process-mark proc))))
        (save-excursion
          (goto-char (process-mark proc))
          (insert string)
          (set-marker (process-mark proc) (point)))
        (when moving
          (goto-char (process-mark proc)))))))

(defun hobo-display-logs ()
  (interactive)
  (display-buffer "*hobo logs*"))

(defun hobo--buffer-before-kill ()
  (hobors--kill-buffer (buffer-name)))

(defvar hobo--buffer-last-state (make-hash-table :test 'equal))

(defun hobo--buffer-after-change (begin end length)
  (let ((buffer-name (buffer-name))
        (changed-text (buffer-substring-no-properties begin end)))
    (hobors--update-buffer buffer-name begin length changed-text)
    (puthash buffer-name (buffer-string) hobo--buffer-last-state)))

(defun hobo--verify-buffer-sync ()
  (let* ((buffer-name (buffer-name))
         (current-content (buffer-string))
         (last-known-content (gethash buffer-name hobo--buffer-last-state nil)))
    (unless (and last-known-content
                 (string= current-content last-known-content))
      (hobors--reset-buffer buffer-name current-content)
      (puthash buffer-name current-content hobo--buffer-last-state))))

(defun hobo--ensure-buffer-monitored ()
  (let ((buffer-name (buffer-name)))
    (unless (or (string-match-p "^ " buffer-name)
                (minibufferp)
                (member buffer-name hobo-ignored-buffers))
      (add-hook 'after-change-functions 'hobo--buffer-after-change nil t)
      (add-hook 'post-command-hook 'hobo--verify-buffer-sync nil t)
      (hobo--verify-buffer-sync))))

(defun hobo-start ()
  "Start the HOBO server."
  (interactive)
  (hobo-init-logger)
  (hobors--start)

  (add-hook 'find-file-hook 'hobo--ensure-buffer-monitored)
  (add-hook 'after-change-major-mode-hook 'hobo--ensure-buffer-monitored)

  (add-hook 'window-configuration-change-hook 
            (lambda () 
              (dolist (window (window-list))
                (with-current-buffer (window-buffer window)
                  (hobo--ensure-buffer-monitored))))))

(defun hobo-stop ()
  (interactive)
  (hobors--stop))

(provide 'hobo)
