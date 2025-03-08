(defcustom hobo-public-path "C:/Users/josh/Code/hobo/public"
  "Path to Hobo public assets directory."
  :group 'hobo
  :type 'string)

(defcustom hobo-server-bind-address "127.0.0.1:3000"
  "Address for the Hobo server to listen on."
  :group 'hobo
  :type 'string)

(defcustom hobo-logs-buffer-name "*hobo-logs*"
  "The name of the Hobo logs buffer."
  :group 'hobo
  :type 'string)

(defcustom hobo-ignored-buffers `(,hobo-logs-buffer-name)
  "Buffers that shouldn't be synced."
  :group 'hobo
  :type '(repeat string))

(require 'hobors)

(defvar hobo-logger-process nil)

(defun hobo--init-logs ()
  (when (not hobo-logger-process)
    (let ((addr (hobors--init-logger))
          (log-buffer (get-buffer-create hobo-logs-buffer-name)))
      (with-current-buffer log-buffer
        (unless (eq major-mode 'special-mode)
          (special-mode))
        (setq buffer-read-only t)
        (set (make-local-variable 'window-point-insertion-type) t))
      (setq hobo-logger-process
            (make-network-process
             :name "hobo logs client"
             :buffer log-buffer
             :host (nth 0 addr)
             :service (nth 1 addr)
             :filter #'hobo--logs-process-filter)))))

(defun hobo--logs-process-filter (proc string)
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
  "Display the Hobo logs buffer."
  (interactive)
  (display-buffer hobo-logs-buffer-name))

(defvar hobo--buffer-last-state (make-hash-table :test 'equal)
  "A hash of buffer name to buffer string.

Stores the contents of sycned buffers at the time they were last synced.
This helps to avoid expensive resets of the entire buffer contents, which
creates very large diffs for the client, and also doesn't preserve the
intention of the edit when resolving conflicts.")

(defun hobo--buffer-after-change (begin end length)
  "Called when a buffer change is observed through `after-change-functions'."
  (let ((buffer-name (buffer-name))
        (changed-text (buffer-substring-no-properties begin end)))
    (hobors--update-buffer buffer-name begin length changed-text)
    (puthash buffer-name (buffer-string) hobo--buffer-last-state)))

(defun hobo--reset-buffer ()
  "Reset or initialize the content of the current buffer."
  (let* ((buffer-name (buffer-name))
         (current-content (buffer-string))
         (last-known-content (gethash buffer-name hobo--buffer-last-state nil)))
    (unless (and last-known-content
                 (string= current-content last-known-content))
      (hobors--reset-buffer buffer-name current-content)
      (puthash buffer-name current-content hobo--buffer-last-state))))

(defun hobo--kill-buffer ()
  "Remove the current buffer from the syned buffers."
  (let ((buffer-name (buffer-name)))
    (hobors--kill-buffer buffer-name)
    (remhash buffer-name hobo--buffer-last-state)))

(defun hobo--should-ignore-buffer-p (buffer)
  "Return t when BUFFER should be ignored."
  (let ((buffer-name (buffer-name buffer)))
    (or (string-match-p "^ " buffer-name)
        (minibufferp)
        (member buffer-name hobo-ignored-buffers))))

(defun hobo--ensure-buffer-monitored ()
  "Ensure that the current buffer is being monitored."
  (unless (hobo--should-ignore-buffer-p (current-buffer))
    (add-hook 'after-change-functions 'hobo--buffer-after-change nil t)
    (add-hook 'post-command-hook 'hobo--reset-buffer nil t)
    (hobo--reset-buffer)))

(defun hobo-start ()
  "Start the Hobo server."
  (interactive)
  (hobo--init-logs)
  (hobors--start)

  (add-hook 'find-file-hook 'hobo--ensure-buffer-monitored)
  (add-hook 'after-change-major-mode-hook 'hobo--ensure-buffer-monitored)
  (add-hook 'kill-buffer-hook 'hobo--kill-buffer)

  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (hobo--ensure-buffer-monitored)))

  (add-hook 'window-configuration-change-hook
            (lambda ()
              (dolist (window (window-list))
                (with-current-buffer (window-buffer window)
                  (hobo--ensure-buffer-monitored)))))

  ;; TODO Global hook to reset messages buffer
  )

(defun hobo-stop ()
  "Stop the Hobo server."
  (interactive)
  (hobors--stop))

(provide 'hobo)
