(defcustom hobo-public-path "C:/Users/josh/Code/hobo/public"
  "Path to HOBO public assets directory."
  :group 'hobo
  :type 'string)

(defcustom hobo-server-bind-address "127.0.0.1:3000"
  "Address for the HOBO server to listen on."
  :group 'hobo
  :type 'string)

(require 'hobors)

(defun hobo--buffer-after-change (start end length)
  (let ((text (buffer-substring-no-properties start end)))
    (hobors--update-buffer (buffer-name) (- start 1) length text)))

(defun hobo-create-buffer ()
  (let ((buf (get-buffer-create "*hobo*")))
    (with-current-buffer buf
      (add-hook 'after-change-functions 'hobo--buffer-after-change nil t)
      (display-buffer buf '(display-buffer-same-window)))))

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

(defun hobo-start ()
  "Start the HOBO server."
  (interactive)
  (hobo-init-logger)
  (hobors--start)
  (hobo-create-buffer))

(defun hobo-stop ()
  (interactive)
  (hobors--stop))

(provide 'hobo)
