(defcustom hobo-public-path "C:/Users/josh/Code/hobo/public"
  "Path to HOBO public assets directory."
  :group 'hobo
  :type 'string)

(defcustom hobo-server-bind-address "127.0.0.1:3000"
  "Address for the HOBO server to listen on."
  :group 'hobo
  :type 'string)

(defun hobo-display-error (msg)
  (let ((buf (get-buffer-create "*hobo errors*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "%s: %s\n" (current-time-string) msg)))
    (display-buffer buf)))

(require 'hobors)

(defvar hobo-errors-thread nil)

(defun hobo--display-errors-loop ()
  (dolist (err (hobors--get-new-errors))
    (hobo-display-error err))
  (when hobo-errors-thread
    (run-with-idle-timer 0 nil #'hobo--display-errors-loop)))

(defun hobo--start-errors-thread ()
  (setq hobo-errors-thread (make-thread 'hobo--display-errors-loop "hobo errors")))

(defun hobo--stop-errors-thread ()
  (thread-signal hobo-errors-thread 'quit nil)
  (setq hobo-errors-thread nil))

(defun hobo-start ()
  "Start the HOBO server."
  (interactive)
  (hobors--start)
  (hobo--start-errors-thread))

(defun hobo-stop ()
  (interactive)
  (hobors--stop)
  (hobo--stop-errors-thread))

(provide 'hobo)
