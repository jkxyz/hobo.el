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
      (insert (format "%s\n" msg)))
    (display-buffer buf)))

(require 'hobors)

(defun hobo--display-errors-loop ()
  (let ((err (hobors--last-error)))
    (when err
      (hobo-display-error err))
    (run-with-idle-timer 0 nil #'hobo--display-errors-loop)))

(defvar hobo-errors-thread
  (make-thread 'hobo--display-errors-loop))

(defun hobo-start ()
  "Start the HOBO server."
  (interactive)
  (hobors--start))

(defun hobo-stop ()
  (interactive)
  (hobors--stop))

(provide 'hobo)
