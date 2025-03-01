(defcustom hobo-public-path "C:/Users/josh/Code/hobo/public"
  "Path to HOBO public assets directory."
  :group 'hobo
  :type 'string)

(defcustom hobo-server-bind-address "127.0.0.1:3000"
  "Address for the HOBO server to listen on."
  :group 'hobo
  :type 'string)

(require 'hobors)

(defun hobo--display-log (log)
  (let ((buf (get-buffer-create "*hobo logs*")))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert (format "%s\n" log)))
    (display-buffer buf)))

(defun hobo--display-logs-loop ()
  (dolist (log (hobors--consume-logs))
    (hobo--display-log log))
  (run-with-idle-timer 0 nil #'hobo--display-logs-loop))

(hobo--display-logs-loop)

(defun hobo-start ()
  "Start the HOBO server."
  (interactive)
  (hobors--start))

(defun hobo-stop ()
  (interactive)
  (hobors--stop))

(provide 'hobo)
