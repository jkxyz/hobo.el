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

(run-with-timer 0 1 (lambda ()
                      (dolist (log (hobors--consume-logs))
                        (hobo--display-log log))))

(defun hobo--buffer-after-change (start end length)
  (let ((text (buffer-substring-no-properties start end)))
    (hobors--update-buffer (buffer-name) (- start 1) length text)))

(defun hobo-create-buffer ()
  (let ((buf (get-buffer-create "*hobo*")))
    (with-current-buffer buf
      (add-hook 'after-change-functions 'hobo--buffer-after-change nil t)
      (display-buffer buf))))

(defun hobo-start ()
  "Start the HOBO server."
  (interactive)
  (hobors--start)
  (hobo-create-buffer))

(defun hobo-stop ()
  (interactive)
  (hobors--stop))

(provide 'hobo)
