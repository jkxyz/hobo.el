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
      (display-buffer buf))))

(defvar hobo-logger-process nil)

(defun hobo-init-logger ()
  (when (not hobo-logger-process)
    (let ((addr (hobors--init-logger)))
      (setq hobo-logger-process
            (make-network-process :name "hobo logger"
                                  :buffer "*hobo logs*"
                                  :host (nth 0 addr)
                                  :service (nth 1 addr))))))

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
