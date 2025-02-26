(defcustom hobo-public-path "C:/Users/josh/Code/hobo/public"
  "Path to HOBO public assets directory."
  :group 'hobo
  :type 'string)

(defcustom hobo-server-bind-address "127.0.0.1:3000"
  "Address for the HOBO server to listen on."
  :group 'hobo
  :type 'string)

(require 'hobors)

(defun hobo-start ()
  "Start the HOBO server."
  (interactive)
  (hobors-start))

(provide 'hobo)
