(in-package #:failscale/client)

(defun utf8-to-octets (s)
  (sb-ext:string-to-octets s :external-format :utf-8))

(defun octets-to-utf8 (octs)
  (sb-ext:octets-to-string octs :external-format :utf-8))

(defun hex-to-octets (hex)
  (ironclad:hex-string-to-byte-array (string-downcase hex)))

(defun octets-to-hex (octs)
  (ironclad:byte-array-to-hex-string octs))

(defun b64-to-octets (s)
  (cl-base64:base64-string-to-usb8-array s))

(defun octets-to-b64 (octs)
  (cl-base64:usb8-array-to-base64-string octs))

(defun sha256-hex (octs)
  (octets-to-hex (ironclad:digest-sequence :sha256 octs)))

(defun rfc3339-now ()
  (local-time:format-rfc3339-timestring nil (local-time:now)
                                        :timezone local-time:+utc-zone+))

(defun random-hex (nbytes)
  (octets-to-hex (ironclad:random-data nbytes)))

(defun ensure-dir (path)
  (uiop:ensure-all-directories-exist (list path)))


(defun slurp (path)
  (with-open-file (in path) (let ((s (make-string (file-length in))))
                              (read-sequence s in) s)))

(defun write-string-file (path s &key (if-exists :supersede) (perm #o600))
  (with-open-file (out path :direction :output :if-exists if-exists)
    (write-string s out))
  (ignore-errors (uiop:run-program `("chmod" ,(format nil "~3,'0o" perm) ,(namestring path))
                                   :ignore-error-status t))
  path)

(defun run (&rest argv)
  (uiop:run-program argv
                    :output :string
                    :error-output :string
                    :ignore-error-status t))
