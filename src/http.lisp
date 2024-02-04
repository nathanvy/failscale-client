(in-package #:failscale/client)

(defun json-to-hash (s)
  (with-input-from-string (in s) (shasht:read-json in)))

(defun hash-get (h &rest keys)
  "Fetch first present of KEYS from hash H, trying strings and keywords."
  (labels ((one (k)
             (or (gethash k h)
                 (and (stringp k) (gethash (intern (string-upcase k) :keyword) h))
                 (and (symbolp k) (gethash (string-downcase (symbol-name k)) h)))))
    (loop for k in keys
          for v = (one k)
          when v do (return v))))

(defun encode-body (alist)
  (with-output-to-string (s)
    (shasht:write-json
     (let ((h (make-hash-table :test #'equal)))
       (dolist (kv alist) (setf (gethash (car kv) h) (cdr kv)))
       h) s :indent nil)))

(defun sync-once ()
  "Return parsed JSON as a hash-table."
  (let* ((url (or (cfg :server-url) "http://127.0.0.1:8080"))
         (path "/v1/sync")
         (iface (or (cfg :iface) "wg-fs0"))
         (listen (or (cfg :listen-port) 51820))
         (keep   (or (cfg :keepalive) 25))
         ;; WireGuard pubkey (base64)
         (wg-priv (or (cfg :wg-private-key-path) (format nil "data/wg/~A.key" iface)))
         (wg-pub-b64 (ensure-wg-keys+pub wg-priv))
         ;; body
         (body-json (encode-body `(("wg_pub" . ,wg-pub-b64)
                                   ("listen_port" . ,listen)
                                   ,@(when (cfg :endpoint-hint)
                                       (list (cons "endpoint_hint" (cfg :endpoint-hint))))
                                   ("keepalive" . ,keep))))
         (body-octs (utf8-to-octets body-json))
         ;; auth
         (reg-priv (or (cfg :reg-priv-hex)
                       (error "Missing :reg-priv-hex in client config")))
         (reg-pub  (or (cfg :reg-pub-hex)
                       (error "Missing :reg-pub-hex in client config")))
         (hdrs (append (sign-headers "POST" path body-octs reg-priv reg-pub)
                       '(("Content-Type" . "application/json"))))
         ;; HTTP
         (resp (dexador:post (concatenate 'string url path)
                             :headers hdrs
                             :content body-json
                             :connect-timeout 5 :read-timeout 15)))
    (json-to-hash resp)))
