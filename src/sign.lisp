(in-package #:failscale/client)

(defun make-ed25519-privkey (priv-hex pub-hex)
  "Create an Ironclad private-key object using 32-byte seed and 32-byte pub."
  (ironclad:make-private-key :ed25519 :x (hex-to-octets priv-hex) :y (hex-to-octets pub-hex)))

(defun canonical-sign-string (method path ts nonce body-octs)
  "METHOD, PATH, TS, NONCE are strings; BODY-OCTS is (u8). Returns UTF-8 octets."
  (utf8-to-octets
   (format nil "~A~%~A~%~A~%~A~%~A~%"
           method path ts nonce (sha256-hex body-octs))))

(defun sign-headers (method path body-octs reg-priv-hex reg-pub-hex)
  "Produce an alist of HTTP headers for the failscale server."
  (let* ((ts (rfc3339-now))
         (nonce (random-hex 16))
         (priv (make-ed25519-privkey reg-priv-hex reg-pub-hex))
         (msg  (canonical-sign-string method path ts nonce body-octs))
         (sig  (ironclad:sign-message priv msg)))
    `(("X-Ed25519-Key"   . ,(string-downcase reg-pub-hex))
      ("X-Ed25519-Sig"   . ,(octets-to-b64 sig))
      ("X-Ed25519-Nonce" . ,nonce)
      ("X-Ed25519-Time"  . ,ts))))
