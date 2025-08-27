(in-package #:failscale/client)

(defun ensure-wg-keys+pub (priv-path)
  "Ensure base64 private key exists at PRIV-PATH; return base64 public key string."
  (ensure-dir (uiop:pathname-directory-pathname priv-path))
  (unless (probe-file priv-path)
    (multiple-value-bind (out err code) (run "wg" "genkey")
      (declare (ignore err))
      (unless (zerop code) (error "wg genkey failed"))
      (write-string-file priv-path (string-trim '(#\Newline #\Return) out) :perm #o600)))
  ;; compute pub
  (let ((priv (string-trim '(#\Newline #\Return) (slurp priv-path))))
    (multiple-value-bind (out err code)
        (uiop:run-program '("wg" "pubkey") :input priv :output :string :error-output :string :ignore-error-status t)
      (declare (ignore err))
      (unless (zerop code) (error "wg pubkey failed"))
      (string-trim '(#\Newline #\Return) out))))

(defun render-wg-config (self-addr-v4 listen-port peers)
  "Peers = vector/list of hash-tables with keys (addr_v4 wg_pub endpoint keepalive)."
  (with-output-to-string (s)
    (format s "[Interface]~%")
    (format s "PrivateKey = ~A~%" (string-trim '(#\Newline #\Return)
                                               (slurp (cfg :wg-private-key-path))))
    (when self-addr-v4 (format s "Address = ~A~%" self-addr-v4))
    (when listen-port  (format s "ListenPort = ~D~%" listen-port))
    (format s "~%")
    (dolist (p (coerce peers 'list))
      (let ((addr (hash-get p "addr_v4" :addr_v4))
            (pub  (hash-get p "wg_pub"  :wg_pub))
            (ep   (hash-get p "endpoint" :endpoint))
            (ka   (or (hash-get p "keepalive" :keepalive) 25)))
        (format s "[Peer]~%")
        (format s "PublicKey = ~A~%" pub)
        (when addr (format s "AllowedIPs = ~A~%" addr))
        (when ep   (format s "Endpoint = ~A~%" ep))
        (format s "PersistentKeepalive = ~D~%~%" ka)))))

(defun apply-wg-config (iface cfg-path &key (prefer-syncconf t))
  "If IFACE exists → sudo wg syncconf; else → sudo wg-quick up."
  (labels ((die (code err)
             (error "sudo wg apply failed (exit ~a): ~a" code (string-right-trim '(#\Newline #\Return) err))))
    ;;does interface already exist?
    (multiple-value-bind (o err code) (sudo "wg" "show" iface)
      (declare (ignore o err))
      (cond
        ;; exists → syncconf for zero-bounce updates
        ((and (zerop code) prefer-syncconf)
         (multiple-value-bind (o2 err2 code2)
             (sudo "wg" "syncconf" iface (namestring cfg-path))
           (declare (ignore o2))
           (unless (zerop code2) (die code2 err2))))
        ;; missing → bring it up (sets Address/ListenPort/routes from the conf)
        (t
         (multiple-value-bind (o3 err3 code3)
             (sudo "wg-quick" "up" (namestring cfg-path))
           (declare (ignore o3))
           (unless (zerop code3) (die code3 err3))))))))

