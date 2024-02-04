(in-package #:failscale/client)

(defparameter *thread* nil)

(defun reconcile-wg (resp)
  (let* ((iface (or (cfg :iface) "wg-fs0"))
         (listen (or (cfg :listen-port) 51820))
         (self (hash-get resp "self" :self))
         (self-addr (and self (hash-get self "addr_v4" :addr_v4)))
         (peers (or (hash-get resp "peers" :peers) #())))
    (unless self (error "Server response missing self"))
    (let* ((cfg-path (or (cfg :wg-config-path)
                         (format nil "/etc/wireguard/~A.conf" iface)))
           (text (render-wg-config iface self-addr listen peers)))
      (ensure-dir (uiop:pathname-directory-pathname cfg-path))
      (write-string-file cfg-path text :perm #o600)
      (apply-wg-config iface cfg-path))))

(defun loop-once ()
  (handler-case
      (let ((resp (sync-once)))
        (reconcile-wg resp)
        t)
    (error (e)
      (format *error-output* "~&[failscale-client] error: ~A~%" e)
      nil)))

(defun start (&key (config-path "etc/client.sexp"))
  (load-config :path config-path)
  (when *thread* (return-from start :already-running))
  (let* ((period (or (cfg :sync-interval) 15)))
    (setf *thread*
          (bordeaux-threads:make-thread
           (lambda ()
             (loop
               (loop-once)
               (sleep period)))
           :name "failscale-client")))
  :started)

(defun stop ()
  (when *thread*
    (bordeaux-threads:destroy-thread *thread*)
    (setf *thread* nil)
    :stopped))
