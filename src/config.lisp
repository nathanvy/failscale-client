(in-package #:failscale/client)

(defparameter *config* (make-hash-table :test #'equal))

(defun cfg (k) (gethash k *config*))

(defun load-config (&key (path "etc/client.sexp"))
  (let* ((sexp (with-open-file (in path) (read in nil nil)))
         (tab (make-hash-table :test #'equal)))
    (dolist (pair sexp) (setf (gethash (car pair) tab) (cdr pair)))
    (setf *config* tab)))

(defun reload-config () (load-config :path (or (cfg :config-path) "etc/client.sexp")))
