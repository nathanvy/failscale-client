(asdf:defsystem "failscale-client"
  :description "Failscale client daemon"
  :depends-on (#:dexador #:shasht #:ironclad #:cl-base64 #:local-time #:uiop #:bordeaux-threads)
  :components ((:file "src/package")
               (:file "src/util")
               (:file "src/config")
               (:file "src/sign")
               (:file "src/http")
               (:file "src/wg")
               (:file "src/main")))
