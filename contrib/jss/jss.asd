;;;; -*- Mode: LISP -*-
(asdf:defsystem :jss
  :author "Alan Ruttenberg, Mark Evenson"
  :version "3.0.4" 
  :components 
  ((:module base 
            :pathname "" :serial t 
            :components ((:file "packages")
                         (:file "invoke")
                         (:file "classpath")
                         (:file "compat")))))




   


