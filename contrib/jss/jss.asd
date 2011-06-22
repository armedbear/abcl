;;;; -*- Mode: LISP -*-
(in-package :asdf)

(defsystem :jss
  :author "Alan Ruttenberg, Mark Evenson"
  :version "2.0.1" 
  :components 
  ((:module base :pathname "" :serial t 
            :components ((:file "packages")
                         (:file "invoke")
                         (:file "asdf-jar")
                         (:file "compat")))))




   


