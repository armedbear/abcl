;;;; -*- Mode: LISP -*-

;;; XXX 
;;(java:add-to-classpath "~/work/lsw2/lib/jscheme.jar")

(in-package :asdf)

(defsystem :jss
  :author "Alan Ruttenberg"
  :version "2.0" 
  :components 
  ((:module base :pathname "" :serial t 
            :components ((:file "packages")
                         (:file "invoke")
                         (:file "asdf-jar")
                         (:file "compat")))))




   


