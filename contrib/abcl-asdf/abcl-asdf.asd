;;;; -*- Mode: LISP -*-
(in-package :asdf)

(defsystem :abcl-asdf
  :author "Mark Evenson"
  :version "0.2.0"
  :depends-on ("jss") ;;; XXX move the JSS ASDf defintions here? uggh.
  :components 
  ((:module base :pathname "" :components
	    ((:file "abcl-asdf")
             (:file "maven-embedder" :depends-on ("abcl-asdf"))))))
