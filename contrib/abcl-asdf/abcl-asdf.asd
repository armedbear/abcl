;;;; -*- Mode: LISP -*-
(in-package :asdf)

(defsystem :abcl-asdf
  :author "Mark Evenson"
  :version "0.2.0"
  :components 
  ((:module base :pathname "" :components
	    ((:file "abcl-asdf")
             (:file "maven-embedder" :depends-on ("abcl-asdf"))))))
