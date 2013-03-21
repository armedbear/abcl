;;;; -*- Mode: LISP -*-
(in-package :asdf)                      

(defsystem :asdf-jar
  :author "Mark Evenson"
  :version "0.2.1"
  :description "<> asdf:defsystem <urn:abcl.org/release/1.1.0/contrib/asdf-jar#0.2.1>"
  :components 
  ((:module base :pathname "" :components
	    ((:file "asdf-jar")
             (:static-file "README.markdown")))))
