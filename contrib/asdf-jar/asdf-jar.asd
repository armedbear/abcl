;;;; -*- Mode: LISP -*-
(in-package :asdf)                      

(defsystem :asdf-jar
  :author "Mark Evenson"
  :version "0.3.2"
  :description "<> asdf:defsystem <urn:abcl.org/release/1.4./contrib/asdf-jar#0.3.2>"
  :components 
  ((:module base :pathname "" :components
	    ((:file "asdf-jar")
             (:static-file "README.markdown")))))
