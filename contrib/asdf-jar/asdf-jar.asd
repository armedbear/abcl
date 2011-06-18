;;;; -*- Mode: LISP -*-
(in-package :asdf)                      

(defsystem :asdf-jar
  :author "Mark Evenson"
  :version "0.2.0"
  :components 
  ((:module base :pathname "" :components
	    ((:file "asdf-jar")))))