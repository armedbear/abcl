;;;; -*- Mode: LISP -*-
(in-package :Asdf)

(defsystem :asdf-jar
  :author "Mark Evenson"
  :version "0.1.0"
  :components 
  ((:module base :pathname "" :components
	    ((:file "asdf-jar")))))