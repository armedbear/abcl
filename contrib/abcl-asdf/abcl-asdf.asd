;;;; -*- Mode: LISP -*-
(in-package :asdf)

(defsystem :abcl-asdf
  :author "Mark Evenson"
  :version "0.1.0"
  :components 
  ((:module base :pathname "" :components
	    ((:file "abcl-asdf")))))