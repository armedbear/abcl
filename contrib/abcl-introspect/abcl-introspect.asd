;;;; -*- Mode: LISP -*-
(in-package :cl-user)

(asdf:defsystem :abcl-introspect
  :author "Alan Ruttenberg"
  :version "1.0.1"
  :description "Introspection on compiled function to aid source location and other debugging functions."
  :depends-on (jss)
  :components ((:file "abcl-introspect")
	       (:file "stacktrace")))
