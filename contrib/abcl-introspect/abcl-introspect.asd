;;;; -*- Mode: LISP -*-
(in-package :cl-user)

(asdf:defsystem :abcl-introspect
  :author "Alan Ruttenberg"
  :description "Introspection on compiled function to aid source location and other debugging functions."
  :long-description "<urn:abcl.org/release/1.5.0/contrib/abcl-introspect#>"
  :version "1.0.1"
  :depends-on (jss)
  :components ((:file "abcl-introspect")
	       (:file "stacktrace")))
