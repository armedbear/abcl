;;;; -*- Mode: LISP -*-
(in-package :cl-user)

(asdf:defsystem :abcl-introspect
  :author "Alan Ruttenberg"
  :version "1.0.0"
  :description "Introspection on compiled function to aid source location other debugging functions."
  :depends-on (jss)
  :components ((:file "abcl-introspect")))
