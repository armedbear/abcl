;;;; -*- Mode: LISP -*-
(defsystem abcl-introspect
  :author ("Alan Ruttenberg" "Mark Evenson")
  :description "Introspection on compiled function to aid source location and other debugging functions."
  :long-description "<urn:abcl.org/release/1.5.0/contrib/abcl-introspect#>"
  :version "2.0.0"
  :depends-on (jss)
  :components ((:file "abcl-introspect")
	       (:file "stacktrace"))
  :in-order-to ((test-op (test-op abcl-introspect-tests))))

