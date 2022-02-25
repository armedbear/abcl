;;;; -*- Mode: LISP -*-
(defsystem abcl-introspect
  :author ("Alan Ruttenberg" "Mark Evenson")
  :description "Introspection on compiled function to aid source location and other debugging functions."
  :long-description "<urn:abcl.org/release/1.8.0/contrib/abcl-introspect#>"
  :version "2.1.0"
  :depends-on (jss)
  :components ((:module package
                :pathname #p"./"
                :components ((:file "packages")))
               (:module source
                :pathname #p"./"
                :components ((:file "abcl-introspect")
	                     (:file "stacktrace")
                             (:file "util"))))
  :in-order-to ((test-op (test-op abcl-introspect-test))))

