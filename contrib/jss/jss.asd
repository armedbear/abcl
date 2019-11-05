;;;; -*- Mode: LISP -*-
(defsystem jss
  :author "Alan Ruttenberg, Mark Evenson"
  :long-description "<urn:abcl.org/release/1.6.0/contrib/jss#>"
  :version "3.6.0" 
  :components ((:module base 
                        :pathname "" :serial t 
                        :components ((:file "packages")
                                     (:file "invoke")
                                     (:file "collections")
                                     (:file "optimize-java-call")
                                     (:file "classpath")
                                     (:file "transform-to-field")
                                     (:file "compat")
                                     (:file "jtypecase")
                                     (:file "util"))))
  :perform (asdf:test-op (op c)
                         (asdf:test-system :jss-tests)))

