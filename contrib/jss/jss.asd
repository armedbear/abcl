;;;; -*- Mode: LISP -*-
(asdf:defsystem :jss
  :author "Alan Ruttenberg, Mark Evenson"
  :version "3.3.0" 
  :description "<> asdf:defsystem <urn:abcl.org/release/1.5.0/contrib/jss#3.3.0>"
  :components ((:module base 
                        :pathname "" :serial t 
                        :components ((:file "packages")
                                     (:file "invoke")
                                     (:file "collections")
				     (:file "optimize-java-call")
                                     (:file "classpath")
                                     (:file "compat")))))





   


