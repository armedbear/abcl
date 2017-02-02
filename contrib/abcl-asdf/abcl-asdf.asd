;;;; -*- Mode: LISP -*-
(in-package :cl-user)

(asdf:defsystem :abcl-asdf
  :author "Mark Evenson"
  :description "Extensions to ASDF for handling Java binary artifacts."
  :long-description "<urn:abcl.org/release/1.5.0/contrib/abcl-asdf#>"
  :version "1.6.1"
  :depends-on (jss)
  :components 
  ((:module package :pathname "" 
            :components
            ((:file "package")))
   (:module base :pathname "" 
            :components
            ((:file "abcl-asdf")
             (:file "asdf-jar" 
                    :depends-on ("abcl-asdf"))
             (:file "maven-embedder" 
                    :depends-on ("abcl-asdf" "asdf-jar")))
            :depends-on (package)))
  :in-order-to ((asdf:test-op (asdf:test-op abcl-asdf-tests))))

