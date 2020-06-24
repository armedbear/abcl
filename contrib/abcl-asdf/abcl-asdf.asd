;;;; -*- Mode: LISP -*-
(defsystem abcl-asdf
  :author "Mark Evenson"
  :description "Extensions to ASDF for handling Java binary artifacts."
  :long-description "<urn:abcl.org/release/1.7.0/contrib/abcl-asdf#>"
  :version "2.1.0"
  :depends-on (jss abcl-build)
  :components 
  ((:module package
            :pathname "" 
            :components ((:file "package")))
   (:module base
            :pathname "" 
            :components ((:file "abcl-asdf")
                         (:file "asdf-jar" :depends-on ("abcl-asdf")))
            :depends-on (package maven))
   (:module maven
            :pathname "" 
            :components ((:file "maven")
                         (:file "mvn-module"))
    :depends-on (base))
   (:module osgi
            :pathname "" 
            :components ((:file "asdf-osgi-bundle"))
    :depends-on (base)))
  :in-order-to ((test-op (test-op abcl-asdf-tests))))

