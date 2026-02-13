;;;; -*- Mode: LISP -*-
(defsystem asdf-jar
  :author "Mark Evenson"
  :description "Packaging ASDF systems into jar files"
  :long-description "<urn:abcl.org/release/1.10.0/contrib/asdf-jar#>"
  :version "0.4.0"
  :in-order-to ((test-op (test-op :asdf-jar-test)))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module base
                :depends-on (package)
                :pathname "./"
                :components ((:file "asdf-jar")
                             (:static-file "README.markdown")))))

