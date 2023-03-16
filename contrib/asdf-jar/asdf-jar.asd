;;;; -*- Mode: LISP -*-
(defsystem asdf-jar
  :author "Mark Evenson"
  :description "Packaging ASDF systems into jar files"
  :long-description "<urn:abcl.org/release/1.9.1/contrib/asdf-jar#>"
  :version "0.4.0"
  :in-order-to ((test-op (test-op :asdf-jar/t)))
  :components ((:module package
                :pathname "./"
                :components ((:file "package")))
               (:module base
                :depends-on (package)
                :pathname "./"
                :components ((:file "asdf-jar")
                             (:static-file "README.markdown")))))

(defsystem asdf-jar/t
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove)
  :perform (test-op (o c)
             (uiop:symbol-call :prove-asdf 'run-test-system c))
  :components ((:module test
                :pathname "t/"
                :components ((:test-file "package-jss")))))


