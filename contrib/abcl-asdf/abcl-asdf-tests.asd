;;;; -*- Mode: LISP -*-

(defsystem abcl-asdf-tests
  :author "Mark Evenson"
  :long-description "<urn:abcl.org/release/1.6.0/contrib/abcl-asdf/test#>"
  :version "2.1.0"
  :defsystem-depends-on (prove-asdf)
  :depends-on (abcl-asdf 
               prove)
  :components ((:module tests
                :pathname "t/" 
                :components ((:test-file "log4j")
                             (:test-file "resolve")
                             (:test-file "resolve-multiple-maven-dependencies")
                             (:test-file "maven"))))
  :perform (asdf:test-op (op c)
              (uiop:symbol-call :prove-asdf 'run-test-system c)))



