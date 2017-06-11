;;;; -*- Mode: LISP -*-

(defsystem abcl-asdf-tests
  :author "Mark Evenson"
  :long-description "<urn:abcl.org/release/1.6.0/contrib/abcl-asdf/test#>"
  :version "2.0.0"
  :defsystem-depends-on (prove-asdf)
  :depends-on (abcl-asdf ;; our dependencies
               ;; Actual testing framework
               prove
               log4j)
  :components ((:module package :pathname "t/"
                        :components ((:file "package")))
               (:module tests :pathname "t/" :serial t
                        :depends-on (package)
                        :components ((:test-file "log4j")
                                     (:test-file "resolve")
                                     (:test-file "resolve-multiple-maven-dependencies")
                                     (:test-file "maven"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))

