;;;; -*- Mode: LISP -*-
(in-package :cl-user)

(asdf:defsystem :abcl-asdf-tests
  :author "Mark Evenson"
  :description "<> asdf:defsystem <urn:abcl.org/release/1.5.0/contrib/abcl-asdf/test#1.6.0>"
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
                                     (:test-file "maven"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))

