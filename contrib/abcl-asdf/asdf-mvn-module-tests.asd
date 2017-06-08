;;;; -*- Mode: LISP -*-

(defsystem asdf-mvn-module-tests
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove asdf-mvn-module)
  :components ((:module tests :pathname "t/"
                        :components ((:file "mvn-module"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))


