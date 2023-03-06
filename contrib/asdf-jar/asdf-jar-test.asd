;;;; -*- Mode: LISP -*-
(defsystem asdf-jar-test
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove)
  :perform (test-op (o c)
             (uiop:symbol-call :prove-asdf 'run-test-system c))
  :components ((:module test
                :pathname "t/"
                :components ((:test-file "package-asdf")))))


