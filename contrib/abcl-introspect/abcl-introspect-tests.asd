;;;; -*- Mode: LISP -*-

(defsystem abcl-introspect-tests
  :author "Mark Evenson"
  :long-description "<urn:abcl.org/release/1.5.0/contrib/abcl-introspect/test#>"
  :version "2.0.0"
  :defsystem-depends-on (prove-asdf)
  :depends-on (abcl-asdf ;; our dependencies
               prove)
  :components ((:module tests
                        :pathname "t/"
                        :components ((:test-file "disassemble"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))




