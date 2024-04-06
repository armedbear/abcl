;;;; -*- Mode: LISP -*-

(defsystem abcl-introspect-test
  :author "Mark Evenson"
  :long-description "<urn:abcl.org/release/1.10.0/contrib/abcl-introspect/test#>"
  :version "2.1.0"
  :defsystem-depends-on (prove-asdf)
  :depends-on (abcl-asdf ;; locate various testing dependencies via ABCL-ASDF
               prove)
  :components ((:module tests
                :pathname "t/"
                :components ((:test-file "disassemble")
                             (:test-file "environments"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))




