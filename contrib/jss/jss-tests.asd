(require :asdf)
(in-package :cl-user)

(asdf:defsystem jss-tests
  :defsystem-depends-on (quicklisp-abcl
                         prove-asdf)
  :depends-on (jss
               prove)
  :components ((:module tests
                        :pathname "" 
                        :components ((:test-file "jss-tests"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))
