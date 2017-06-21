;;;; -*- Mode: LISP -*-
(defsystem jss-tests
  :defsystem-depends-on (quicklisp-abcl
                         prove-asdf)
  :depends-on (jss
               prove)
  :components ((:module tests
                        :pathname "t" 
                        :components ((:test-file "jss-tests")
                                     (:test-file "collections"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))
