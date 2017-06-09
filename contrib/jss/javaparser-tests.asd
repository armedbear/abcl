(defsystem javaparser-tests
  :defsystem-depends-on (prove-asdf)
  :depends-on (javaparser
               prove)
  :components ((:module tests
                        :pathname "t" 
                        :components ((:test-file "javaparser"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c)))
