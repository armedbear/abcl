(defsystem abcl-build-tests
  :version "2.0.1"
  :description "Test ABCL build system."
  :defsystem-depends-on (prove-asdf)
  :depends-on (abcl-build
               prove)
  :perform (test-op (op c)
                    (symbol-call :prove-asdf 'run-test-system c))
  :components ((:module build
                        :pathname "build/t/"
                        :components ((:test-file "util")
                                     (:test-file "install")
                                     (:test-file "ant")
                                     (:test-file "maven")
                                     (:test-file "abcl-build")))))

                                     


