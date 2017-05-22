;;; aka the "Lisp-hosted build system" which doesn't share build
;;; instructions with the canonical build system in <file:build.xml>
;;; Works for: abcl, sbcl, clisp, cmu, lispworks, allegro, openmcl
(defsystem abcl-build
  :version "2.0.0"
  :description "Build ABCL from a Lisp.  Downloads necessary build-time tools to local cache."
  :in-order-to ((test-op (test-op abcl-build/t)))
  :components ((:module package
                        :pathname "build/"
                        :components ((:file "package")))
               (:module util
                        :pathname "build/"
                        :depends-on (package)
                        :components ((:file "util")))
               (:module build
                        :pathname "build/"
                        :depends-on (util)
                        :serial t 
                        :components (;;; TODO optionally parse a local configuration for customization
                                     (:file "customizations-default")
                                     (:file "install")
                                     (:file "maven")
                                     (:file "ant")
                                     (:file "abcl-build") ;; TODO: support API 
                                     (:file "deprecated")))))

(defsystem abcl-build/t
  :description "Test ABCL build system."
  :defsystem-depends-on (prove-asdf)
  :depends-on (abcl-build
               prove)
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf 'run-test-system c))
  :components ((:module package
                        :pathname "build/t/"
                        :components ((:file "package")))
               (:module build
                        :depends-on (package)
                        :pathname "build/t/"
                        :components ((:test-file "util")
                                     (:test-file "install")
                                     (:test-file "ant")
                                     (:test-file "maven")
                                     (:test-file "abcl-build")))))

                                     


