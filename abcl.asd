;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
(defsystem abcl
  :version "1.5.0"
  :in-order-to ((test-op (test-op "abcl/test/lisp"))))

(defsystem abcl/test/lisp
  :version "1.5.0"
  :description "Test ABCL with the its own collection of unit tests."
  :perform  (test-op (o s)
                     (uiop:symbol-call :abcl.test.lisp '#:run))
  :components ((:module abcl-rt 
                        :pathname "test/lisp/abcl/" :serial t :components
                        ((:file "rt-package") 
                         (:file "rt")
                         (:file "test-utilities")))
               (:module package  :depends-on (abcl-rt)
                        :pathname "test/lisp/abcl/" :components
                        ((:file "package")))
               (:module test :depends-on (package)
                        :pathname "test/lisp/abcl/" :components
                        ((:file "utilities")
                         (:file "compiler-tests")
                         (:file "condition-tests")
                         #+abcl
                         (:file "class-file")
                         #+abcl
                         (:file "metaclass")
                         #+abcl
                         (:file "mop-tests-setup")
                         #+abcl
                         (:file "mop-tests"
                                :depends-on ("mop-tests-setup"))
                         (:file "clos-tests")
                         (:file "file-system-tests")
                         #+abcl
                         (:file "jar-pathname"
                                :depends-on ("utilities" "pathname-tests" "file-system-tests"))
                         #+abcl
                         (:file "url-pathname")
                         (:file "math-tests"
                                :depends-on ("compiler-tests"))
                         (:file "misc-tests")
                         (:file "latin1-tests")
                         (:file "bugs" :depends-on 
                                ("file-system-tests"))
                         (:file "wild-pathnames"
                                :depends-on ("file-system-tests"))
                         #+abcl 
                         (:file "weak-hash-tables")
                         #+abcl 
                         (:file "zip")
                         #+abcl 
                         (:file "java")
                         #+abcl
                         (:file "pathname-tests" :depends-on 
                                ("utilities"))
                         #+abcl
                         (:file "runtime-class")
                         #+abcl
                         (:file "package-local-nicknames-tests")))))

;;; FIXME Currently requires ACBL-CONTRIB and QUICKLISP-ABCL to be
;;; loaded, but can't seem to put in the :defsystem-depends-on stanza
(defsystem abcl/t
  :description "Tests for ABCL via PROVE."
  :defsystem-depends-on (prove-asdf)
  :depends-on (abcl
               prove)
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :prove-asdf :run-test-system c))
  :components ((:module package
                        :pathname "t/"
                        :components ((:file "package")))
               (:module java6
                        :depends-on (package)
                        :pathname "t/"
                        :components ((:test-file "run-program")))
               (:module build
                        :depends-on (package)
                        :pathname "t/"
                        :components ((:test-file "resolve-multiple-maven-dependencies")
                                     (:test-file "disassemble")
                                     (:test-file "pathname")))))

;;;
;;; ASDF definitions and the ANSI-TEST suite
;;;
;;; Below refer to the ANSI-TEST source tree, which isn't included as
;;; part of ABCL, but may be obtained at
;;; <git+https://gitlab.common-lisp.net/ansi-test/ansi-test.git>
;;; For the 'abcl/test/ansi/*' definitions to work, we require that
;;; the ANSI-TEST to be in a sibling directory named "ansi-tests"
;;; which should be manually synced with the contents of the SVN
;;; repository listed above.
;;; The ABCL.TEST.ANSI defines a function VERIFY-ANSI-TESTS to check
;;; whether the test suite is present, which provides a more useful
;;; diagnostic, but I can't seem to find a way to hook this into the
;;; ASDF:LOAD-OP phase.
(defsystem abcl/ansi-rt
  :description "Enapsulation of the REGRESSION-TEST framework used by ~
the ANSI test suite, so that we may build on its 'API'.

Requires that the contents of <git+https://gitlab.common-lisp.net/ansi-test/ansi-test.git> ~
be in a directory named '../ansi-test/'."
  :pathname "../ansi-test/" ;;; NB works when loaded from ASDF but not with a naked EVAL
  :default-component-class cl-source-file.lsp
  :components ((:file "rt-package")
               (:file "rt" :depends-on (rt-package))))

(defsystem abcl/test/ansi
  :depends-on (abcl/ansi-rt)
  :components 
  ((:module ansi-tests :pathname "test/lisp/ansi/" :components
            ((:file "packages")
             (:file "abcl-ansi" :depends-on ("packages"))
             (:file "parse-ansi-errors" :depends-on ("abcl-ansi"))))))

(defsystem abcl/test/ansi/interpreted 
  :version "1.2" 
  :description "Test ABCL with the interpreted ANSI tests." 
  :depends-on (abcl/test/ansi)
  :perform (test-op (o s)
                    (uiop:symbol-call :abcl.test.ansi 'run :compile-tests nil)))
  
(defsystem abcl/test/ansi/compiled
  :version "1.2" 
  :description "Test ABCL with the compiled ANSI tests." 
  :depends-on (abcl/test/ansi)
  :perform (test-op (o s)
                    (uiop:symbol-call :abcl.test.ansi 'run :compile-tests t))
  :components ((:module ansi-tests
                        :pathname "test/lisp/ansi/"
                        :components ((:file "packages")
                                     (:file "abcl-ansi"
                                            :depends-on ("packages"))
                                     (:file "parse-ansi-errors"
                                            :depends-on ("abcl-ansi"))))))

(defsystem abcl/test/cl-bench 
  :description "Test ABCL with CL-BENCH."
  :perform (test-op (o s)
                    (uiop:symbol-call :abcl.test.cl-bench 'run))
  :components ((:module package :pathname "../cl-bench/"
                        :components ((:file "defpackage")))
               (:module wrapper :pathname "test/lisp/cl-bench/" 
                        :depends-on (package) :components
                        ((:file "wrapper")))))
(defsystem abcl/documentation
  :description "Tools to generate LaTeX source from docstrings."
  :depends-on (swank)
  :components
  ((:module package
            :pathname "doc/manual/" :components ((:file "package")))
   (:module grovel
            :depends-on (package)
            :pathname "doc/manual/" 
            :components ((:file "index" :depends-on (grovel))
                         (:file "grovel")))))

