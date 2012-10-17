;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; $Id$

(require 'asdf)
(in-package :asdf)

;;; Wrapper for all ABCL ASDF definitions.
(defsystem :abcl :version "0.6.0")

;;;  Run via (asdf:operate 'asdf:test-op :abcl :force t)
(defmethod perform ((o test-op) (c (eql (find-system :abcl))))
  (load-system (find-system :abcl-test-lisp))
  (operate 'test-op :abcl-test-lisp))

;;; Test ABCL with the Lisp unit tests collected in "test/lisp/abcl"
;;;
;;; We guard with #+abcl for tests that other Lisps cannot load.  This
;;; could be possibly be done at finer granularity in the files
;;; themselves.
(defsystem :abcl-test-lisp :version "1.2" :components
	   ((:module abcl-rt 
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
                      (:file "mop-tests" :depends-on 
                             ("mop-tests-setup"))
                      (:file "clos-tests")
                      (:file "file-system-tests")
                      #+abcl
                      (:file "jar-pathname" :depends-on 
                             ("utilities" "pathname-tests" "file-system-tests"))
                      #+abcl
                      (:file "url-pathname")
                      (:file "math-tests" :depends-on 
                             ("compiler-tests"))
                      (:file "misc-tests")
                      (:file "latin1-tests")
                      (:file "bugs" :depends-on 
                             ("file-system-tests"))
                      (:file "wild-pathnames" :depends-on 
                             ("file-system-tests"))
                      #+abcl 
                      (:file "weak-hash-tables")
                      #+abcl 
                      (:file "zip")
                      #+abcl
                      (:file "pathname-tests" :depends-on 
                             ("utilities"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl-test-lisp))))
   "Invoke tests with (asdf:oos 'asdf:test-op :abcl-test-lisp)."
   (funcall (intern (symbol-name 'run) :abcl.test.lisp)))

;;;;
;;;; ASDF definitions and the ANSI-TEST
;;;;

;;; We refer to the ANSI-TESTS source tree, which isn't shipped as
;;; part of ABCL, but may be obtained at 
;;; <svn://common-lisp.net/project/ansi-test/svn/trunk/ansi-tests>.

;;; We currently require that the ANSI-TESTS to be in a sibling
;;; directory named "ansi-tests" which should be manually synced with
;;; the contents of the SVN repository listed above.

;;; The ASDF definition for ABCL.TEST.ANSI defines VERIFY-ANSI-TESTS
;;; which provides a more useful diagnostic, but I can't seem to find
;;; a way to hook this into the ASDF:LOAD-OP phase
(defsystem :ansi-rt
  :description "Enapsulation of the REGRESSION-TEST framework use by ~
the ANSI test suite, so that we may build on its 'API'.

Requires that the contents of <svn://common-lisp.net/project/ansi-test/svn/trunk/ansi-tests> ~
be in a directory named '../ansi-test/'."
  :pathname "../ansi-tests/" ;;; NB works when loaded from ASDF but not with a naked EVAL
  :default-component-class cl-source-file.lsp
  :components ((:file "rt-package")
               (:file "rt" :depends-on (rt-package))))

(defsystem :ansi-interpreted 
  :version "1.2" 
  :description "Test ABCL with the interpreted ANSI tests." 
  :depends-on (ansi-rt) :components 
  ((:module ansi-tests :pathname "test/lisp/ansi/" :components
            ((:file "packages")
             (:file "abcl-ansi" :depends-on ("packages"))
             (:file "parse-ansi-errors" :depends-on ("abcl-ansi"))))))
(defmethod perform :before ((o test-op) (c (eql (find-system :ansi-interpreted))))
  (load-system :ansi-interpreted))

(defmethod perform :after ((o load-op) (c (eql (find-system :ansi-interpreted))))
  (funcall (intern (symbol-name 'load-tests) :abcl.test.ansi)))

(defmethod perform ((o test-op) (c (eql (find-system :ansi-interpreted))))
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests nil))

(defsystem :ansi-compiled :version "1.2" 
           :description "Test ABCL with the compiled ANSI tests." 
           :depends-on (ansi-rt)
           :components 
           ((:module ansi-tests :pathname "test/lisp/ansi/" :components
                     ((:file "packages")
                      (:file "abcl-ansi" :depends-on ("packages"))
                      (:file "parse-ansi-errors" :depends-on ("abcl-ansi"))))))

(defmethod perform :before ((o test-op) (c (eql (find-system :ansi-compiled))))
  (load-system :ansi-compiled))
(defmethod perform ((o test-op) (c (eql (find-system :ansi-compiled))))
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests t))

(defsystem :cl-bench 
  :description "Test ABCL with CL-BENCH."
  :components ((:module cl-bench-package :pathname "../cl-bench/"
                        :components ((:file "defpackage")))
               (:module cl-bench-wrapper :pathname "test/lisp/cl-bench/" 
                        :depends-on (cl-bench-package) :components
                        ((:file "wrapper")))))
(defmethod perform :before ((o test-op) (c (eql (find-system :cl-bench))))
  (load-system :cl-bench))
(defmethod perform ((o test-op) (c (eql (find-system :cl-bench))))
  (funcall (intern (symbol-name 'run) :abcl.test.cl-bench)))
 
;;; Build ABCL from a Lisp.
;;; aka the "Lisp-hosted build system"
;;; Works for: abcl, sbcl, clisp, cmu, lispworks, allegro, openmcl
(defsystem :build-abcl :components 
	   ((:module build :pathname ""  :components
		     ((:file "build-abcl") 
		      (:file "customizations" :depends-on ("build-abcl"))))))

(defsystem :abcl-contrib
  ;; :version "1.1"
  :components ((:static-file "README")))
  ;; #+nil ((:module source :pathname "src/org/armedbear/lisp/" :components 
  ;;                       ((:file  "abcl-contrib")
  ;;                        #+nil::needs-abcl-asdf (:iri "jar-file:dist/abcl-contrib.jar"))))

;; XXX Currently need to force load via (asdf:load-system :abcl-contrib :force t)
(defmethod perform ((o load-op) (c (eql (find-system :abcl-contrib))))
 (require :abcl-contrib))

