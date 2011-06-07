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
                      #+abcl
                      (:file "bugs" :depends-on 
                             ("file-system-tests"))
                      (:file "wild-pathnames" :depends-on 
                             ("file-system-tests"))
                      #+abcl 
                      (:file "weak-hash-tables")
                      #+abcl
                      (:file "pathname-tests" :depends-on 
                             ("utilities"))))))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl-test-lisp))))
   "Invoke tests with (asdf:oos 'asdf:test-op :abcl-test-lisp)."
   (funcall (intern (symbol-name 'run) :abcl.test.lisp)))

;;; Test ABCL with the interpreted ANSI tests
(defsystem :ansi-interpreted :version "1.1" 
           :components
           ((:module ansi-tests :pathname "test/lisp/ansi/" :components
	       ((:file "package")
                (:file "parse-ansi-errors" :depends-on ("package"))))))
(defmethod perform :before ((o test-op) (c (eql (find-system :ansi-interpreted))))
  (load-system  :ansi-interpreted))
(defmethod perform ((o test-op) (c (eql (find-system :ansi-interpreted))))
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests nil))

;;; Test ABCL with the compiled ANSI tests
(defsystem :ansi-compiled :version "1.1" 
           :components
           ((:module ansi-tests :pathname "test/lisp/ansi/" :components
	       ((:file "package")
                (:file "parse-ansi-errors" :depends-on ("package"))))))
(defmethod perform :before ((o test-op) (c (eql (find-system :ansi-compiled))))
  (load-system :ansi-compiled))
(defmethod perform ((o test-op) (c (eql (find-system :ansi-compiled))))
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests t))

;;; Test ABCL with CL-BENCH 
(defsystem :cl-bench :components
           ((:module cl-bench-package :pathname "../cl-bench/"
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



