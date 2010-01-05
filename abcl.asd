;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; $Id$

(require 'asdf)
(defpackage :abcl-asdf
  (:use :cl :asdf))
(in-package :abcl-asdf)

;;; Wrapper for all ABCL ASDF definitions.
(defsystem :abcl :version "0.3.1")

(defmethod perform :after ((o load-op) (c (eql (find-system :abcl))))
  ;;; Additional test suite loads would go here.
  (operate 'load-op :test-abcl :force t))

(defmethod perform ((o test-op) (c (eql (find-system :abcl))))
  ;;; Additional test suite invocations would go here.
  (operate 'test-op :ansi-compiled :force t))

;;; A collection of test suites for ABCL.
(defsystem :test-abcl
  :version "0.3.1"
  :depends-on (:ansi-compiled #+nil :abcl-tests))

(defmethod perform :after ((o load-op) (c (eql (find-system :abcl))))
  (operate 'load-op :cl-bench :force t)
  (operate 'load-op :abcl-test-lisp :force t)
  (operate 'load-op :ansi-compiled :force t)
  (operate 'load-op :ansi-interpreted :force t))

#+nil
(defmethod perform :before ((o load-op) (c t))
  (warn "ASDF load-op class is ~A" c))

(defsystem :ansi-test :version "1.0" :components
     ;;; GCL ANSI test suite.
     ((:module ansi-tests :pathname "test/lisp/ansi/" :components
	       ((:file "package")))))

(defsystem :ansi-interpreted :version "1.0" :depends-on (ansi-test))
(defmethod perform ((o test-op) (c (eql (find-system :ansi-interpreted))))
   "Invoke tests with:  (asdf:oos 'asdf:test-op :ansi-interpreted :force t)."
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests nil))
(defmethod perform :before ((o test-op) (c (eql (find-system
                                                 :ansi-interpreted))))
  (operate 'load-op :ansi-interpreted :force t))

(defsystem :ansi-compiled :version "1.0" :depends-on (ansi-test))
(defmethod perform ((o test-op) (c (eql (find-system :ansi-compiled))))
  "Invoke tests with:  (asdf:oos 'asdf:test-op :abcl-compiled :force t)."
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests t))
(defmethod perform :before ((o test-op) (c (eql (find-system
                                                 :ansi-compiled))))
  (operate 'load-op :ansi-compiled :force t))

(defsystem :abcl-test-lisp :version "1.1" :components
	   ((:module abcl-rt :pathname "test/lisp/abcl/" :serial t :components
		     ((:file "rt-package") (:file "rt")))
	    (:module package  :depends-on (abcl-rt)
		     :pathname "test/lisp/abcl/" :components
		     ((:file "package")))))

(defmethod perform :before ((o test-op) (c (eql (find-system
                                                 :abcl-test-lisp))))
  (operate 'load-op :abcl-test-lisp :force t))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl-test-lisp))))
   "Invoke tests with (asdf:oos 'asdf:test-op :abcl-test-lisp)."
   (funcall (intern (symbol-name 'run) :abcl-test)))

(defsystem :cl-bench :components
  ((:module cl-bench-wrapper :pathname "test/lisp/cl-bench/" :components 
            ((:file "wrapper")))))

(defmethod perform :before ((o test-op) (c (eql (find-system :cl-bench))))
  (operate 'load-op :cl-bench :force t))

(defmethod perform ((o test-op) (c (eql (find-system :cl-bench))))
  (funcall (intern (symbol-name 'run) :abcl.test.cl-bench)))
 
;;; Build ABCL from a Lisp.
;;; aka the "Lisp-hosted build system"
;;; Works for: abcl, sbcl, clisp, cmu, lispworks, allegro, openmcl
(defsystem :build-abcl :components 
	   ((:module build :pathname ""  :components
		     ((:file "build-abcl") 
		      (:file "customizations" :depends-on ("build-abcl"))))))



