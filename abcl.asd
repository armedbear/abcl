;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; $Id$

;;;; To run:
;;;;   
#|
cmd$ abcl 
CL-USER(1): (progn (require 'asdf) 
                   (asdf:oos 'asdf:load-op :abcl) 
                   (asdf:oos 'asdf:test-op :ansi-test-compiled :force t))
|#

(require 'asdf)
(defpackage :abcl-asdf
  (:use :cl :asdf))
(in-package :abcl-asdf)

;;; Wrapper for all ABCL ASDF definitions.
(defsystem :abcl
  :version "0.3.0")

(defmethod perform :after ((o load-op) (c (eql (find-system 'abcl))))
  ;;; Additional test suite loads would go here.
  (asdf:oos 'asdf:load-op :test-abcl :force t))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl))))
  ;;; Additional test suite invocations would go here.
  (asdf:oos 'asdf:test-op :ansi-test-compiled :force t))

;;; A collection of test suites for ABCL.
(defsystem :test-abcl
  :version "0.3"
  :depends-on (:ansi-test-compiled :abcl-tests))

(defmethod perform :after ((o load-op) (c (eql (find-system 'test-abcl))))
  #+nil (asdf:oos 'asdf:test-op :cl-bench :force t)
  #+nil (asdf:oos 'asdf:test-op :abcl-tests :force t)
  #+nil (asdf:oos 'asdf:test-op :ansi-test-interpreted :force t)
  (asdf:oos 'asdf:load-op :ansi-test-compiled :force t))

(defsystem :ansi-test :version "0.1" :components
     ;;; GCL ANSI test suite.
     ((:module ansi-tests :pathname "test/lisp/ansi/" :components
	       ((:file "package")))))
(defsystem :ansi-test-interpreted :version "0,1" :depends-on (ansi-test))
(defsystem :ansi-test-compiled :version "0.1" :depends-on (ansi-test))

(defsystem :abcl-tests
   :version "1.0"
   :components
     ((:module rt :serial t  :pathname "test/lisp/abcl/" :components
 	      ((:file "rt-package") (:file "rt") (:file "test-utilities")))
      (:module tests :depends-on (rt)
 	      :pathname "test/lisp/abcl/" :components
 	      ((:file "compiler-tests") 
 	       (:file "condition-tests")
 	       (:file "file-system-tests") 
 #+nil	       (:file "math-tests")
 	       (:file "java-tests")
 	       (:file "misc-tests")
 	       (:file "pathname-tests")))))
 
 (defmethod perform ((o test-op) (c (eql (find-system 'abcl-tests))))
   "Invoke tests with:  (asdf:oos 'asdf:test-op :abcl-tests :force t)."
   ;;; FIXME needs ASDF:OOS to be invoked with :FORCE t
   (funcall (intern (symbol-name 'do-tests) :test)))
 
(defmethod perform ((o test-op) (c (eql (find-system 'ansi-test-interpreted))))
   "Invoke tests with:  (asdf:oos 'asdf:test-op :abcl-tests :force t)."
   ;;; FIXME needs ASDF:OOS to be invoked with :FORCE t
  (funcall (intern (symbol-name 'run) :abcl.tests.ansi-tests)
	   :compile-tests nil))

(defmethod perform ((o test-op) (c (eql (find-system 'ansi-test-compiled))))
  "Invoke tests with:  (asdf:oos 'asdf:test-op :abcl-test-compiled :force t)."
  (funcall (intern (symbol-name 'run) :abcl.tests.ansi-tests)
	   :compile-tests t))

;;; Build ABCL from a Lisp.
;;; Works for: abcl, sbcl, clisp, cmu, lispworks, allegro, openmcl
(defsystem :build-abcl 
  :components 
	   ((:module build :pathname ""  :components
		     ((:file "build-abcl") 
		      (:file "customizations" :depends-on ("build-abcl"))))))



