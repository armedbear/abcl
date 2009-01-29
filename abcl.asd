;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; $Id$

(require 'asdf)
(defpackage :abcl-asdf
  (:use :cl :asdf))
(in-package :abcl-asdf)

;;; Wrapper for all ABCL ASDF definitions.
(defsystem :abcl :version "0.3.0")

(defmethod perform :after ((o load-op) (c (eql (find-system 'abcl))))
  ;;; Additional test suite loads would go here.
  (asdf:oos 'asdf:load-op :test-abcl :force t))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl))))
  ;;; Additional test suite invocations would go here.
  (asdf:oos 'asdf:test-op :ansi-compiled :force t))

;;; A collection of test suites for ABCL.
(defsystem :test-abcl
  :version "0.3"
  :depends-on (:ansi-compiled #+nil :abcl-tests))

(defmethod perform :after ((o load-op) (c (eql (find-system 'test-abcl))))
  #+nil (asdf:oos 'asdf:test-op :cl-bench :force t)
  (asdf:oos 'asdf:load-op :abcl-test-lisp :force t)
  (asdf:oos 'asdf:load-op :ansi-compiled :force t)
  (asdf:oos 'asdf:load-op :ansi-interpreted :force t))

(defsystem :ansi-test :version "1.0" :components
     ;;; GCL ANSI test suite.
     ((:module ansi-tests :pathname "test/lisp/ansi/" :components
	       ((:file "package")))))

(defsystem :ansi-interpreted :version "1.0" :depends-on (ansi-test))
(defmethod perform ((o test-op) (c (eql (find-system 'ansi-interpreted))))
   "Invoke tests with:  (asdf:oos 'asdf:test-op :ansi-interpreted :force t)."
   ;;; FIXME needs ASDF:OOS to be invoked with :FORCE t
  (funcall (intern (symbol-name 'run) :ansi.test.ansi)
	   :compile-tests nil))

(defsystem :ansi-compiled :version "1.0" :depends-on (ansi-test))
(defmethod perform ((o test-op) (c (eql (find-system 'ansi-compiled))))
  "Invoke tests with:  (asdf:oos 'asdf:test-op :abcl-compiled :force t)."
  (funcall (intern (symbol-name 'run) :abcl.test.ansi)
	   :compile-tests t))

(defsystem :abcl-test-lisp :version "1.0" :components
	   ((:module package  :pathname "test/lisp/abcl/" :components
		     ((:file "package")))))
(defmethod perform ((o test-op) (c (eql (find-system 'abcl-test-lisp))))
   "Invoke tests with:  (asdf:oos 'asdf:test-op :abcl-tests :force t)."
   ;;; FIXME needs ASDF:OOS to be invoked with :FORCE t
   (funcall (intern (symbol-name 'run) :abcl.test.lisp)))
 
;;; Build ABCL from a Lisp.
;;; aka the "Lisp-hosted build system"
;;; Works for: abcl, sbcl, clisp, cmu, lispworks, allegro, openmcl
(defsystem :build-abcl :components 
	   ((:module build :pathname ""  :components
		     ((:file "build-abcl") 
		      (:file "customizations" :depends-on ("build-abcl"))))))



