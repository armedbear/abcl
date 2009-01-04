;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; $Id$

(require 'asdf)
(defpackage :abcl-asdf
  (:use :cl :asdf))
(in-package :abcl-asdf)

(defsystem :abcl
  :documentation "Wrapper for all ABCL ASDF definitions."
  :version "0.2.0")

(defmethod perform :around ((o load-op) (c (eql (find-system 'abcl))))
  (call-next-method)
  (format t "DEBUG: load-op around :abcl.~%")
  (asdf:oos 'asdf:load-op :test-abcl))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl))))
  (format t "DEBUG: test-op :abcl.~%")
  (asdf:oos 'asdf:load-op :test-abcl :force t)
  (asdf:oos 'asdf:test-op :ansi-test-compiled :force t))

(defsystem :test-abcl
  :documentation "A collection of test suites for ABCL." 
  :version "0.3"
  :depends-on (:ansi-test-compiled :ansi-test-interpreted))

(defmethod perform :after ((o test-op) (c (eql (find-system 'test-abcl))))
  (asdf:oos 'asdf:load-op :ansi-test-interpreted :force t)
  (asdf:oos 'asdf:load-op :ansi-test-compiled :force t))

(defsystem :ansi-test :version "0.1" :components
     ((:module ansi-tests :pathname "test/lisp/ansi/"
	       :documentation "GCL ANSI test suite."
	       :components
	      ((:file "package")))))
(defsystem :ansi-test-interpreted :version "0,1" :depends-on (ansi-test))
(defsystem :ansi-test-compiled :version "0.1" :depends-on (ansi-test))

(defmethod perform ((o test-op) (c (eql (find-system 'ansi-test-interpreted))))
  (funcall (intern (symbol-name 'run) :abcl.tests.ansi-tests)
	   :compile-tests nil))

(defmethod perform ((o test-op) (c (eql (find-system 'ansi-test-compiled))))
  (funcall (intern (symbol-name 'run) :abcl.tests.ansi-tests)
	   :compile-tests t))

;;; Works for: abcl, sbcl, clisp
(defsystem :build-abcl 
  :documentation "Build ABCL from a Lisp."
  :components 
	   ((:module build :pathname ""  :components
		     ((:file "build-abcl") 
		      (:file "customizations" :depends-on ("build-abcl"))))))



