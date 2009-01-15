;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; $Id$

(require 'asdf)
(defpackage :abcl-asdf
  (:use :cl :asdf))
(in-package :abcl-asdf)

;;; Wrapper for all ABCL ASDF definitions.
(defsystem :abcl
  :version "0.2.0")

(defmethod perform :after ((o load-op) (c (eql (find-system 'abcl))))
  ;;; Additional test suite loads would go here.
  (asdf:oos 'asdf:load-op :test-abcl :force t))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl))))
  ;;; Additional test suite invocations would go here.
  (asdf:oos 'asdf:test-op :ansi-test-compiled :force t))

;;; A collection of test suites for ABCL.
(defsystem :test-abcl

  :version "0.3"
  :depends-on (:ansi-test-compiled :ansi-test-interpreted))

(defmethod perform :after ((o test-op) (c (eql (find-system 'test-abcl))))
  (asdf:oos 'asdf:load-op :ansi-test-interpreted :force t)
  (asdf:oos 'asdf:load-op :ansi-test-compiled :force t))

(defsystem :ansi-test :version "0.1" :components
     ;;; GCL ANSI test suite.
     ((:module ansi-tests :pathname "test/lisp/ansi/" :components
	       ((:file "package")))))
(defsystem :ansi-test-interpreted :version "0,1" :depends-on (ansi-test))
(defsystem :ansi-test-compiled :version "0.1" :depends-on (ansi-test))

(defmethod perform ((o test-op) (c (eql (find-system 'ansi-test-interpreted))))
  (funcall (intern (symbol-name 'run) :abcl.tests.ansi-tests)
	   :compile-tests nil))

(defmethod perform ((o test-op) (c (eql (find-system 'ansi-test-compiled))))
  (funcall (intern (symbol-name 'run) :abcl.tests.ansi-tests)
	   :compile-tests t))

;;; Build ABCL from a Lisp.
;;; Works for: abcl, sbcl, clisp, cmu, lispworks, allegro, openmcl
(defsystem :build-abcl 
  :components 
	   ((:module build :pathname ""  :components
		     ((:file "build-abcl") 
		      (:file "customizations" :depends-on ("build-abcl"))))))



