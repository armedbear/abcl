;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
;;; $Id$

(require 'asdf)
(defpackage :abcl-asdf
  (:use :cl :asdf))
(in-package :abcl-asdf)

(defsystem :abcl
  :documentation "Wrapper for all ABCL ASDF definitions."
  :version "0.2.0")

(defmethod perform :after ((o load-op) (c (eql (find-system 'abcl))))
  (asdf:oos 'asdf:load-op :test-abcl))

(defsystem :test-abcl
  :documentation "A collection of test suites for ABCL." 
  :version "0.3"
  :components
     ((:module ansi-tests :pathname "test/lisp/ansi/"
	       :documentation "GCL ANSI test suite"
	       :components
	      ((:file "package")))))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl))))
  "Invoke tests with:  (asdf:oos 'asdf:test-op :test-abcl)."
  (funcall (intern (symbol-name 'run-ansi-tests) 
		   :abcl.tests.ansi-tests)))

;;; Works for: abcl, sbcl, clisp
(defsystem :build-abcl 
  :documentation "Build ABCL from a Lisp."
  :components 
	   ((:module build :pathname ""  :components
		     ((:file "build-abcl") 
		      (:file "customizations" :depends-on ("build-abcl"))))))



