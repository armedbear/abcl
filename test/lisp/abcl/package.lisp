(defpackage #:abcl.test.lisp 
  (:use #:cl #:abcl-rt)
  (:export #:run))
(in-package #:abcl.test.lisp)

(defparameter *abcl-lisp-test-pathname* nil)

(eval-when (:load-toplevel)
  (setf *abcl-lisp-test-pathname* *load-truename*))

(defun run ()
  (progv 
      '(*default-pathname-defaults*)
      `(,(merge-pathnames *abcl-lisp-test-pathname* *default-pathname-defaults*))
    (rem-all-tests)

    (load "test-utilities.lisp")

    (load "compiler-tests.lisp")
    (load "condition-tests.lisp")
    (load "file-system-tests.lisp")
    (load "java-tests.lisp")
    (load "math-tests.lisp")
    (load "misc-tests.lisp")

    (do-tests)))



   


	