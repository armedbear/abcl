(defpackage #:abcl.test.lisp 
  (:use #:cl #:abcl-rt)
  (:nicknames "ABCL-TEST")
  (:export #:run))
(in-package #:abcl.test.lisp)

(defvar *abcl-lisp-test-directory* 
  (pathname (directory-namestring *load-truename*))
  "The directory in which the ABCL test source files are located.")

(defun run ()
  "Run the Lisp test suite for ABCL."

  (let ((*default-pathname-defaults* *abcl-lisp-test-directory*))
    (rem-all-tests)

    (load "test-utilities.lisp")

    (load "compiler-tests.lisp")
    (load "condition-tests.lisp")
    (load "file-system-tests.lisp")
    (load "java-tests.lisp")
    (load "math-tests.lisp")
    (load "misc-tests.lisp")

    (when (find :unix *features*)
      (load "jar-file.lisp"))

    (do-tests)))

	