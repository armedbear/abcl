;;;; Run a bisection tool to determine where a test fails
;;; This file is in the public domain.
;;; Copyright (C) 2012 by Mark <evenson.not.org@gmail.com>
(in-package :cl-user)

(defun generate-bisect-wrapper ()
  "Create 'check.sh', a script suitable for use with hg bisect.

  To use, adjust the contents of the *TESTS*

   hg clone https://evenson.not.org@code.google.com/p/abcl-dynamic-install/ ./abcl
&& cd abcl
&& hg bisect --reset && hg bisect --good && hg --command sh ./check.sh
"
  (let ((check.sh #p"check.sh"))
    (unless (probe-file check.sh)
      (with-open-file (output check.sh :direction :output)
        (format output "#!/bin/sh~A~%"
                "ant && ./abcl --noinit --batch --eval \"(load \\\"check.lisp\\\"")))))

;;; XXX separate out runtime yucky top-level forms
(require :asdf)
(require :abcl-contrib)
(require :asdf-install)  ;;; to push "~/.asdf-install-dir/systems/" into ASDF:*CENTRAL-REGISTRY*



;;; The ASDF definition for ANSI-COMPILED contains the ANSI-TESTS package.
;;; The CL-TEST package is defined by the GCL ANSI tests.
(eval-when (:load-toplevel :execute)
  (asdf:load-system :abcl)
  (asdf:load-system :ansi-compiled)
  (ansi-tests:load-tests)) ;; TODO figure out how to not load all the tests

(defparameter *test*
  'CL-TEST::SYNTAX.SHARP-BACKSLASH.7)

(unless (rt:do-test *test*)
  (error "~A failed" *test*))



