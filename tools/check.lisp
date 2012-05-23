;;;; Run a bisection tool to determine where a test fails
;;; This file is in the public domain.
;;; Copyright (C) 2012 by Mark <evenson.not.org@gmail.com>
(in-package :cl-user)

(defun generate-bisect-wrapper ()
  "Create 'check.sh', a script suitable for use with hg bisect.

To use, first clone 

   hg clone https://evenson.not.org@code.google.com/p/abcl-dynamic-install/ ./abcl
   cd abcl

Then copy 'check.lisp' to this directory, as well as the bisect
wrapper script 'check.sh'.  Adjust 'check.lisp' to raise an error if
the problem exists in a given changeset.

Then reset the hg bisection data via:

   hg bisect --reset

Mark the last known good and earliest known bad changeset via

   hg bisect --good <revision>
   hg bisect --bad <revision>

Then issue

   hg bisect --command sh ./check.sh

which will churn through the revisions until it finds the earliest
known version in which the 'check.lisp' raises the error.
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



