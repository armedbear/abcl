(defpackage :abcl.test.cl-bench
  (:use :cl :asdf)
  (:nicknames "cl-bench")
  (:export run))

(in-package :abcl.test.cl-bench)

(defparameter *cl-bench-master-source-location*
  "<http://www.chez.com/emarsden/downloads/cl-bench.tar.gz>")

(defparameter *cl-bench-directory*
  (if (find :asdf2 *features*)
      (asdf:system-relative-pathname 
       :cl-bench "../cl-bench/")
      (merge-pathnames #p"../cl-bench/"
                       (component-pathname (find-system :abcl)))))
  
;;; cl-bench defines BENCH-GC and WITH-SPAWNED-THREAD in
;;; '*cl-bench-directory*/sysdep/setup-ablisp.lisp'.  
(defun cl-bench::bench-gc () (ext:gc))
(defmacro cl-bench::with-spawned-thread (&body body)
  `(progn ,@body))

(defun run ()
  (unless (probe-file *cl-bench-directory*)
    (error "Failed to find the cl-bench test suite in '~A'.~%
Please manually download and extract the cl-bench tool suite~%
from ~A to run the tests."
           *cl-bench-directory*
           *cl-bench-master-source-location*))
  (let ((*default-pathname-defaults* *cl-bench-directory*))
    (if (find :unix *features*)
        (run-shell-command 
         (format nil "cd ~A; make clean optimize-files" *cl-bench-directory*))
        (run-shell-command "cd ~A && make clean optimize-files" *cl-bench-directory*))
    (load "generate.lisp")
    (load "do-compilation-script.lisp")
    (load "do-execute-script.lisp")))
  
