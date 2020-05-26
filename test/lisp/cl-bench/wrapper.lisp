(defpackage :abcl.test.cl-bench
  (:use :cl :asdf)
  (:nicknames "CL-BENCH")
  (:export #:run))
(in-package :abcl.test.cl-bench)

(defparameter *cl-bench-master-source-location*
  "<git+https://gitlab.common-lisp.net/ansi-test/cl-bench.git>")

;;; Deprecated.  Use ASDF to locate CL-BENCH source
(defparameter *cl-bench-directory*
  (asdf:system-relative-pathname :abcl "../cl-bench/"))
  
;;; cl-bench defines BENCH-GC and WITH-SPAWNED-THREAD in
;;; '*cl-bench-directory*/sysdep/setup-ablisp.lisp'.  
(defun cl-bench::bench-gc () (ext:gc))
(defmacro cl-bench::with-spawned-thread (&body body)
  `(progn ,@body))

(defun add-to-asdf (directory &key (asdf-conf-file "cl-bench.conf"))
  (let* ((source-registry.conf.d
          (merge-pathnames ".config/common-lisp/source-registry.conf.d/"
                           (user-homedir-pathname)))
         (asdf-conf
           (merge-pathnames asdf-conf-file source-registry.conf.d)))
    (unless (probe-file source-registry.conf.d)
      (ensure-directories-exist source-registry.conf.d))
    (when (probe-file asdf-conf)
      (format *standard-output* "Overwriting existing ~a" asdf-conf))
    (with-open-file (o asdf-conf
                       :direction :output :if-exists :supersede)
      (write `(:directory ,directory) :stream o))
    (format *standard-output* "Configured ASDF via ~%~t~a~% to search~%~t'~a'~%"
            asdf-conf directory)))

(defun run ()
  (unless (ignore-errors (asdf:find-system :cl-bench))
    (if (probe-file *cl-bench-directory*)
        (when (probe-file (merge-pathnames "cl-bench.asd" *cl-bench-directory*))
          (add-to-asdf *cl-bench-directory*)
          (asdf/source-registry:initialize-source-registry)
          (unless (ignore-errors (asdf:find-system :cl-bench))
            (error "Failed to configure ASDF to find CL-BENCH in ~a" *cl-bench-directory*)))
        (error "Please download and install a newer version of CL-BENCH containing an ASDF definition in ~a from ~a"
               *cl-bench-directory* *cl-bench-master-source-location*)))
  (ql:quickload :cl-bench)
  (uiop:symbol-call :cl-bench :bench-run))


;;; Deprecated running CL-BENCH without ASDF definition.  
(defun old-run ()  
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
  
