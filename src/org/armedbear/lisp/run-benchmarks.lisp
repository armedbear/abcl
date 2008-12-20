;; Driver for Eric Marsden's CL-BENCH Lisp performance benchmarks.

(in-package :cl-user)

#+armedbear
(require 'pprint)

#+allegro
(progn
  (setq excl:*record-source-file-info* nil)
  (setq excl:*load-source-file-info* nil)
  (setq excl:*record-xref-info* nil)
  (setq excl:*load-xref-info* nil))

(setf *default-pathname-defaults* #p"/home/peter/cl-bench/")

(load #p"defpackage.lisp")
(compile-file #p"files/arrays.olisp")
(compile-file #p"files/bignum.olisp")
(compile-file #p"files/boehm-gc.olisp")
(compile-file #p"files/clos.olisp")
(compile-file #p"files/crc40.olisp")
(compile-file #p"files/deflate.olisp")
(compile-file #p"files/gabriel.olisp")
(compile-file #p"files/hash.olisp")
(compile-file #p"files/math.olisp")
(compile-file #p"files/ratios.olisp")
(compile-file #p"files/richards.olisp")
(compile-file #p"files/misc.olisp")

(load (compile-file-pathname #p"files/arrays.olisp"))
(load (compile-file-pathname #p"files/bignum.olisp"))
(load (compile-file-pathname #p"files/boehm-gc.olisp"))
(load (compile-file-pathname #p"files/clos.olisp"))
(load (compile-file-pathname #p"files/crc40.olisp"))
(load (compile-file-pathname #p"files/deflate.olisp"))
(load (compile-file-pathname #p"files/gabriel.olisp"))
(load (compile-file-pathname #p"files/hash.olisp"))
(load (compile-file-pathname #p"files/math.olisp"))
(load (compile-file-pathname #p"files/ratios.olisp"))
(load (compile-file-pathname #p"files/richards.olisp"))
(load (compile-file-pathname #p"files/misc.olisp"))
(compile-file #p"support.lisp")
(load (compile-file-pathname #p"support.lisp"))

(in-package :cl-bench)

(export '(run-benchmark run-benchmarks))

(setf *benchmark-output* t)

#+(or armedbear clisp)
(defun bench-gc ()
  (ext:gc))

#+sbcl
(defun bench-gc ()
  (sb-ext:gc #+gencgc :full #+gencgc t))

#+allegro
(defun bench-gc ()
  (excl:gc))

(defun report-filename ()
  (let ((impl ""))
    #+allegro   (setf impl "-allegro")
    #+armedbear (setf impl "-armedbear")
    #+clisp     (setf impl "-clisp")
    #+sbcl      (setf impl "-sbcl")
    (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
      (format nil "~abenchmark-~d~2,'0d~2,'0dT~2,'0d~2,'0d~a"
              #+win32 "" #-win32 "/var/tmp/"
              year month day hour min impl))))

(defun run-benchmark (function &optional args (times 1))
  (let ((name (symbol-name function)))
    (format t "Running benchmark ~A" (symbol-name function))
    (when (> times 1)
      (format t " (~D runs)" times))
    (terpri)
    (force-output)
    (let (before-real after-real before-user after-user)
      (setf before-real (get-internal-real-time))
      (setf before-user (get-internal-run-time))
      (dotimes (i times)
        (apply function args))
      (setf after-user (get-internal-run-time))
      (setf after-real (get-internal-real-time))
      (let ((real (/ (- after-real before-real) internal-time-units-per-second))
            (user (/ (- after-user before-user) internal-time-units-per-second)))
        (format *benchmark-output*
                ";; ~25a ~8,2f ~8,2f~%"
                name real user)
        (format *trace-output*
                ";; ~25a ~8,2f ~8,2f~%"
                name real user))
      (force-output *benchmark-output*)))
  (bench-gc)
  (values))

(defun run-benchmarks ()
  (with-open-file (f (report-filename)
                     :direction :output
                     :if-exists :supersede)
    (let ((*benchmark-output* f))
      (format *benchmark-output* "~A ~A "
              (lisp-implementation-type) (lisp-implementation-version))
      (multiple-value-bind (second minute hour date month year)
        (get-decoded-time)
        (format *benchmark-output* "~d-~2,'0d-~2,'0d ~2,'0d:~2,'0d~%"
                year month date hour minute))
      (format *benchmark-output* "~a~%" (short-site-name))
      (force-output *benchmark-output*)
      (bench-gc)
      ;; The benchmarks.
      #+nil
      (run-benchmark 'cl-bench.misc:run-compiler nil 3)
      #+nil
      (run-benchmark 'cl-bench.misc:run-fasload nil 20)
      #-allegro
      (run-benchmark 'cl-bench.misc:run-permutations nil 2)
      #+nil
      (progn
        (cl-bench.misc::setup-walk-list/seq)
        (run-benchmark 'cl-bench.misc:walk-list/seq)
        (setf cl-bench.misc::*big-seq-list* nil)
        (bench-gc))
      #+nil
      (progn
        (cl-bench.misc::setup-walk-list/mess)
        (run-benchmark 'cl-bench.misc:walk-list/mess)
        (setf cl-bench.misc::*big-mess-list* nil)
        (bench-gc))
      (run-benchmark 'cl-bench.gabriel:boyer nil 30)
      (run-benchmark 'cl-bench.gabriel:browse nil 10)
      (run-benchmark 'cl-bench.gabriel:dderiv-run nil 50)
      (run-benchmark 'cl-bench.gabriel:deriv-run nil 60)
      (run-benchmark 'cl-bench.gabriel:run-destructive nil 100)
      (run-benchmark 'cl-bench.gabriel:run-div2-test1 nil 200)
      (run-benchmark 'cl-bench.gabriel:run-div2-test2 nil 200)
      (run-benchmark 'cl-bench.gabriel:run-fft nil 30)
      (run-benchmark 'cl-bench.gabriel:run-frpoly/fixnum nil 100)
      (run-benchmark 'cl-bench.gabriel:run-frpoly/bignum nil 30)
      (run-benchmark 'cl-bench.gabriel:run-frpoly/float nil 100)
      (run-benchmark 'cl-bench.gabriel:run-puzzle nil 1500)
      (run-benchmark 'cl-bench.gabriel:run-tak)
      (run-benchmark 'cl-bench.gabriel:run-ctak)
      (run-benchmark 'cl-bench.gabriel:run-trtak)
      (run-benchmark 'cl-bench.gabriel:run-takl)
      #+nil
      (run-benchmark 'cl-bench.gabriel:run-stak)
      (run-benchmark 'cl-bench.gabriel:fprint/ugly nil 200)
      (run-benchmark 'cl-bench.gabriel:fprint/pretty)
      (run-benchmark 'cl-bench.gabriel:run-traverse)
      (run-benchmark 'cl-bench.gabriel:run-triangle)
      (run-benchmark 'cl-bench.richards:richards)
      (run-benchmark 'cl-bench.math:run-factorial nil 1000)
      (run-benchmark 'cl-bench.math:run-fib nil 50)
      (run-benchmark 'cl-bench.math:run-fib-ratio)
      #+nil
      (run-benchmark 'cl-bench.math:run-ackermann)
      (run-benchmark 'cl-bench.math:run-mandelbrot/complex)
      (run-benchmark 'cl-bench.math:run-mandelbrot/dfloat)
      (run-benchmark 'cl-bench.math:run-mrg32k3a)
      (run-benchmark 'cl-bench.crc:run-crc40)
      (run-benchmark 'cl-bench.bignum:run-elem-100-1000)
      (run-benchmark 'cl-bench.bignum:run-elem-1000-100)
      (run-benchmark 'cl-bench.bignum:run-elem-10000-1)
      (run-benchmark 'cl-bench.bignum:run-pari-100-10)
      (run-benchmark 'cl-bench.bignum:run-pari-200-5)
      (run-benchmark 'cl-bench.bignum:run-pi-decimal/small)
      #-allegro
      (run-benchmark 'cl-bench.bignum:run-pi-decimal/big)
      (run-benchmark 'cl-bench.bignum:run-pi-atan)
      (run-benchmark 'cl-bench.ratios:run-pi-ratios)
      #-clisp
      (run-benchmark 'cl-bench.hash:run-slurp-lines nil 30)
      #-allegro
      (run-benchmark 'cl-bench.hash:hash-strings nil 2)
      (run-benchmark 'cl-bench.hash:hash-integers nil 10)
      #-allegro
      (run-benchmark 'cl-bench.boehm-gc:gc-benchmark)
      (run-benchmark 'cl-bench.deflate:run-deflate-file nil 100)
      #-allegro
      (run-benchmark 'cl-bench.arrays:bench-1d-arrays)
      #-allegro
      (run-benchmark 'cl-bench.arrays:bench-2d-arrays '(1000 1))
      #-allegro
      (run-benchmark 'cl-bench.arrays:bench-3d-arrays '(100 1))
      (run-benchmark 'cl-bench.arrays:bench-bitvectors nil 3)
      #-allegro
      (run-benchmark 'cl-bench.arrays:bench-strings)
      #-allegro
      (run-benchmark 'cl-bench.arrays:bench-strings/adjustable '(1000000 1))
      #-(or allegro clisp)
      (run-benchmark 'cl-bench.arrays:bench-string-concat '(1000000 1))
      #-allegro
      (run-benchmark 'cl-bench.arrays:bench-search-sequence '(1000000 1))
      (return-from run-benchmarks)
      (run-benchmark 'cl-bench.clos:run-defclass)
      (run-benchmark 'cl-bench.clos:run-defmethod)
      (run-benchmark 'cl-bench.clos:make-instances)
      (run-benchmark 'cl-bench.clos:make-instances/simple)
      (run-benchmark 'cl-bench.clos:methodcalls/simple)
      (run-benchmark 'cl-bench.clos:methodcalls/simple+after)
      #-clisp
      (run-benchmark 'cl-bench.clos:methodcalls/complex)
      #+nil
      (run-benchmark 'cl-bench.clos:run-eql-fib)
      (run-benchmark 'cl-bench.clos::eql-fib '(16)))))

(in-package "CL-USER")

(import '(cl-bench:run-benchmark cl-bench:run-benchmarks))

(export '(run-benchmark run-benchmarks))
