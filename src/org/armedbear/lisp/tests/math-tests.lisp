;;; math-tests.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
;;; $Id$
;;;
;;; This program is free software; you can redistribute it and/or
;;; modify it under the terms of the GNU General Public License
;;; as published by the Free Software Foundation; either version 2
;;; of the License, or (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program; if not, write to the Free Software
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

;;; Some of these tests are based on tests in the CLISP test suite.

(load "test-utilities.lisp")

(in-package #:test)

#+(or abcl cmu sbcl)
(defmacro set-floating-point-modes (&rest args)
  `(funcall #+abcl 'ext:set-floating-point-modes
            #+cmu  'ext:set-floating-point-modes
            #+sbcl 'sb-int:set-floating-point-modes
            ,@args))

#+(or abcl cmu sbcl)
(defmacro get-floating-point-modes ()
  #+abcl `(ext:get-floating-point-modes)
  #+cmu  `(ext:get-floating-point-modes)
  #+sbcl `(sb-int:get-floating-point-modes))

#+(or abcl cmu sbcl)
(defun restore-default-floating-point-modes ()
  #+abcl
  (set-floating-point-modes :traps '(:overflow :underflow))
  #+(or cmu sbcl)
  (set-floating-point-modes :traps '(:overflow :invalid :divide-by-zero)))

#+(or abcl cmu sbcl)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (restore-default-floating-point-modes))

(deftest most-negative-fixnum.1
  (= (/ most-negative-fixnum -1) (- most-negative-fixnum))
  t)

(deftest most-negative-fixnum.2
  (= (abs most-negative-fixnum) (- most-negative-fixnum))
  t)

#+(or abcl cmu sbcl)
(deftest floating-point-modes.1
  (unwind-protect
      (progn
        (set-floating-point-modes :traps nil)
        (getf (get-floating-point-modes) :traps))
    (restore-default-floating-point-modes))
  nil)

#+(or abcl cmu sbcl)
(deftest floating-point-modes.2
  (unwind-protect
      (progn
        (set-floating-point-modes :traps '(:overflow))
        (getf (get-floating-point-modes) :traps))
    (restore-default-floating-point-modes))
   (:overflow))

#+(or abcl cmu sbcl)
(deftest floating-point-modes.3
  (unwind-protect
      (progn
        (set-floating-point-modes :traps '(:underflow))
        (getf (get-floating-point-modes) :traps))
    (restore-default-floating-point-modes))
  (:underflow))

#+(or abcl cmu sbcl)
(deftest floating-point-modes.4
  (unwind-protect
      (progn
        (set-floating-point-modes :traps '(:overflow :underflow))
        (set-exclusive-or (getf (get-floating-point-modes) :traps)
                          '(:overflow :underflow)))
    (restore-default-floating-point-modes))
  nil)

(deftest single-float-epsilon.1
  single-float-epsilon
  #+lispworks 1.1102230246251568f-16
  #-lispworks 5.960465f-8)

(deftest single-float-negative-epsilon.1
  single-float-negative-epsilon
  #+lispworks 5.551115123125784f-17
  #-lispworks 2.9802326f-8)

(deftest most-positive-single-float.1
  most-positive-single-float
  #-lispworks
  3.4028235e+38
  #+lispworks
  1.7976931348623157E308)

(deftest most-positive-single-float.2
  (log most-positive-single-float)
  #-lispworks 88.72284
  #+lispworks 709.782712893384)

(deftest least-positive-single-float.1
  least-positive-single-float
  #-(or clisp lispworks) 1.4012985e-45
  #+clisp 1.1754944E-38
  #+lispworks 4.9406564584124646E-324)

(deftest least-positive-single-float.2
  (log least-positive-single-float)
  #-(or clisp lispworks) -103.27893
  #+clisp -87.33655
  #+lispworks -744.4400719213812)

;; SQRT
(deftest sqrt.1
  (sqrt 0)
  #+clisp 0
  #-clisp 0.0)

(deftest sqrt.2
  (sqrt 1)
  #+clisp 1
  #-clisp 1.0)

(deftest sqrt.3
  (sqrt 9)
  #+clisp 3
  #-clisp 3.0)

(deftest sqrt.4
  (sqrt -9)
  #+clisp #c(0 3)
  #-clisp #c(0.0 3.0))

(deftest sqrt.5
  (sqrt #c(-7 24))
  #-(or clisp lispworks) #c(3.0 4.0)
  #+clisp #c(3 4)
  #+lispworks #c(3.0 3.999999999999999))

(deftest sqrt.6
  (sqrt 1d0)
  1.0d0)

(deftest sqrt.7
  (sqrt -1)
  #+(or clisp) #c(0 1)
  #+(or abcl allegro cmu lispworks sbcl) #c(0.0 1.0))

(deftest sqrt.8
  (sqrt -1d0)
  #c(0 1.0d0))

(deftest sqrt.9
  (sqrt #c(0.0 0.0))
  #c(0.0 0.0))

(deftest sqrt.10
  (sqrt #c(4.0 0.0))
  #c(2.0 0.0))

(deftest sqrt.11
  (sqrt #c(-4.0 0.0))
  #c(0.0 2.0))

(deftest sqrt.12
  (sqrt #c(-4.4855622e-7 0.0))
  #-lispworks
  #c(0.0 6.697434e-4)
  #+lispworks
  #c(0.0 6.697433986236818e-4))

#+(or abcl cmu lispworks sbcl)
(deftest sqrt.13
  (float-sign (sqrt -0.0))
  -1.0)

#+(or abcl cmu lispworks sbcl)
(deftest sqrt.14
  (float-sign (sqrt -0.0d0))
  -1.0d0)

;; EXP
(deftest exp.1
  (exp #c(0 0))
  #+(or abcl allegro cmu lispworks sbcl) 1.0
  #+clisp 1)

(deftest exp.2
  (exp #c(0 1))
  #-lispworks #c(0.5403023          0.84147096)
  #+lispworks #c(0.5403023058681398 0.8414709848078965))

(deftest exp.3
  (exp #c(1 1))
  #+(or abcl cmu sbcl) #c(1.4686939          2.2873552)
  #+(or allegro clisp) #c(1.468694           2.2873552)
  #+lispworks          #c(1.4686939399158851 2.2873552871788423))

(deftest exp.4
  (exp #c(1 1d0))
  #c(1.4686939399158851d0 2.2873552871788423d0))

(deftest exp.5
  (exp #c(1d0 1d0))
  #c(1.4686939399158851d0 2.2873552871788423d0))

(deftest exp.6
  (exp #c(0 1d0))
  #c(0.5403023058681398d0 0.8414709848078965d0))

(deftest exp.7
  (exp 1)
  #-lispworks 2.7182817
  #+lispworks 2.718281828459045)

(deftest exp.8
  (exp 1f0)
  #-lispworks 2.7182817
  #+lispworks 2.718281828459045)

(deftest exp.9
  (exp 1d0)
  2.718281828459045d0)

;; EXPT
(deftest expt.1
  (expt -5.0f0 2)
  25.0)

(deftest expt.2
  (expt -5.0f0 1.9f0)
  #c(20.241808 -6.576964))

(deftest expt.3
  (expt -5.0f0 2.0f0)
  #+(or abcl cmu sbcl) 25f0
  #+allegro            #c(25.0               -6.1230318e-15)
  #+clisp              #c(25f0               0f0)
  #+lispworks          #c(24.999999999999993 -6.123031769111885e-15))

(deftest expt.4
  (expt -5.0f0 2.1f0)
  #c(27.928223 9.074421))

(deftest expt.5
  (expt -5.0d0 1.9d0)
  #+(or abcl allegro) #c(20.24180952239008d0  -6.576962601219341d0)
  #+clisp             #c(20.241809522390078d0 -6.576962601219342d0)
  #+(or cmu sbcl)     #c(20.241809522390078d0 -6.57696260121934d0))

(deftest expt.6
  (expt -5.0d0 2.0d0)
  #+(or abcl cmu sbcl) 25d0
  #+allegro            #c(24.999999999999996d0 -6.1230317691118855d-15)
  #+clisp              #c(25d0                 0d0))

(deftest expt.7
  (expt -5.0d0 2.1d0)
  #+allegro            #c(27.92822499968966d0  9.074430383223417d0)
  #+clisp              #c(27.928224999689668d0 9.074430383223435d0)
  #-(or allegro clisp) #c(27.92822499968967d0  9.07443038322342d0))

(deftest expt.8
  (expt -5 2)
  25)

(deftest expt.9
  (eql (expt 5f0 3f0) (* 5.0 5.0 5.0))
  t)

(deftest expt.10
  (expt 5f0 3f0)
  125f0)

(deftest expt.11
  (expt 5d0 3d0)
  125d0)

(deftest expt.12
  (expt 5 3)
  125)

(deftest expt.13
  (expt #c(10 11) 1)
  #c(10 11))

(deftest expt.14
  (expt 0 1/2)
  #+(or abcl allegro clisp lispworks) 0
  #+(or cmu sbcl) 0.0)

(deftest expt.15
  (expt 1 1/2)
  #+clisp 1
  #-clisp 1.0)

(deftest expt.16
  (expt 9 1/2)
  #+clisp 3
  #-clisp 3.0)

(deftest expt.17
  (expt -9 1/2)
  #+clisp                 #c(0             3)
  #+(or allegro sbcl cmu) #c(1.8369095e-16 3.0)
  #+abcl                  #c(1.8369701e-16 3.0))

(deftest expt.18
  (expt -8 1/3)
  #c(1.0 1.7320508))

(deftest expt.19
  (expt #c(-7 24) 1/2)
  #+clisp #c(3 4)
  #-clisp #c(3.0 4.0))

(deftest expt.20
  (expt 729 1/6)
  #+clisp 3
  #-clisp 3.0)

(deftest expt.21
  (expt -3 -1)
  -1/3)

(deftest expt.22
  (expt #c(3 4) -1)
  #c(3/25 -4/25))

(deftest expt.23
  (expt 14 #c(1.0 1.0))
  #-(or clisp allegro) #c(-12.269101 6.743085)
  #+(or clisp allegro) #c(-12.269099 6.7430854))

(deftest expt.24
  (expt 0.0 4)
  0.0)

(deftest expt.25
  (expt #c(0 0.0) 4)
  #c(0.0 0.0))

(deftest expt.25
  (expt #c(0 0.0) 4.0)
  #c(0.0 0.0))

(deftest log.1
  (typep (log 17d0 10) 'double-float)
  t)

(deftest log.2
  (typep (log 17 10d0) 'double-float)
  t)

(deftest log.3
  (log 17 10)
  #+(and abcl java-1.4)               1.2304488
  #+(and abcl (or java-1.5 java-1.6)) 1.230449
  #+(or allegro clisp cmu sbcl)       1.230449
  #+lispworks                         #.(log 17d0 10d0))

(deftest log.4
  (log 17.0 10.0)
  #+(and abcl java-1.4)               1.2304488
  #+(and abcl (or java-1.5 java-1.6)) 1.230449
  #+(or cmu sbcl)                     1.2304488
  #+(or allegro clisp)                1.230449
  #+lispworks                         #.(log 17d0 10d0))

(deftest log.5
  (log 17d0 10)
  #+(and abcl java-1.4)               1.2304489042913307d0
  #+(and abcl (or java-1.5 java-1.6)) #.(log 17d0 10d0)
  #+(or allegro clisp lispworks)      #.(log 17d0 10d0)
  #-(or abcl allegro clisp lispworks) 1.2304489042913307d0)

(deftest log.6
  (log 17 10d0)
  #+(and abcl java-1.4)               1.2304489149763256d0
  #+(and abcl (or java-1.5 java-1.6)) #.(log 17d0 10d0)
  #+(or allegro clisp lispworks)      #.(log 17d0 10d0)
  #-(or abcl allegro clisp lispworks) 1.2304489149763256d0)

(deftest log.7
  (log 17d0 10d0)
  1.2304489213782739d0)

(deftest pi.1
  pi
  #+clisp 3.1415926535897932385l0
  #-clisp 3.141592653589793d0)

(deftest tan.1
  (tan 1)
  #+lispworks 1.5574077246549023
  #-lispworks 1.5574077)

(deftest tan.2
  (tan (- (/ pi 2) 0.0001))
  #+(or abcl allegro cmu sbcl) 10000.0002192818d0
  #+clisp                      10000.000219287924741l0
  #+lispworks                   9999.999966661644)

(deftest tan.3
  (tan (/ pi 2))
  #+abcl                             1.633123935319537d16
  #+(or allegro cmu lispworks sbcl)  1.6331778728383844d16
  #+clisp                           -3.9867976290042641156l19)

(deftest tan.4
  (tan (+ (/ pi 2) 0.0001))
  #+(or abcl allegro cmu sbcl) -10000.000219294045d0
  #+clisp                      -10000.000219287919724l0
  #+lispworks                   -9999.999966673891d0)

(deftest atanh.1
  (atanh 2)
  #C(0.54930615 1.5707964))

(deftest atanh.2
  (atanh -2)
  #C(-0.54930615 -1.5707964))

(deftest truncate.1
  (truncate least-positive-single-float)
  0 #.least-positive-single-float)

(deftest truncate.2
  (truncate least-positive-double-float)
  0 #.least-positive-double-float)

(deftest truncate.3
  (signals-error (truncate least-positive-single-float 2) 'floating-point-underflow)
  t)

(deftest truncate.4
  (signals-error (truncate least-positive-double-float 2) 'floating-point-underflow)
  t)

(deftest read-from-string.1
  #+(or cmu sbcl)
  (unwind-protect
      (signals-error (read-from-string "1.0f-1000") 'reader-error)
    (progn
      (ignore-errors (set-floating-point-modes :traps '(:underflow)))
      (restore-default-floating-point-modes)))
  #-(or cmu sbcl)
  (signals-error (read-from-string "1.0f-1000") 'reader-error)
  t)

(do-tests)
