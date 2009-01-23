;;; compiler-tests.lisp
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

#+abcl
(require '#:jvm)

(load (merge-pathnames "test-utilities.lisp" *load-truename*))

(in-package #:test)

(defconstant most-positive-java-long 9223372036854775807)
(defconstant most-negative-java-long -9223372036854775808)

#+abcl
(assert (eql most-positive-java-long ext:most-positive-java-long))
#+abcl
(assert (eql most-negative-java-long ext:most-negative-java-long))

(defmacro define-compiler-test (name lambda-form &key args results)
  `(deftest ,name
     (progn
       (fmakunbound ',name)
       (defun ,name ,(cadr lambda-form)
         ,@(cddr lambda-form))
       (values
        (funcall ',name ,@args)
        (multiple-value-list (compile ',name))
        (compiled-function-p #',name)
        (funcall ',name ,@args)))
     ,results
     (,name nil nil)
     t
     ,results))

#+abcl
(deftest unused.1
  (let ((output (with-output-to-string (*error-output*)
                  (compile nil '(lambda () (let ((x 42)) 17))))))
    (integerp (search "The variable X is defined but never used." output)))
  t)

(deftest unused.2
  (progn
    (fmakunbound 'unused.2)
    (defun unused.2 () (let ((x 42)) 17))
    (values
     #-lispworks
     (multiple-value-list (compile 'unused.2))
     #+lispworks
     (let ((list (multiple-value-list (compile 'unused.2))))
       (list (first list)
             (not (null (second list)))
             (third list)))
     (unused.2)))
  #+(or abcl allegro) (unused.2 t   nil)
  #+clisp             (unused.2 1   nil)
  #+(or cmu sbcl)     (unused.2 nil nil)
  #+lispworks         (unused.2 t   nil)
  17)

(deftest plus.1
  (progn
    (fmakunbound 'plus.1)
    (defun plus.1 (x y)
      (+ x y))
    (compile 'plus.1)
    (plus.1 most-positive-fixnum most-positive-fixnum))
  #.(+ most-positive-fixnum most-positive-fixnum))

(deftest plus.2
  (progn
    (fmakunbound 'plus.2)
    (defun plus.2 (x y)
      (declare (optimize speed))
      (declare (type fixnum x y))
      (+ x y))
    (compile 'plus.2)
    (plus.2 most-positive-fixnum most-positive-fixnum))
  #.(+ most-positive-fixnum most-positive-fixnum))

(deftest plus.3
  (progn
    (fmakunbound 'plus.3)
    (defun plus.3 (x y)
      (declare (optimize speed (safety 0)))
      (declare (type fixnum x y))
      (+ x y))
    (compile 'plus.3)
    (plus.3 most-positive-fixnum most-positive-fixnum))
  #.(+ most-positive-fixnum most-positive-fixnum))
#+allegro (pushnew 'plus.3 *expected-failures*)

(define-compiler-test plus.4
  (lambda (x y)
    (declare (type (integer #.most-negative-java-long #.most-positive-java-long) x y))
    (+ x y))
  :args (#.most-positive-java-long #.most-positive-java-long)
  :results #.(+ most-positive-java-long most-positive-java-long))

(define-compiler-test minus.1
  (lambda (x)
    (declare (type fixnum x))
    (- x))
  :args (#.most-negative-fixnum)
  :results #.(- most-negative-fixnum))

(define-compiler-test minus.2
  (lambda (x)
    (declare (type (integer #.most-negative-java-long #.most-positive-java-long) x))
    (- x))
  :args (#.most-negative-java-long)
  :results #.(- most-negative-java-long))

(define-compiler-test minus.3
  (lambda (x y)
    (declare (type (integer #.most-negative-java-long #.most-positive-java-long) x y))
    (- x y))
  :args (#.most-negative-java-long #.most-positive-java-long)
  :results #.(- most-negative-java-long most-positive-java-long))

(define-compiler-test logxor-minus.1
  (lambda (x)
    (declare (type (integer 0 255) x))
    (logxor (- x) #.most-positive-java-long))
  :args (17)
  :results -9223372036854775792)

(deftest times.1
  (progn
    (fmakunbound 'times.1)
    (defun times.1 (x y)
      (* x y))
    (compile 'times.1)
    (times.1 most-positive-fixnum most-positive-fixnum))
  #.(* most-positive-fixnum most-positive-fixnum))

(deftest times.2
  (progn
    (fmakunbound 'times.2)
    (defun times.2 (x y)
      (declare (optimize speed))
      (declare (type fixnum x y))
      (* x y))
    (compile 'times.2)
    (times.2 most-positive-fixnum most-positive-fixnum))
  #.(* most-positive-fixnum most-positive-fixnum))

(deftest times.3
  (progn
    (fmakunbound 'times.3)
    (defun times.3 (x y)
      (declare (optimize speed (safety 0)))
      (declare (type fixnum x y))
      (* x y))
    (compile 'times.3)
    (times.3 most-positive-fixnum most-positive-fixnum))
  #.(* most-positive-fixnum most-positive-fixnum))

(deftest dotimes.1
  (progn
    (fmakunbound 'dotimes.1)
    (defun dotimes.1 ()
      (declare (optimize speed (safety 0)))
      (let ((result 0))
        (dotimes (i 10)
          (incf result))
        result))
    (compile 'dotimes.1)
    (dotimes.1))
  10)

(deftest dotimes.2
  (progn
    (fmakunbound 'dotimes.2)
    (defun dotimes.2 ()
      (declare (optimize speed (safety 0)))
      (let ((result 0))
        (declare (type fixnum result))
        (dotimes (i 10)
          (incf result))
        result))
    (compile 'dotimes.2)
    (dotimes.2))
  10)

#+abcl
(deftest derive-type-logxor.1
  (let ((type
         (jvm:derive-compiler-type `(logxor (the (unsigned-byte 8) x)
                                            (the (unsigned-byte 8) y)))))
    (and (sys:integer-type-p type)
         (values
          (sys:integer-type-low type)
          (sys:integer-type-high type))))
  0 255)

#+abcl
(deftest derive-type-logxor.2
  (let ((type
         (jvm:derive-compiler-type `(logxor 441516657
                                            (the (integer 0 8589934588) x)))))
    (and (sys:integer-type-p type)
         (values
          (sys:integer-type-low type)
          (sys:integer-type-high type))))
  0 8589934588)

#+abcl
(deftest derive-type-logxor.3
  (let ((type
         (jvm:derive-compiler-type `(logxor 441516657
                                            (the (integer 0 8589934588) x)
                                            (ash (the (integer 0 8589934588) x) -5)))))
    (and (sys:integer-type-p type)
         (values
          (sys:integer-type-low type)
          (sys:integer-type-high type))))
  0 8589934588)

(deftest ash.1
  (progn
    (fmakunbound 'ash.1)
    (defun ash.1 (n shift)
      (declare (type (integer 0 8589934588) n))
      (declare (type (integer -31 -1) shift))
      (ash n shift))
    (compile 'ash.1)
    (values
     (ash.1 8589934588 -1)
     (ash.1 8589934588 -2)
     (ash.1 8589934588 -3)
     (ash.1 8589934588 -4)
     (ash.1 8589934588 -5)
     (ash.1 8589934588 -6)
     (ash.1 8589934588 -31)))
  4294967294
  2147483647
  1073741823
  536870911
  268435455
  134217727
  3)

(deftest bignum-constant.1
  (progn
    (fmakunbound 'bignum-constant.1)
    (defun bignum-constant.1 () #.most-positive-java-long)
    (values (funcall 'bignum-constant.1)
            (multiple-value-list (compile 'bignum-constant.1))
            (compiled-function-p #'bignum-constant.1)
            (funcall 'bignum-constant.1)))
  #.most-positive-java-long
  (bignum-constant.1 nil nil)
  t
  #.most-positive-java-long)

(deftest bignum-constant.2
  (progn
    (fmakunbound 'bignum-constant.2)
    (defun bignum-constant.2 () #.(1+ most-positive-java-long))
    (values (funcall 'bignum-constant.2)
            (multiple-value-list (compile 'bignum-constant.2))
            (compiled-function-p #'bignum-constant.2)
            (funcall 'bignum-constant.2)))
  #.(1+ most-positive-java-long)
  (bignum-constant.2 nil nil)
  t
  #.(1+ most-positive-java-long))

(deftest bignum-constant.3
  (progn
    (fmakunbound 'bignum-constant.3)
    (defun bignum-constant.3 () #.most-negative-java-long)
    (values (funcall 'bignum-constant.3)
            (multiple-value-list (compile 'bignum-constant.3))
            (compiled-function-p #'bignum-constant.3)
            (funcall 'bignum-constant.3)))
  #.most-negative-java-long
  (bignum-constant.3 nil nil)
  t
  #.most-negative-java-long)

(deftest bignum-constant.4
  (progn
    (fmakunbound 'bignum-constant.4)
    (defun bignum-constant.4 () #.(1- most-negative-java-long))
    (values (funcall 'bignum-constant.4)
            (multiple-value-list (compile 'bignum-constant.4))
            (compiled-function-p #'bignum-constant.4)
            (funcall 'bignum-constant.4)))
  #.(1- most-negative-java-long)
  (bignum-constant.4 nil nil)
  t
  #.(1- most-negative-java-long))

(deftest shiftf.1
  (progn
    (fmakunbound 'shiftf.1)
    (defun shiftf.1 (x)
      (declare (type (integer -5213 238468) x))
      (+ x (shiftf x 168771)))
    (values (funcall 'shiftf.1 96411)
            (multiple-value-list (compile 'shiftf.1))
            (compiled-function-p #'shiftf.1)
            (funcall 'shiftf.1 96411)))
  192822
  (shiftf.1 nil nil)
  t
  192822)

(deftest logand-values.1
  (ignore-errors (funcall (compile nil '(lambda () (logand 18 (values 42 7))))))
  2)

(deftest logand-lognot.1
  (progn
    (fmakunbound 'logand-lognot.1)
    (defun logand-lognot.1 (x)
      (declare (type (unsigned-byte 32) x))
      (logand #.(1- (expt 2 32)) (lognot x)))
    (values (funcall 'logand-lognot.1 123456789)
            (multiple-value-list (compile 'logand-lognot.1))
            (compiled-function-p #'logand-lognot.1)
            (funcall 'logand-lognot.1 123456789)))
  4171510506
  (logand-lognot.1 nil nil)
  t
  4171510506)

(deftest logior-logand-setf.1
  (progn
    (fmakunbound 'foo)
    (defun foo (x y)
      (declare (type (integer 2005076 2881158415) x))
      (declare (type (integer -28121355 17748872) y))
      (logior (logand (setf y -3475589)
                      x))
      y)
    (values (funcall 'foo 12345678 42)
            (multiple-value-list (compile 'foo))
            (compiled-function-p #'foo)
            (funcall 'foo 12345678 42)))
  -3475589
  (foo nil nil)
  t
  -3475589)

(deftest logxor.1
  (progn
    (fmakunbound 'foo)
    (defun foo ()
      (logxor -4153366606 (- 0)))
    (values (funcall 'foo)
            (multiple-value-list (compile 'foo))
            (compiled-function-p #'foo)
            (funcall 'foo)))
    -4153366606
    (foo nil nil)
    t
    -4153366606)

(define-compiler-test min.1
  (lambda (x y)
    (declare (type fixnum x y))
    (min x y))
  :args (3 4)
  :results 3)

(define-compiler-test min.2
  (lambda (x y)
    (declare (type fixnum x y))
    (min x y))
  :args (#.most-positive-fixnum #.most-negative-fixnum)
  :results #.most-negative-fixnum)

(define-compiler-test min.3
  (lambda (x y)
    (declare (type (integer #.most-negative-java-long #.most-positive-java-long) x y))
    (min x y))
  :args (3 4)
  :results 3)

(define-compiler-test min.4
  (lambda (x y)
    (declare (type (integer #.most-negative-java-long #.most-positive-java-long) x y))
    (min x y))
  :args (#.most-positive-java-long #.most-negative-java-long)
  :results #.most-negative-java-long)

(define-compiler-test max.1
  (lambda (x y)
    (declare (type fixnum x y))
    (max x y))
  :args (3 4)
  :results 4)

(define-compiler-test max.2
  (lambda (x y)
    (declare (type fixnum x y))
    (max x y))
  :args (#.most-positive-fixnum #.most-negative-fixnum)
  :results #.most-positive-fixnum)

(define-compiler-test max.3
  (lambda (x y)
    (declare (type (integer #.most-negative-java-long #.most-positive-java-long) x y))
    (max x y))
  :args (3 4)
  :results 4)

(define-compiler-test max.4
  (lambda (x y)
    (declare (type (integer #.most-negative-java-long #.most-positive-java-long) x y))
    (max x y))
  :args (#.most-positive-java-long #.most-negative-java-long)
  :results #.most-positive-java-long)

(do-tests)
