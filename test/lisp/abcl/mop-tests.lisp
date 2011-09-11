;;; mop-tests.lisp
;;;
;;; Copyright (C) 2010 Matthias Hölzl
;;; Copyright (C) 2010 Erik Huelsmann
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

(in-package #:abcl.test.lisp)

(deftest compute-applicable-methods.foo.1
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(111 222))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'fixnum 'fixnum)))
  t)

(deftest compute-applicable-methods.foo.2
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(x y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'symbol 'symbol)))
  t)

(deftest compute-applicable-methods.foo.3
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(111 y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'fixnum 'symbol)))
  t)

(deftest compute-applicable-methods.foo.4
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(x 111))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'symbol  'fixnum)))
  t)

(deftest compute-applicable-methods.foo.5
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(111 "asdf"))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'fixnum  'simple-base-string)))
  t)

(deftest compute-applicable-methods.foo.6
    (equalp (mop:compute-applicable-methods #'mop-test.foo '(111 222))
	    (list (find-foo 'fixnum 'fixnum)
		  (find-foo 'fixnum t)
		  (find-foo t t)))
  t)

(deftest compute-applicable-methods.foo.7
    (equalp (mop:compute-applicable-methods #'mop-test.foo '(111 x))
	    (list (find-foo 'fixnum t)
		  (find-foo t t)))
  t)

(deftest compute-applicable-methods.foo.8
    (equalp (mop:compute-applicable-methods #'mop-test.foo '(x 222))
	    (list (find-foo t t)))
  t)


(deftest compute-applicable-methods.bar.1
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 222))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'fixnum 'fixnum)))
  ;;; Bar with two fixnums might select EQL specializer for second
  ;;; argument.
  nil)

(deftest compute-applicable-methods.bar.1a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 222))
     (list (find-bar 'fixnum 'fixnum)
	   (find-bar 'fixnum t)
	   (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.1b
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 123))
     (list (find-method #'mop-test.bar nil (list (find-class 'fixnum) '(eql 123)))
	   (find-bar 'fixnum 'fixnum)
	   (find-bar 'fixnum t)
	   (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.1c
    (mop:compute-applicable-methods-using-classes
     #'mop-test.bar (find-classes 'fixnum 'fixnum))
  nil
  nil)

(deftest compute-applicable-methods.bar.2
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(x y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'symbol 'symbol)))
  t)

(deftest compute-applicable-methods.bar.2a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(x y))
     (list (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.3
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'fixnum 'symbol)))
  t)

(deftest compute-applicable-methods.bar.3a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 y))
     (list (find-bar 'fixnum t)
	   (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.4
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(x 111))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'symbol  'fixnum)))
  t)

(deftest compute-applicable-methods.bar.4a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(x 111))
     (list (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.5
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 "asdf"))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'fixnum  'simple-base-string)))
  t)

(deftest compute-applicable-methods.bar.5a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 "asdf"))
     (list (find-bar 'fixnum 'string)
	   (find-bar 'fixnum t)
	   (find-bar t t)))
  t)


(deftest compute-applicable-methods.baz.1
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(111 222))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'fixnum 'fixnum)))
  ;; Two fixnum arguments might select EQL specializer for first
  ;; argument.
  nil)

(deftest compute-applicable-methods.baz.1a
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(111 222))
     (list (find-baz 'fixnum 'fixnum)
	   (find-baz 'fixnum t)
	   (find-baz t t)))
  t)

(deftest compute-applicable-methods.baz.1b
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(234 222))
     (list (find-method #'mop-test.baz nil (list '(eql 234) (find-class 'fixnum)))
	   (find-baz 'fixnum 'fixnum)
	   (find-baz 'fixnum t)
	   (find-baz t t)))
  t)

(deftest compute-applicable-methods.baz.1c
    (mop:compute-applicable-methods-using-classes
     #'mop-test.baz (find-classes 'fixnum 'fixnum))
  nil
  nil)

(deftest compute-applicable-methods.baz.2
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(x y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'symbol 'symbol)))
  t)

(deftest compute-applicable-methods.baz.3
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(111 y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'fixnum 'symbol)))
  t)

(deftest compute-applicable-methods.baz.4
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(x 111))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'symbol  'fixnum)))
  t)

(deftest compute-applicable-methods.baz.5
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(111 "asdf"))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'fixnum  'simple-base-string)))
  t)


(deftest compute-applicable-methods.quux.1
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 222))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'fixnum 'fixnum)))
  t)

(deftest compute-applicable-methods.quux.1a
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 222))
     (list (find-quux 'fixnum 'fixnum)
	   (find-quux 'fixnum t)
	   (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.2
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(x y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'symbol 'symbol)))
  t)

(deftest compute-applicable-methods.quux.2a
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(x y))
     (list (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.3
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'fixnum 'symbol)))
  t)

(deftest compute-applicable-methods.quux.3a
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 y))
     (list (find-quux 'fixnum t)
	   (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.4
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(x 111))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'symbol  'fixnum)))
  ;; Symbol/fixnum might trigger EQL spezializer
  nil)

(deftest compute-applicable-methods.quux.4a
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(x 111))
     (list (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.4b
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(:foo 111))
     (list (find-method #'mop-test.quux nil
			(list '(eql :foo) (find-class 'fixnum)))

	   (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.4c
    (mop:compute-applicable-methods-using-classes
     #'mop-test.quux (find-classes 'symbol 'fixnum))
  nil
  nil)

(deftest compute-applicable-methods.quux.5
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 "asdf"))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'fixnum  'simple-base-string)))
  t)



;; tests for D-M-C, long form, taken from SBCL

;; D-M-C should return the name of the new method combination, nothing else.

(deftest dmc-return.1
    (define-method-combination dmc-test-return-foo)
  'dmc-test-return-foo)

(deftest dmc-return.2
    (define-method-combination dmc-test-return-bar :operator and)
  'dmc-test-return-bar)

(deftest dmc-return.3
    (define-method-combination dmc-test-return
        (&optional (order :most-specific-first))
      ((around (:around))
       (primary (dmc-test-return) :order order :required t))
      (let ((form (if (rest primary)
                      `(and ,@(mapcar #'(lambda (method)
                                          `(call-method ,method))
                                      primary))
                      `(call-method ,(first primary)))))
        (if around
            `(call-method ,(first around)
                          (,@(rest around)
                             (make-method ,form)))
            form)))
  'dmc-test-return)

;; A method combination which originally failed;
;;   for different reasons in SBCL than in ABCL (hence leaving out
;;   the original comment)

(define-method-combination dmc-test-mc.1
    (&optional (order :most-specific-first))
  ((around (:around))
   (primary (dmc-test-mc) :order order :required t))
  (let ((form (if (rest primary)
                  `(and ,@(mapcar #'(lambda (method)
                                      `(call-method ,method))
                                  primary))
                  `(call-method ,(first primary)))))
    (if around
        `(call-method ,(first around)
                      (,@(rest around)
                         (make-method ,form)))
        form)))

(defgeneric dmc-test-mc.1 (&key k) (:method-combination dmc-test-mc.1))

(defmethod dmc-test-mc.1 dmc-test-mc (&key k)
  k)

(deftest dmc-test-mc.1
    (dmc-test-mc.1 :k 1)
  1)


;; Completely DIY -- also taken from SBCL:
(define-method-combination dmc-test-mc.2 ()
  ((all-methods *))
  (do ((methods all-methods (rest methods))
       (primary nil)
       (around nil))
      ((null methods)
       (let ((primary (nreverse primary))
             (around (nreverse around)))
         (if primary
              (let ((form (if (rest primary)
                             `(call-method ,(first primary) ,(rest primary))
                             `(call-method ,(first primary)))))
                (if around
                    `(call-method ,(first around) (,@(rest around)
                                                   (make-method ,form)))
                    form))
              `(make-method (error "No primary methods")))))
    (let* ((method (first methods))
           (qualifier (first (method-qualifiers method))))
      (cond
        ((equal :around qualifier)
         (push method around))
        ((null qualifier)
         (push method primary))))))

(defgeneric dmc-test-mc.2a (val)
  (:method-combination dmc-test-mc.2))

(defmethod dmc-test-mc.2a ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))

(deftest dmc-test-mc.2a
    (= (dmc-test-mc.2a 13) 13)
  T)

(defgeneric dmc-test-mc.2b (val)
  (:method-combination dmc-test-mc.2))

(defmethod dmc-test-mc.2b ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))

(defmethod dmc-test-mc.2b :around ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))

(deftest dmc-test-mc.2b
    (= 26 (dmc-test-mc.2b 13))
  T)


;;; Taken from SBCL: error when method sorting is ambiguous
;;;  with multiple method groups

(define-method-combination dmc-test-mc.3a ()
  ((around (:around))
   (primary * :required t))
  (let ((form (if (rest primary)
                  `(call-method ,(first primary) ,(rest primary))
                  `(call-method ,(first primary)))))
    (if around
        `(call-method ,(first around) (,@(rest around)
                                       (make-method ,form)))
        form)))

(defgeneric dmc-test-mc.3a (val)
  (:method-combination dmc-test-mc.3a))

(defmethod dmc-test-mc.3a ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))

(defmethod dmc-test-mc.3a :around ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))

(defmethod dmc-test-mc.3a :somethingelse ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))

(deftest dmc-test-mc.3a
    (multiple-value-bind
          (value error)
        (ignore-errors (wam-test-mc.3a 13))
      (declare (ignore value))
      (typep error 'error))
  T)

;;; Taken from SBCL: error when method sorting is ambiguous
;;;  with a single (non *) method group


(define-method-combination dmc-test-mc.3b ()
  ((methods listp :required t))
  (if (rest methods)
      `(call-method ,(first methods) ,(rest methods))
      `(call-method ,(first methods))))

(defgeneric dmc-test-mc.3b (val)
  (:method-combination dmc-test-mc.3b))

(defmethod dmc-test-mc.3b :foo ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))

(defmethod dmc-test-mc.3b :bar ((val number))
  (+ val (if (next-method-p) (call-next-method) 0)))

(deftest dmc-test-mc.3b
    (multiple-value-bind
          (value error)
        (ignore-errors (dmc-test-mc.3b 13))
      (declare (ignore value))
      (typep error 'error))
  T)


;; Taken from SBCL: test that GF invocation arguments
;;   are correctly bound using the (:arguments ...) form

(defparameter *dmc-test-4* nil)

(defun object-lock (obj)
  (push "object-lock" *dmc-test-4*)
  obj)
(defun unlock (obj)
  (push "unlock" *dmc-test-4*)
  obj)
(defun lock (obj)
  (push "lock" *dmc-test-4*)
  obj)


(define-method-combination dmc-test-mc.4 ()
  ((methods ()))
  (:arguments object)
  `(unwind-protect
        (progn (lock (object-lock ,object))
               ,@(mapcar #'(lambda (method)
                             `(call-method ,method))
                         methods))
     (unlock (object-lock ,object))))

(defgeneric dmc-test.4 (x)
  (:method-combination progn-with-lock))
(defmethod dmc-test.4 ((x symbol))
  (push "primary" *dmc-test-4*))
(defmethod dmc-test.4 ((x number))
  (error "foo"))

(deftest dmc-test.4a
    (progn
      (setq *dmc-test-4* nil)
      (values (equal (dmc-test.4 t) '("primary" "lock" "object-lock"))
              (equal *dmc-test-4* '("unlock" "object-lock"
                                    "primary" "lock" "object-lock"))))
  T T)

(deftest dmc-test.4b
    (progn
      (setq *dmc-test-4* nil)
      (ignore-errors (dmc-test.4 1))
      (equal *dmc-test-4* '("unlock" "object-lock" "lock" "object-lock")))
  T)


;; From SBCL: method combination (long form) with arguments

(define-method-combination dmc-test.5 ()
  ((method-list *))
  (:arguments arg1 arg2 &aux (extra :extra))
  (print (type-of method-list))
  (print method-list)
  `(progn ,@(mapcar (lambda (method) `(call-method ,method)) method-list)))

(defgeneric dmc-test-mc.5 (p1 p2 s)
  (:method-combination dmc-test.5)
  (:method ((p1 number) (p2 t) s)
    (vector-push-extend (list 'number p1 p2) s))
  (:method ((p1 string) (p2 t) s)
    (vector-push-extend (list 'string p1 p2) s))
  (:method ((p1 t) (p2 t) s) (vector-push-extend (list t p1 p2))))

(deftest dmc-test.5a
    (let ((v (make-array 0 :adjustable t :fill-pointer t)))
      (values (dmc-test-mc.5 1 2 v)
              (equal (aref v 0) '(number 1 2))
              (equal (aref v 1) '(t 1 2))))
  1 T T)



(define-method-combination dmc-test.6 ()
  ((normal ())
   (ignored (:ignore :unused)))
  `(list 'result
    ,@(mapcar #'(lambda (method) `(call-method ,method)) normal)))

(defgeneric dmc-test-mc.6 (x)
  (:method-combination dmc-test.6)
  (:method :ignore ((x number)) (/ 0)))

(deftest dmc-test-mc.6a
    (multiple-value-bind
          (value error)
        (ignore-errors (dmc-test-mc.6 7))
      (values (null value)
              (typep error 'invalid-method-error)))
  T T)


(define-method-combination dmc-test.7 ()
  ((methods *))
  (:arguments x &rest others)
  `(progn
     ,@(mapcar (lambda (method)
                 `(call-method ,method))
               methods)
     (list ,x (length ',others))))

(defgeneric dmc-test-mc.7 (x &rest others)
  (:method-combination dmc-test.7))

(defmethod dmc-test-mc.7 (x &rest others)
  (declare (ignore others))
  nil)

(deftest dmc-test-mc.7a
    (equal (apply #'dmc-test-mc.7 :foo (list 1 2 3 4 5 6 7 8))
           '(:foo 8)))


(defclass foo-class (standard-class))
(defmethod mop:validate-superclass ((class foo-class) (superclass standard-object))
  t)

(deftest validate-superclass.1
    (mop:validate-superclass
     (make-instance 'foo-class)
     (make-instance 'standard-object))
  t)


