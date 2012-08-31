
;;; clos-tests.lisp
;;;
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


;; These tests are in clos tests, because e.g. D-M-C isn't mop, but *is* clos

(in-package #:abcl.test.lisp)



;; tests for D-M-C, long form, some taken from SBCL

;; D-M-C should return the name of the new method combination, nothing else.

(deftest dmc-return.1
    (define-method-combination dmc-test-return-foo)
  dmc-test-return-foo)

(deftest dmc-return.2
    (define-method-combination dmc-test-return-bar :operator and)
  dmc-test-return-bar)

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
  dmc-test-return)

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
  ((methods *))
  (:arguments object)
  `(unwind-protect
        (progn (lock (object-lock ,object))
               ,@(mapcar #'(lambda (method)
                             `(call-method ,method))
                         methods))
     (unlock (object-lock ,object))))

(defgeneric dmc-test.4 (x)
  (:method-combination dmc-test-mc.4))
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
  `(progn ,@(mapcar (lambda (method) `(call-method ,method)) method-list)))

(defgeneric dmc-test-mc.5 (p1 p2 s)
  (:method-combination dmc-test.5)
  (:method ((p1 number) (p2 t) s)
    (vector-push-extend (list 'number p1 p2) s))
  (:method ((p1 string) (p2 t) s)
    (vector-push-extend (list 'string p1 p2) s))
  (:method ((p1 t) (p2 t) s) (vector-push-extend (list t p1 p2) s)))

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
              (typep error 'error)))
  T T)


(define-method-combination dmc-test.7 ()
  ((methods *))
  (:arguments x &rest others)
  `(progn
     ,@(mapcar (lambda (method)
                 `(call-method ,method))
               methods)
     (list ,x (length ,others))))

(defgeneric dmc-test-mc.7 (x &rest others)
  (:method-combination dmc-test.7))

(defmethod dmc-test-mc.7 (x &rest others)
  (declare (ignore others))
  nil)

(deftest dmc-test-mc.7a
    (equal (apply #'dmc-test-mc.7 :foo (list 1 2 3 4 5 6 7 8))
           '(:foo 8))
  T)


;; Tests for D-M-C with :arguments option
;; created due to http://trac.common-lisp.net/armedbear/ticket/201

(define-method-combination dmc-test-args-with-whole.1 ()
  ((methods ()))
  (:arguments &whole whole)
  `(progn (format nil "using ~a" ,whole)
          ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)))

(defgeneric dmc-test-args-with-whole.1 (x)
  (:method-combination dmc-test-args-with-whole.1)
  (:method (x) x))

;; This test fails throws an error under #201
(deftest dmc-test-args-with-whole.1
    (dmc-test-args-with-whole.1 T)
  T)

(define-method-combination dmc-test-args-with-whole.2 ()
  ((methods ()))
  (:arguments &whole whole &rest rest)
  `(progn (format nil "using ~a ~a" ,whole ,rest)
          ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)))

(defgeneric dmc-test-args-with-whole.2 (x)
  (:method-combination dmc-test-args-with-whole.2)
  (:method (x) x))

(deftest dmc-test-args-with-whole.2
    (dmc-test-args-with-whole.2 T)
  T)


(define-method-combination dmc-test-args-with-whole.3a ()
  ((methods ()))
  (:arguments &whole whole &optional opt)
  `(progn (format nil "using ~a ~a" ,whole ,opt)
          ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)))

(defgeneric dmc-test-args-with-whole.3a (x)
  (:method-combination dmc-test-args-with-whole.3a)
  (:method (x) x))

(deftest dmc-test-args-with-whole.3a
    (dmc-test-args-with-whole.3a T)
  T)

(define-method-combination dmc-test-args-with-whole.3b ()
  ((methods ()))
  (:arguments &whole whole &optional opt &key k)
  `(progn (format nil "using ~a ~a ~a" ,whole ,opt ,k)
          ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)))

(defgeneric dmc-test-args-with-whole.3b (x)
  (:method-combination dmc-test-args-with-whole.3b)
  (:method (x) x))

(deftest dmc-test-args-with-whole.3b
    (dmc-test-args-with-whole.3b T)
  T)

(define-method-combination dmc-test-args-with-whole.3c ()
  ((methods ()))
  (:arguments &whole whole &optional opt &rest r)
  `(progn (format nil "using ~a ~a ~a" ,whole ,opt ,r)
          ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)))

(defgeneric dmc-test-args-with-whole.3c (x)
  (:method-combination dmc-test-args-with-whole.3c)
  (:method (x) x))

(deftest dmc-test-args-with-whole.3c
    (dmc-test-args-with-whole.3c T)
  T)


(define-method-combination dmc-test-args-with-whole.3d ()
  ((methods ()))
  (:arguments &whole whole &optional opt &rest r &key k)
  `(progn (format nil "using ~a ~a ~a ~a" ,whole ,opt ,r ,k)
          ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)))

(defgeneric dmc-test-args-with-whole.3d (x)
  (:method-combination dmc-test-args-with-whole.3d)
  (:method (x) x))

(deftest dmc-test-args-with-whole.3d
    (dmc-test-args-with-whole.3d T)
  T)

(define-method-combination dmc-test-args-with-whole.4 ()
  ((methods ()))
  (:arguments &whole whole &key k)
  `(progn (format nil "using ~a ~a" ,whole ,k)
          ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)))

(defgeneric dmc-test-args-with-whole.4 (x)
  (:method-combination dmc-test-args-with-whole.4)
  (:method (x) x))

(deftest dmc-test-args-with-whole.4
    (dmc-test-args-with-whole.4 T)
  T)

(define-method-combination dmc-test-args-with-whole.5 ()
  ((methods ()))
  (:arguments &whole whole &aux a)
  `(progn (format nil "using ~a ~a" ,whole ,a)
          ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)))

(defgeneric dmc-test-args-with-whole.5 (x)
  (:method-combination dmc-test-args-with-whole.5)
  (:method (x) x))

(deftest dmc-test-args-with-whole.5
    (dmc-test-args-with-whole.5 T)
  T)

(define-method-combination dmc-test-args-with-optional.1 ()
  ((methods ()))
  (:arguments &optional a)
  `(progn ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)
          ,a))

(defgeneric dmc-test-args-with-optional.1 (x &optional b)
  (:method-combination dmc-test-args-with-optional.1)
  (:method (x &optional b) (progn x b)))

(deftest dmc-test-args-with-optional.1a
    (dmc-test-args-with-optional.1 T)
  nil)

(deftest dmc-test-args-with-optional.1b
    (dmc-test-args-with-optional.1 T T)
  T)

(define-method-combination dmc-test-args-with-optional.2 ()
  ((methods *))
  (:arguments &optional (a :default))
  (print `(progn ,@(mapcar (lambda (method) `(call-method ,method))
                           methods)
                 ,a)))

(defgeneric dmc-test-args-with-optional.2 (x &optional b)
  (:method-combination dmc-test-args-with-optional.2)
  (:method (x &optional b) (progn x b)))

(deftest dmc-test-args-with-optional.2a
    :documentation "TODO"
    (dmc-test-args-with-optional.2 T)
  :default)

(deftest dmc-test-args-with-optional.2b
    :documentation "Describe what the test does here."
    (dmc-test-args-with-optional.2 T T)
  T)

(define-method-combination dmc-test-args-with-optional.3 ()
  ((methods *))
  (:arguments &optional (a :default))
  (print `(progn ,@(mapcar (lambda (method) `(call-method ,method))
                           methods)
                 ,a)))

(defgeneric dmc-test-args-with-optional.3 (x)
  (:method-combination dmc-test-args-with-optional.3)
  (:method (x) (progn x)))

(deftest dmc-test-args-with-optional.3
    :documentation "TODO"
    (dmc-test-args-with-optional.3 T)
  nil)


(define-method-combination dmc-test-args-with-optional.4 ()
  ((methods ()))
  (:arguments &optional (a :default sup-p))
  `(progn ,@(mapcar (lambda (method) `(call-method ,method))
                    methods)
          (values ,a ,sup-p)))

(defgeneric dmc-test-args-with-optional.4a (x &optional b)
  (:method-combination dmc-test-args-with-optional.4)
  (:method (x &optional b) (progn x b)))

(deftest dmc-test-args-with-optional.4a
    (dmc-test-args-with-optional.4a T)
  :default
  nil)

(deftest dmc-test-args-with-optional.4b
    (dmc-test-args-with-optional.4a T T)
  T
  T)

(defgeneric dmc-test-args-with-optional.4c (x)
  (:method-combination dmc-test-args-with-optional.4)
  (:method (x) (progn x)))

(deftest dmc-test-args-with-optional.4c
    :documentation "TODO"
    (dmc-test-args-with-optional.4c T)
  nil
  nil)