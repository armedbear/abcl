(in-package #:abcl.test.lisp)

;;; EXT:COMPARE-AND-SWAP and the ATOMIC- macros
;;; <https://github.com/armedbear/abcl/issues/92>

(defstruct cas-test-struct
  (a 0)
  (b 0 :type fixnum)
  (c 0 :read-only t))

(defclass cas-test-class ()
  ((a :initform 3)
   (shared :allocation :class :initform 0)
   (unbound)))

(defvar *cas-test-special* 10)

(deftest cas.implementation
    (and (member (ext:compare-and-swap-implementation) '(:varhandle :lock))
         t)
  t)

(deftest cas.car.1
    (let ((c (list 1 2)))
      (values (ext:compare-and-swap (car c) 1 :one) c))
  1 (:one 2))

(deftest cas.car.2
    (let ((c (list 1 2)))
      (values (ext:compare-and-swap (car c) 5 :five) c))
  1 (1 2))

;;; Fixnums outside the cache are fresh objects, so EQ fails where
;;; EQL holds; COMPARE-AND-SWAP compares with EQL and returns OLD
;;; itself on success.
(deftest cas.car.eql
    (let* ((old (+ 1000 (length '(a))))
           (c (list (+ 1000 (length '(b))))))
      (values (eq (ext:compare-and-swap (car c) old :new) old) c))
  t (:new))

(deftest cas.car.type-error
    (handler-case (ext:compare-and-swap (car (identity :not-a-cons)) 1 2)
      (type-error () :type-error))
  :type-error)

(deftest cas.cdr
    (let ((c (cons 1 2)))
      (values (ext:compare-and-swap (cdr c) 2 :two)
              (ext:compare-and-swap (cdr c) 2 :again)
              c))
  2 :two (1 . :two))

(deftest cas.first-rest
    (let ((c (list 1 2)))
      (ext:compare-and-swap (first c) 1 :a)
      (ext:compare-and-swap (rest c) (cdr c) :b)
      c)
  (:a . :b))

(deftest cas.svref
    (let ((v (vector 1 2 3)))
      (values (ext:compare-and-swap (svref v 1) 2 :two)
              (ext:compare-and-swap (svref v 1) 2 :again)
              v))
  2 :two #(1 :two 3))

(deftest cas.svref.bounds
    (handler-case (ext:compare-and-swap (svref (vector 1 2 3) 3) 1 2)
      (error () :error))
  :error)

(deftest cas.structure
    (let ((s (make-cas-test-struct)))
      (values (ext:compare-and-swap (cas-test-struct-a s) 0 :a)
              (ext:compare-and-swap (cas-test-struct-a s) 0 :again)
              (cas-test-struct-a s)
              (ext:compare-and-swap (cas-test-struct-b s) 0 5)
              (cas-test-struct-b s)))
  0 :a :a 0 5)

(deftest cas.structure.read-only
    (handler-case
        (macroexpand '(ext:compare-and-swap (cas-test-struct-c s) 0 1))
      (error () :error))
  :error)

(deftest cas.symbol-value.global
    (let ((old *cas-test-special*))
      (prog1
          (list (ext:compare-and-swap (symbol-value '*cas-test-special*)
                                      old :new)
                *cas-test-special*
                (ext:compare-and-swap *cas-test-special* old :again)
                *cas-test-special*)
        (setf *cas-test-special* old)))
  (10 :new :new :new))

(deftest cas.symbol-value.dynamic
    (list (let ((*cas-test-special* 1))
            (list (ext:compare-and-swap *cas-test-special* 1 2)
                  *cas-test-special*))
          *cas-test-special*)
  ((1 2) 10))

(deftest cas.slot-value
    (let ((o (make-instance 'cas-test-class)))
      (values (ext:compare-and-swap (slot-value o 'a) 3 :a)
              (slot-value o 'a)
              (ext:compare-and-swap (slot-value o 'shared) 0 1)
              (slot-value (make-instance 'cas-test-class) 'shared)
              (handler-case (ext:compare-and-swap (slot-value o 'unbound) 0 1)
                (unbound-slot () :unbound))))
  3 :a 0 1 :unbound)

(deftest cas.standard-instance-access
    (let* ((o (make-instance 'cas-test-class))
           (location (mop:slot-definition-location
                      (find 'a (mop:class-slots (class-of o))
                            :key #'mop:slot-definition-name))))
      (values (ext:compare-and-swap (mop:standard-instance-access o location)
                                    3 :a)
              (slot-value o 'a)))
  3 :a)

(defun cas-test-second (list)
  (second list))

(ext:defcas cas-test-second (list) cas-test-cas-second)

(defun cas-test-cas-second (list old new)
  (ext:compare-and-swap (car (cdr list)) old new))

(deftest cas.defcas
    (let ((l (list 1 2 3)))
      (values (ext:compare-and-swap (cas-test-second l) 2 :two) l))
  2 (1 :two 3))

(ext:define-cas-expander cas-test-third (list)
  (let ((l (gensym)) (old (gensym)) (new (gensym)))
    (values (list l) (list list) old new
            `(ext:compare-and-swap (car (cddr ,l)) ,old ,new)
            `(third ,l))))

(deftest cas.define-cas-expander
    (let ((l (list 1 2 3)))
      (values (ext:compare-and-swap (cas-test-third l) 3 :three)
              (ext:atomic-incf (cas-test-third (list 1 2 3)))
              l))
  3 3 (1 2 :three))

(deftest cas.compiled
    (let ((f (compile nil '(lambda (c old new)
                            (ext:compare-and-swap (car c) old new))))
          (c (list 1)))
      (values (funcall f c 1 2) (funcall f c 1 3) c))
  1 2 (2))

(deftest cas.atomic-incf-decf
    (let ((c (list 10)))
      (values (ext:atomic-incf (car c))
              (ext:atomic-incf (car c) 5)
              (ext:atomic-decf (car c))
              (ext:atomic-decf (car c) 3)
              c))
  10 11 16 15 (12))

(deftest cas.atomic-push-pop
    (let ((c (list nil)))
      (values (ext:atomic-push 1 (car c))
              (ext:atomic-push 2 (car c))
              (ext:atomic-pop (car c))
              c))
  (1) (2 1) 2 ((1)))

(deftest cas.atomic-update
    (let ((s (make-cas-test-struct :a (list 1))))
      (values (ext:atomic-update (cas-test-struct-a s) #'cons 0)
              (ext:atomic-update (cas-test-struct-b s) #'+ 5)
              (cas-test-struct-b s)))
  (0 1) 5 5)

;;; Contention: N threads each make M updates to one place; none may
;;; be lost.

(defun cas-test-contend (n m fn)
  "Run FN M times in each of N threads, released together; return
when all have finished."
  (let* ((go (list nil))
         (threads
           (loop repeat n
                 collect (threads:make-thread
                          (lambda ()
                            (loop until (car go))
                            (dotimes (i m) (funcall fn)))))))
    (setf (car go) t)
    (mapc #'threads:thread-join threads)))

(deftest cas.contention.atomic-incf
    (let ((c (list 0)))
      (cas-test-contend 4 50000 (lambda () (ext:atomic-incf (car c))))
      (car c))
  200000)

(deftest cas.contention.compare-and-swap
    (let ((s (make-cas-test-struct)))
      (cas-test-contend 4 50000
                        (lambda ()
                          (loop for old = (cas-test-struct-b s)
                                until (eq old (ext:compare-and-swap
                                               (cas-test-struct-b s)
                                               old (1+ old))))))
      (cas-test-struct-b s))
  200000)

(deftest cas.contention.svref
    (let ((v (vector 0 0)))
      (cas-test-contend 4 50000
                        (lambda ()
                          (ext:atomic-incf (svref v 0))
                          (ext:atomic-decf (svref v 1))))
      v)
  #(200000 -200000))

(deftest cas.contention.atomic-push
    (let ((c (list nil)))
      (cas-test-contend 4 25000 (lambda () (ext:atomic-push :x (car c))))
      (length (car c)))
  100000)
