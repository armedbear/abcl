(in-package #:abcl.test.lisp)

;;; MAKE-INSTANCE initializes an instance through STD-SHARED-INITIALIZE
;;; directly while only the system-supplied methods of
;;; INITIALIZE-INSTANCE and SHARED-INITIALIZE apply to it.  These tests
;;; check that methods defined or removed later, and class
;;; redefinitions, are still honored.

(defclass mi-test-box ()
  ((width :initarg :width :initform 1)
   (height :initarg :height :initform 2)))

(defclass mi-test-sub (mi-test-box) ())

(deftest make-instance.initargs-and-initforms
    (let ((b (make-instance 'mi-test-box :width 10)))
      (list (slot-value b 'width) (slot-value b 'height)))
  (10 2))

(deftest make-instance.invalid-initarg
    (handler-case (progn (make-instance 'mi-test-box :no-such-initarg 1) :no-error)
      (program-error () :program-error))
  :program-error)

(deftest make-instance.after-method-added-later
    (progn
      ;; Warm the cache with no methods of our own.
      (make-instance 'mi-test-sub)
      (let ((method (eval '(defmethod initialize-instance :after
                               ((b mi-test-sub) &key (extra 0))
                             (setf (slot-value b 'height) (+ 100 extra))))))
        (unwind-protect
             (list (slot-value (make-instance 'mi-test-sub) 'height)
                   (slot-value (make-instance 'mi-test-sub :extra 5) 'height))
          (remove-method #'initialize-instance method))))
  (100 105))

(deftest make-instance.after-method-removed
    (progn
      (let ((method (eval '(defmethod initialize-instance :after ((b mi-test-sub) &key)
                             (setf (slot-value b 'width) :after)))))
        (make-instance 'mi-test-sub)
        (remove-method #'initialize-instance method))
      (list (slot-value (make-instance 'mi-test-sub) 'width)
            (handler-case (progn (make-instance 'mi-test-sub :extra 1) :no-error)
              (program-error () :program-error))))
  (1 :program-error))

;;; REMOVE-METHOD must forget the initargs the removed method allowed.
(deftest make-instance.initarg-of-removed-method
    (let ((method (eval '(defmethod initialize-instance :after
                             ((b mi-test-sub) &key removed-initarg)
                           (declare (ignore removed-initarg))))))
      (make-instance 'mi-test-sub :removed-initarg 1)
      (remove-method #'initialize-instance method)
      (handler-case (progn (make-instance 'mi-test-sub :removed-initarg 1) :no-error)
        (program-error () :program-error)))
  :program-error)

(deftest make-instance.shared-initialize-on-superclass
    (progn
      (make-instance 'mi-test-sub)
      (let ((method (eval '(defmethod shared-initialize :before
                               ((b mi-test-box) slot-names &key)
                             (declare (ignore slot-names))
                             (setf (slot-value b 'width) :before)))))
        (unwind-protect
             (list (slot-value (make-instance 'mi-test-sub) 'width)
                   (slot-value (make-instance 'mi-test-sub :width 3) 'width))
          (remove-method #'shared-initialize method))))
  (:before 3))

(deftest make-instance.class-redefined
    (progn
      (eval '(defclass mi-test-redefined () ((a :initform 1))))
      (make-instance 'mi-test-redefined)
      (eval '(defclass mi-test-redefined ()
              ((a :initform 1) (b :initarg :b :initform 2))))
      (let ((o (make-instance 'mi-test-redefined :b 3)))
        (list (slot-value o 'a) (slot-value o 'b))))
  (1 3))

;;; A metaclass whose SLOT-VALUE-USING-CLASS methods must see every
;;; slot that initialization writes.

(defclass mi-test-metaclass (standard-class) ())

(defmethod mop:validate-superclass ((class mi-test-metaclass)
                                    (superclass standard-class))
  t)

(defvar *mi-test-writes* nil)

(defmethod (setf mop:slot-value-using-class) :before
    (value (class mi-test-metaclass) object slot)
  (declare (ignore value object))
  (push (mop:slot-definition-name slot) *mi-test-writes*))

(defclass mi-test-metabox ()
  ((a :initarg :a :initform 1)
   (b :initform 2))
  (:metaclass mi-test-metaclass))

(deftest make-instance.slot-value-using-class
    (let ((*mi-test-writes* '()))
      (let ((o (make-instance 'mi-test-metabox :a 10)))
        (list (sort (copy-list *mi-test-writes*) #'string<)
              (slot-value o 'a) (slot-value o 'b))))
  ((a b) 10 2))
