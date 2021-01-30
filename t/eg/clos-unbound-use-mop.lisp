(use-package :mop)

(defclass test-direct-slot-definition
          (standard-direct-slot-definition)
  ())

(defclass test-effective-slot-definition
          (standard-effective-slot-definition)
  ())

(defclass test-class (standard-class) ())

(defmethod validate-superclass
           ((class test-class)
            (superclass standard-class))
  t)

(defmethod direct-slot-definition-class
           ((class test-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'test-direct-slot-definition))

(defmethod effective-slot-definition-class
           ((class test-class) &rest initargs)
  (declare (ignore initargs))
  (find-class 'test-effective-slot-definition))

(defclass test-object ()
  ((some-slot :accessor some-slot
              :initarg :some-slot
              :initform 'some-slot
              :type symbol
              :allocation :class
              :documentation "a slot"))
  (:metaclass test-class))

(unless (mop:class-finalized-p (find-class 'test-object))
  (finalize-inheritance (find-class 'test-object)))

