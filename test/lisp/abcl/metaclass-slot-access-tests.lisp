(in-package #:abcl.test.lisp)

;;; Under a metaclass for which no SLOT-VALUE-USING-CLASS (or SETF,
;;; BOUNDP, MAKUNBOUND) method but the system's may apply, SLOT-VALUE
;;; and the rest read the slots directly, as for a STANDARD-CLASS.  These
;;; tests check that any method that does apply is still called, however
;;; it is specialized, and also when it is defined after instances have
;;; been used.

(defclass msa-metaclass (standard-class) ())
(defmethod mop:validate-superclass ((class msa-metaclass) (superclass standard-class))
  t)

(defclass msa-other-metaclass (standard-class) ())
(defmethod mop:validate-superclass ((class msa-other-metaclass) (superclass standard-class))
  t)

(defclass msa-box ()
  ((a :initarg :a :accessor msa-box-a :initform 1)
   (b :accessor msa-box-b)
   (shared :allocation :class :accessor msa-box-shared :initform :shared))
  (:metaclass msa-metaclass))

(defclass msa-sub-box (msa-box) ()
  (:metaclass msa-metaclass))

(defvar *msa-calls* nil)

(deftest metaclass-slot-access.basic
    (let ((box (make-instance 'msa-box :a 10)))
      (list (mop::standard-slot-access-p (find-class 'msa-box))
            (slot-value box 'a)
            (msa-box-a box)
            (progn (setf (msa-box-a box) 20) (slot-value box 'a))
            (slot-boundp box 'b)
            (handler-case (msa-box-b box) (unbound-slot () :unbound))
            (progn (setf (slot-value box 'b) 2) (msa-box-b box))
            (progn (slot-makunbound box 'b) (slot-boundp box 'b))
            (msa-box-shared box)
            (handler-case (slot-value box 'no-such-slot)
              (error () :missing))))
  (t 10 10 20 nil :unbound 2 nil :shared :missing))

(defmacro with-temporary-method ((gf &rest defmethod-args) &body body)
  `(let ((method (eval '(defmethod ,@defmethod-args))))
     (unwind-protect (progn ,@body)
       (remove-method ,gf method))))

(deftest metaclass-slot-access.method-on-metaclass
    (let ((box (make-instance 'msa-box)))
      (slot-value box 'a)               ; use the direct path first
      (list (with-temporary-method (#'mop:slot-value-using-class
                                    mop:slot-value-using-class :around
                                    ((class msa-metaclass) object slot)
                                    (declare (ignore object slot))
                                    (list :around (call-next-method)))
              (list (mop::standard-slot-access-p (find-class 'msa-box))
                    (slot-value box 'a)
                    (msa-box-a box)))
            (mop::standard-slot-access-p (find-class 'msa-box))
            (slot-value box 'a)))
  ((nil (:around 1) (:around 1)) t 1))

(deftest metaclass-slot-access.method-on-instance-class
    (let ((box (make-instance 'msa-sub-box)))
      (slot-value box 'a)
      (with-temporary-method (#'(setf mop:slot-value-using-class)
                              (setf mop:slot-value-using-class) :before
                              (new (class t) (object msa-sub-box) slot)
                              (declare (ignore class object slot))
                              (push new *msa-calls*))
        (let ((*msa-calls* nil))
          (setf (slot-value box 'a) 5)
          (setf (msa-box-a box) 6)
          (list (mop::standard-slot-access-p (find-class 'msa-box))
                (mop::standard-slot-access-p (find-class 'msa-sub-box))
                *msa-calls*))))
  (t nil (6 5)))

(deftest metaclass-slot-access.method-on-boundp-and-makunbound
    (let ((box (make-instance 'msa-box)))
      (list
       (with-temporary-method (#'mop:slot-boundp-using-class
                               mop:slot-boundp-using-class
                               ((class msa-metaclass) object slot)
                               (declare (ignore object slot))
                               :boundp-method)
         (slot-boundp box 'b))
       (let ((*msa-calls* nil))
         (with-temporary-method (#'mop:slot-makunbound-using-class
                                 mop:slot-makunbound-using-class :before
                                 ((class msa-metaclass) object slot)
                                 (declare (ignore class object))
                                 (push (mop:slot-definition-name slot) *msa-calls*))
           (slot-makunbound box 'a)
           *msa-calls*))))
  (:boundp-method (a)))

(deftest metaclass-slot-access.unrelated-method
    ;; A method for another metaclass leaves the direct path in place.
    (with-temporary-method (#'mop:slot-value-using-class
                            mop:slot-value-using-class :around
                            ((class msa-other-metaclass) object slot)
                            (declare (ignore object slot))
                            (list :other (call-next-method)))
      (list (mop::standard-slot-access-p (find-class 'msa-box))
            (slot-value (make-instance 'msa-box) 'a)))
  (t 1))

;;; A method specialized on a custom effective slot definition class.

(defclass msa-slot-metaclass (standard-class) ())
(defmethod mop:validate-superclass ((class msa-slot-metaclass) (superclass standard-class))
  t)
(defclass msa-effective-slot (mop:standard-effective-slot-definition) ())
(defmethod mop:effective-slot-definition-class ((class msa-slot-metaclass) &rest initargs)
  (declare (ignore initargs))
  (find-class 'msa-effective-slot))

(defclass msa-slot-box ()
  ((a :initform 1 :accessor msa-slot-box-a))
  (:metaclass msa-slot-metaclass))

(deftest metaclass-slot-access.method-on-slot-definition-class
    (let ((box (make-instance 'msa-slot-box)))
      (list (msa-slot-box-a box)
            (with-temporary-method (#'mop:slot-value-using-class
                                    mop:slot-value-using-class :around
                                    (class object (slot msa-effective-slot))
                                    (declare (ignore class object))
                                    (list :slot (call-next-method)))
              (list (mop::standard-slot-access-p (find-class 'msa-slot-box))
                    (msa-slot-box-a box)
                    (slot-value box 'a)))
            (msa-slot-box-a box)))
  (1 (nil (:slot 1) (:slot 1)) 1))
