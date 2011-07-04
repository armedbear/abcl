;;;; Does not currently include all the MOP, but it should.

(in-package #:mop)

(defclass funcallable-standard-class (class))

(defmethod class-name ((class funcallable-standard-class))
  'funcallable-standard-class)

;;; StandardGenericFunction.java defines FUNCALLABLE-INSTANCE-FUNCTION and
;;; SET-FUNCALLABLE-INSTANCE-FUNCTION.
;;;
;;; TODO 
;;;
;;;   1. Verify that we can make FUNCALLABLE-STANDARD-CLASS instances
;;;      which work.
;;;
;;;   2. Tighten the type checks so that only instances of
;;;      FUNCALLABLE-STANDARD-CLASS are callable.

(defgeneric validate-superclass (class superclass)
 (:documentation 
  "This generic function is called to determine whether the class
  superclass is suitable for use as a superclass of class."))

(defmethod validate-superclass ((class class) (superclass class))
  (or (eql (class-name superclass) t)
      (eql (class-name class) (class-name superclass))
      (or (and (eql (class-name class) 'standard-class)
               (eql (class-name superclass) 'funcallable-standard-class))
          (and (eql (class-name class) 'funcallable-standard-class)
               (eql (class-name superclass) 'standard-class)))))

(export '(funcallable-standard-class
          validate-superclass))

(provide 'mop)





