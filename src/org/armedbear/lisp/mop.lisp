;;;; Does not currently include all the MOP, but it should.

(in-package #:mop)

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

;;; TODO Hook VALIDATE-SUPERCLASS into during class metaobject
;;; initialization and reinitialization. (AMOP p.240-1)
(defmethod validate-superclass ((class class) (superclass class))
  (or (eql (class-name superclass) t)
      (eql (class-name class) (class-name superclass))
      (or (and (eql (class-name class) 'standard-class)
               (eql (class-name superclass) 'funcallable-standard-class))
          (and (eql (class-name class) 'funcallable-standard-class)
               (eql (class-name superclass) 'standard-class)))))

(export '(;; classes
          funcallable-standard-object
          funcallable-standard-class
          forward-referenced-class
          direct-slot-definition-class
          effective-slot-definition-class
          standard-method
          standard-accessor-method
          standard-reader-method
          standard-writer-method
          
          compute-effective-slot-definition
          compute-class-precedence-list
          compute-effective-slot-definition
          compute-slots
          finalize-inheritance
          slot-boundp-using-class
          slot-makunbound-using-class
          validate-superclass

          ensure-class
          ensure-class-using-class

          class-default-initargs
          class-direct-default-initargs
          class-direct-slots
          class-direct-subclasses
          class-direct-superclasses
          class-finalized-p
          class-prototype
          
          generic-function-lambda-list
          generic-function-argument-precedence-order
          generic-function-method-class

          method-function
          method-generic-function
          method-lambda-list
          method-specializers
          method-qualifiers

          standard-reader-method
          standard-writer-method
          reader-method-class
          writer-method-class

          slot-definition
          slot-definition-readers
          slot-definition-writers
          slot-definition-location
          standard-instance-access

          intern-eql-specializer
          eql-specializer-object
          specializer-direct-methods
          specializer-direct-generic-functions
          add-direct-method
          remove-direct-method

          extract-lambda-list
          extract-specializer-names

          add-dependent
          remove-dependent
          map-dependents
          update-dependent))

(provide 'mop)





