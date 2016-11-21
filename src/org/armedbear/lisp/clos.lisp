;;; clos.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves
;;; Copyright (C) 2010-2013 Mark Evenson
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
;;; Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
;;;
;;; As a special exception, the copyright holders of this library give you
;;; permission to link this library with independent modules to produce an
;;; executable, regardless of the license terms of these independent
;;; modules, and to copy and distribute the resulting executable under
;;; terms of your choice, provided that you also meet, for each linked
;;; independent module, the terms and conditions of the license of that
;;; module.  An independent module is a module which is not derived from
;;; or based on this library.  If you modify this library, you may extend
;;; this exception to your version of the library, but you are not
;;; obligated to do so.  If you do not wish to do so, delete this
;;; exception statement from your version.

;;; Originally based on Closette.

;;; Closette Version 1.0 (February 10, 1991)
;;;
;;; Copyright (c) 1990, 1991 Xerox Corporation.
;;; All rights reserved.
;;;
;;; Use and copying of this software and preparation of derivative works
;;; based upon this software are permitted.  Any distribution of this
;;; software or derivative works must comply with all applicable United
;;; States export control laws.
;;;
;;; This software is made available AS IS, and Xerox Corporation makes no
;;; warranty about the software, its performance or its conformity to any
;;; specification.
;;;
;;; Closette is an implementation of a subset of CLOS with a metaobject
;;; protocol as described in "The Art of The Metaobject Protocol",
;;; MIT Press, 1991.

(in-package #:mop)

(export '(%defgeneric canonicalize-direct-superclasses))


;;
;;
;;
;; In order to bootstrap CLOS, first implement the required API as
;; normal functions which only apply to the "root" metaclass
;; STANDARD-CLASS.
;;
;; After putting the normal functions in place, the building blocks
;; are in place to gradually swap the normal functions with
;; generic functions and methods.
;;
;; Some functionality implemented in the temporary regular functions
;; needs to be available later as a method definition to be dispatched
;; to for the standard case, e.g. with arguments of type STANDARD-CLASS
;; or STANDARD-GENERIC-FUNCTION.  To prevent repeated code, the
;; functions are implemented in functions by the same name as the API
;; functions, but with the STD- prefix.  These functions are sometimes
;; used in regular code as well, either in a "fast path" or to break a
;; circularity (e.g., within compute-discriminating-function when the
;; user adds a method to compute-discriminating-function).
;;
;; When hacking this file, note that some important parts are implemented
;; in the Java world. These Java bits can be found in the files
;;
;; * LispClass.java
;; * SlotClass.java
;; * StandardClass.java
;; * BuiltInClass.java
;; * StandardObject.java
;; * StandardObjectFunctions.java
;; * FuncallableStandardObject.java
;; * Layout.java
;;
;; In case of function names, those defined on the Java side can be
;; recognized by their prefixed percent (%) sign.
;;
;; The API functions need to be declaimed NOTINLINE explicitly, because
;; that prevents inlining in the current FASL (which is allowed by the
;; CLHS without the declaration); this is a hard requirement to in order
;; to be able to swap the symbol's function slot with a generic function
;; later on - with it actually being used.
;;
;;
;;
;; ### Note that the "declares all API functions as regular functions"
;; isn't true when I write the above, but it's definitely the target.
;;
;; A note about AMOP: the first chapters (and the sample Closette
;; implementation) of the book sometimes deviate from the specification.
;; For example, in the examples slot-value-using-class has the slot name
;; as third argument where in the specification it is the effective slot
;; definition.  When in doubt, we aim to follow the specification, the
;; MOP test suite at http://common-lisp.net/project/closer/features.html
;; and the behavior of other CL implementations in preference to
;; chapters 1-4 and appendix D.

(defconstant +the-standard-class+ (find-class 'standard-class))
(defconstant +the-funcallable-standard-class+
  (find-class 'funcallable-standard-class))
(defconstant +the-standard-object-class+ (find-class 'standard-object))
(defconstant +the-funcallable-standard-object-class+
  (find-class 'funcallable-standard-object))
(defconstant +the-standard-method-class+ (find-class 'standard-method))
(defconstant +the-T-class+ (find-class 'T))
(defconstant +the-standard-slot-definition-class+ (find-class 'standard-slot-definition))
(defconstant +the-standard-direct-slot-definition-class+ (find-class 'standard-direct-slot-definition))
(defconstant +the-standard-effective-slot-definition-class+ (find-class 'standard-effective-slot-definition))

;; Don't use DEFVAR, because that disallows loading clos.lisp
;; after compiling it: the binding won't get assigned to T anymore
(defparameter *clos-booting* t)

(defmacro define-class->%class-forwarder (name)
  (let* (($name (if (consp name) (cadr name) name))
         (%name (intern (concatenate 'string
                                     "%"
                                     (if (consp name)
                                         (symbol-name 'set-) "")
                                     (symbol-name $name))
                        (symbol-package $name))))
    `(progn
       (declaim (notinline ,name))
       (defun ,name (&rest args)
         (apply #',%name args)))))

;;
;;  DEFINE PLACE HOLDER FUNCTIONS
;;

(define-class->%class-forwarder class-name)
(define-class->%class-forwarder (setf class-name))
(define-class->%class-forwarder class-slots)
(define-class->%class-forwarder (setf class-slots))
(define-class->%class-forwarder class-direct-slots)
(define-class->%class-forwarder (setf class-direct-slots))
(define-class->%class-forwarder class-layout)
(define-class->%class-forwarder (setf class-layout))
(define-class->%class-forwarder class-direct-superclasses)
(define-class->%class-forwarder (setf class-direct-superclasses))
(define-class->%class-forwarder class-direct-subclasses)
(define-class->%class-forwarder (setf class-direct-subclasses))
(define-class->%class-forwarder class-direct-methods)
(define-class->%class-forwarder (setf class-direct-methods))
(define-class->%class-forwarder class-precedence-list)
(define-class->%class-forwarder (setf class-precedence-list))
(define-class->%class-forwarder class-finalized-p)
(define-class->%class-forwarder (setf class-finalized-p))
(define-class->%class-forwarder class-default-initargs)
(define-class->%class-forwarder (setf class-default-initargs))
(define-class->%class-forwarder class-direct-default-initargs)
(define-class->%class-forwarder (setf class-direct-default-initargs))

(declaim (notinline add-direct-subclass remove-direct-subclass))
(defun add-direct-subclass (superclass subclass)
  (setf (class-direct-subclasses superclass)
        (adjoin subclass (class-direct-subclasses superclass))))
(defun remove-direct-subclass (superclass subclass)
  (setf (class-direct-subclasses superclass)
        (remove subclass (class-direct-subclasses superclass))))

(defun fixup-standard-class-hierarchy ()
  ;; Make the result of class-direct-subclasses for the pre-built
  ;; classes agree with AMOP Table 5.1 (pg. 141).  This could be done in
  ;; StandardClass.java where these classes are defined, but it's less
  ;; painful to do it Lisp-side.
  (flet ((add-subclasses (class subclasses)
           (when (atom subclasses) (setf subclasses (list subclasses)))
           (setf (class-direct-subclasses (find-class class))
                 (union (class-direct-subclasses (find-class class))
                        (mapcar #'find-class subclasses)))))
    (add-subclasses t 'standard-object)
    (add-subclasses 'function 'funcallable-standard-object)
    (add-subclasses 'standard-object '(funcallable-standard-object metaobject))
    (add-subclasses 'metaobject
                    '(method slot-definition specializer))
    (add-subclasses 'specializer '(class))
    (add-subclasses 'method 'standard-method)
    (add-subclasses 'slot-definition
                    '(direct-slot-definition effective-slot-definition
                      standard-slot-definition))
    (add-subclasses 'standard-slot-definition
                    '(standard-direct-slot-definition
                      standard-effective-slot-definition))
    (add-subclasses 'direct-slot-definition 'standard-direct-slot-definition)
    (add-subclasses 'effective-slot-definition
                    'standard-effective-slot-definition)
    (add-subclasses 'class
                    '(built-in-class standard-class funcallable-standard-class))))
(fixup-standard-class-hierarchy)

(defun std-class-p (class)
  (let ((metaclass (class-of class)))
    (or (eq metaclass +the-standard-class+)
        (eq metaclass +the-funcallable-standard-class+))))

(defun no-applicable-method (generic-function &rest args)
  (error "There is no applicable method for the generic function ~S when called with arguments ~S."
         generic-function
         args))

(defun function-keywords (method)
  (std-function-keywords method))

(declaim (notinline map-dependents))
(defun map-dependents (metaobject function)
  ;; stub, will be redefined later
  (declare (ignore metaobject function))
  nil)

(defmacro push-on-end (value location)
  `(setf ,location (nconc ,location (list ,value))))

;;; (SETF GETF*) is like (SETF GETF) except that it always changes the list,
;;; which must be non-nil.

(defun (setf getf*) (new-value plist key)
  (block body
    (do ((x plist (cddr x)))
        ((null x))
      (when (eq (car x) key)
        (setf (car (cdr x)) new-value)
        (return-from body new-value)))
    (push-on-end key plist)
    (push-on-end new-value plist)
    new-value))

(defun mapappend (fun &rest args)
  (if (some #'null args)
      ()
      (append (apply fun (mapcar #'car args))
              (apply #'mapappend fun (mapcar #'cdr args)))))

(defun mapplist (fun x)
  (if (null x)
      ()
      (cons (funcall fun (car x) (cadr x))
            (mapplist fun (cddr x)))))

(defsetf std-slot-value set-std-slot-value)

(defsetf std-instance-layout %set-std-instance-layout)
(defsetf standard-instance-access %set-standard-instance-access)
(defun funcallable-standard-instance-access (instance location)
  (standard-instance-access instance location))
(defsetf funcallable-standard-instance-access %set-standard-instance-access)

(defun (setf find-class) (new-value symbol &optional errorp environment)
  (declare (ignore errorp environment))
  (%set-find-class symbol new-value))

(defun canonicalize-direct-slots (direct-slots)
  `(list ,@(mapcar #'canonicalize-direct-slot direct-slots)))

(defun canonicalize-direct-slot (spec)
  (if (symbolp spec)
      `(list :name ',spec)
      (let ((name (car spec))
            (initfunction nil)
            (initform nil)
            (initargs ())
            (type nil)
            (allocation nil)
            (documentation nil)
            (readers ())
            (writers ())
            (other-options ())
            (non-std-options ()))
        (do ((olist (cdr spec) (cddr olist)))
            ((null olist))
          (case (car olist)
            (:initform
             (when initform
               (error 'program-error
                      "duplicate slot option :INITFORM for slot named ~S"
                      name))
             (setq initfunction t)
             (setq initform (cadr olist)))
            (:initarg
             (push-on-end (cadr olist) initargs))
            (:allocation
             (when allocation
               (error 'program-error
                      "duplicate slot option :ALLOCATION for slot named ~S"
                      name))
             (setf allocation (cadr olist))
             (push-on-end (car olist) other-options)
             (push-on-end (cadr olist) other-options))
            (:type
             (when type
               (error 'program-error
                      "duplicate slot option :TYPE for slot named ~S"
                      name))
             (setf type (cadr olist)))
            (:documentation
             (when documentation
               (error 'program-error
                      "duplicate slot option :DOCUMENTATION for slot named ~S"
                      name))
             (setf documentation (cadr olist)))
            (:reader
             (maybe-note-name-defined (cadr olist))
             (push-on-end (cadr olist) readers))
            (:writer
             (maybe-note-name-defined (cadr olist))
             (push-on-end (cadr olist) writers))
            (:accessor
             (maybe-note-name-defined (cadr olist))
             (push-on-end (cadr olist) readers)
             (push-on-end `(setf ,(cadr olist)) writers))
            (t
             (push-on-end (cadr olist) (getf non-std-options (car olist))))))
        `(list
          :name ',name
          ,@(when initfunction
              `(:initform ',initform
                :initfunction ,(if (eq allocation :class)
                                   ;; CLHS specifies the initform for a
                                   ;; class allocation level slot needs
                                   ;; to be evaluated in the dynamic
                                   ;; extent of the DEFCLASS form
                                   (let ((var (gensym)))
                                     `(let ((,var ,initform))
                                        (lambda () ,var)))
                                 `(lambda () ,initform))))
          ,@(when initargs `(:initargs ',initargs))
          ,@(when readers `(:readers ',readers))
          ,@(when writers `(:writers ',writers))
          ,@(when type `(:type ',type))
          ,@(when documentation `(:documentation ',documentation))
          ,@other-options
          ,@(mapcar #'(lambda (opt) (if (or (atom opt) (/= 1 (length opt)))
                                        `',opt
                                        `',(car opt)))
                    non-std-options)))))

(defun maybe-note-name-defined (name)
  (when (fboundp 'note-name-defined)
    (note-name-defined name)))

(defun canonicalize-defclass-options (options)
  (mapappend #'canonicalize-defclass-option options))

(defun canonicalize-defclass-option (option)
  (case (car option)
    (:metaclass
     (list ':metaclass
           `(find-class ',(cadr option))))
    (:default-initargs
     (list
      ':direct-default-initargs
      `(list ,@(mapplist
                #'(lambda (key value)
                    `(list ',key ',value ,(make-initfunction value)))
                (cdr option)))))
    ((:documentation :report)
     (list (car option) `',(cadr option)))
    (t (list `(quote ,(car option)) `(quote ,(cdr option))))))

(defun make-initfunction (initform)
  `(function (lambda () ,initform)))

(defun slot-definition-allocation (slot-definition)
  (std-slot-value slot-definition 'sys::allocation))

(declaim (notinline (setf slot-definition-allocation)))
(defun (setf slot-definition-allocation) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::allocation) value))

(defun slot-definition-initargs (slot-definition)
  (std-slot-value slot-definition 'sys::initargs))

(declaim (notinline (setf slot-definition-initargs)))
(defun (setf slot-definition-initargs) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::initargs) value))

(defun slot-definition-initform (slot-definition)
  (std-slot-value slot-definition 'sys::initform))

(declaim (notinline (setf slot-definition-initform)))
(defun (setf slot-definition-initform) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::initform) value))

(defun slot-definition-initfunction (slot-definition)
  (std-slot-value slot-definition 'sys::initfunction))

(declaim (notinline (setf slot-definition-initfunction)))
(defun (setf slot-definition-initfunction) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::initfunction) value))

(defun slot-definition-name (slot-definition)
  (std-slot-value slot-definition 'sys:name))

(declaim (notinline (setf slot-definition-name)))
(defun (setf slot-definition-name) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys:name) value))

(defun slot-definition-readers (slot-definition)
  (std-slot-value slot-definition 'sys::readers))

(declaim (notinline (setf slot-definition-readers)))
(defun (setf slot-definition-readers) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::readers) value))

(defun slot-definition-writers (slot-definition)
  (std-slot-value slot-definition 'sys::writers))

(declaim (notinline (setf slot-definition-writers)))
(defun (setf slot-definition-writers) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::writers) value))

(defun slot-definition-allocation-class (slot-definition)
  (std-slot-value slot-definition 'sys::allocation-class))

(declaim (notinline (setf slot-definition-allocation-class)))
(defun (setf slot-definition-allocation-class) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::allocation-class) value))

(defun slot-definition-location (slot-definition)
  (std-slot-value slot-definition 'sys::location))

(declaim (notinline (setf slot-definition-location-class)))
(defun (setf slot-definition-location) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::location) value))

(defun slot-definition-type (slot-definition)
  (std-slot-value slot-definition 'sys::%type))

(declaim (notinline (setf slot-definition-type)))
(defun (setf slot-definition-type) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys::%type) value))

(defun slot-definition-documentation (slot-definition)
  (std-slot-value slot-definition 'sys:%documentation))

(declaim (notinline (setf slot-definition-documentation)))
(defun (setf slot-definition-documentation) (value slot-definition)
  (setf (std-slot-value slot-definition 'sys:%documentation) value))

(defun init-slot-definition (slot &key name
                                    (initargs ())
                                    (initform nil)
                                    (initfunction nil)
                                    (readers ())
                                    (writers ())
                                    (allocation :instance)
                                    (allocation-class nil)
                                    (type t)
                                    (documentation nil))
  (setf (slot-definition-name slot) name)
  (setf (slot-definition-initargs slot) initargs)
  (setf (slot-definition-initform slot) initform)
  (setf (slot-definition-initfunction slot) initfunction)
  (setf (slot-definition-readers slot) readers)
  (setf (slot-definition-writers slot) writers)
  (setf (slot-definition-allocation slot) allocation)
  (setf (slot-definition-allocation-class slot) allocation-class)
  (setf (slot-definition-type slot) type)
  (setf (slot-definition-documentation slot) documentation)
  slot)

(declaim (notinline direct-slot-definition-class))
(defun direct-slot-definition-class (class &rest args)
  (declare (ignore class args))
  +the-standard-direct-slot-definition-class+)

(defun make-direct-slot-definition (class &rest args)
  (let ((slot-class (apply #'direct-slot-definition-class class args)))
    (if (eq slot-class +the-standard-direct-slot-definition-class+)
        (let ((slot (%make-slot-definition +the-standard-direct-slot-definition-class+)))
          (apply #'init-slot-definition slot :allocation-class class args)
          slot)
        (progn
          (let ((slot (apply #'make-instance slot-class :allocation-class class
                             args)))
            slot)))))

(declaim (notinline effective-slot-definition-class))
(defun effective-slot-definition-class (class &rest args)
  (declare (ignore class args))
  +the-standard-effective-slot-definition-class+)

(defun make-effective-slot-definition (class &rest args)
  (let ((slot-class (apply #'effective-slot-definition-class class args)))
    (if (eq slot-class +the-standard-effective-slot-definition-class+)
        (let ((slot (%make-slot-definition +the-standard-effective-slot-definition-class+)))
          (apply #'init-slot-definition slot args)
          slot)
        (progn
          (let ((slot (apply #'make-instance slot-class args)))
            slot)))))

;;; finalize-inheritance

(declaim (notinline compute-default-initargs))
(defun compute-default-initargs (class)
  (std-compute-default-initargs class))

(defun std-compute-default-initargs (class)
  (delete-duplicates
   (mapcan #'(lambda (c)
               (copy-list
                (class-direct-default-initargs c)))
           (class-precedence-list class))
   :key #'car :from-end t))

(defun std-finalize-inheritance (class)
  ;; In case the class is already finalized, return
  ;; immediately, as per AMOP.
  (when (class-finalized-p class)
    (return-from std-finalize-inheritance))
  (setf (class-precedence-list class)
        (funcall (if (std-class-p class)
                     #'std-compute-class-precedence-list
                     #'compute-class-precedence-list)
                 class))
  (setf (class-slots class)
        (funcall (if (std-class-p class)
                     #'std-compute-slots
                     #'compute-slots) class))
  (let ((old-layout (class-layout class))
        (length 0)
        (instance-slots '())
        (shared-slots '()))
    (dolist (slot (class-slots class))
      (case (slot-definition-allocation slot)
        (:instance
         (setf (slot-definition-location slot) length)
         (incf length)
         (push (slot-definition-name slot) instance-slots))
        (:class
         (unless (slot-definition-location slot)
           (let ((allocation-class (slot-definition-allocation-class slot)))
             (if (eq allocation-class class)
                 ;; We initialize class slots here so they can be
                 ;; accessed without creating a dummy instance.
                 (let ((initfunction (slot-definition-initfunction slot)))
                   (setf (slot-definition-location slot)
                         (cons (slot-definition-name slot)
                               (if initfunction
                                   (funcall initfunction)
                                   +slot-unbound+))))
                 (setf (slot-definition-location slot)
                       (slot-location allocation-class (slot-definition-name slot))))))
         (push (slot-definition-location slot) shared-slots))))
    (when old-layout
      ;; Redefined class: initialize added shared slots.
      (dolist (location shared-slots)
        (let* ((slot-name (car location))
               (old-location (layout-slot-location old-layout slot-name)))
          (unless old-location
            (let* ((slot-definition (find slot-name (class-slots class) :key 'slot-definition-name))
                   (initfunction (slot-definition-initfunction slot-definition)))
              (when initfunction
                (setf (cdr location) (funcall initfunction))))))))
    (setf (class-layout class)
          (make-layout class (nreverse instance-slots) (nreverse shared-slots))))
  (setf (class-default-initargs class)
        (compute-default-initargs class))
  (setf (class-finalized-p class) t))

(declaim (notinline finalize-inheritance))
(defun finalize-inheritance (class)
  (std-finalize-inheritance class))


;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
    (dolist (super classes-to-order)
      (when (typep super 'forward-referenced-class)
        (error "Can't compute class precedence list for class ~A ~
                which depends on forward referenced class ~A." class super)))
    (topological-sort classes-to-order
                      (remove-duplicates
                       (mapappend #'local-precedence-ordering
                                  classes-to-order))
                      #'std-tie-breaker-rule)))

;;; topological-sort implements the standard algorithm for topologically
;;; sorting an arbitrary set of elements while honoring the precedence
;;; constraints given by a set of (X,Y) pairs that indicate that element
;;; X must precede element Y.  The tie-breaker procedure is called when it
;;; is necessary to choose from multiple minimal elements; both a list of
;;; candidates and the ordering so far are provided as arguments.

(defun topological-sort (elements constraints tie-breaker)
  (let ((remaining-constraints constraints)
        (remaining-elements elements)
        (result ()))
    (loop
      (let ((minimal-elements
             (remove-if
              #'(lambda (class)
                 (member class remaining-constraints
                         :key #'cadr))
              remaining-elements)))
        (when (null minimal-elements)
          (if (null remaining-elements)
              (return-from topological-sort result)
              (error "Inconsistent precedence graph.")))
        (let ((choice (if (null (cdr minimal-elements))
                          (car minimal-elements)
                          (funcall tie-breaker
                                   minimal-elements
                                   result))))
          (setq result (append result (list choice)))
          (setq remaining-elements
                (remove choice remaining-elements))
          (setq remaining-constraints
                (remove choice
                        remaining-constraints
                        :test #'member)))))))

;;; In the event of a tie while topologically sorting class precedence lists,
;;; the CLOS Specification says to "select the one that has a direct subclass
;;; rightmost in the class precedence list computed so far."  The same result
;;; is obtained by inspecting the partially constructed class precedence list
;;; from right to left, looking for the first minimal element to show up among
;;; the direct superclasses of the class precedence list constituent.
;;; (There's a lemma that shows that this rule yields a unique result.)

(defun std-tie-breaker-rule (minimal-elements cpl-so-far)
  (dolist (cpl-constituent (reverse cpl-so-far))
    (let* ((supers (class-direct-superclasses cpl-constituent))
           (common (intersection minimal-elements supers)))
      (when (not (null common))
        (return-from std-tie-breaker-rule (car common))))))

;;; This version of collect-superclasses* isn't bothered by cycles in the class
;;; hierarchy, which sometimes happen by accident.

(defun collect-superclasses* (class)
  (labels ((all-superclasses-loop (seen superclasses)
                                  (let ((to-be-processed
                                         (set-difference superclasses seen)))
                                    (if (null to-be-processed)
                                        superclasses
                                        (let ((class-to-process
                                               (car to-be-processed)))
                                          (all-superclasses-loop
                                           (cons class-to-process seen)
                                           (union (class-direct-superclasses
                                                   class-to-process)
                                                  superclasses)))))))
          (all-superclasses-loop () (list class))))

;;; The local precedence ordering of a class C with direct superclasses C_1,
;;; C_2, ..., C_n is the set ((C C_1) (C_1 C_2) ...(C_n-1 C_n)).

(defun local-precedence-ordering (class)
  (mapcar #'list
          (cons class
                (butlast (class-direct-superclasses class)))
          (class-direct-superclasses class)))

;;; Slot inheritance

(defun std-compute-slots (class)
  (let* ((all-slots (mapappend #'(lambda (c) (class-direct-slots c))
                               ;; Slots of base class must come first
                               (reverse (class-precedence-list class))))
         (all-names (delete-duplicates
                     (mapcar 'slot-definition-name all-slots)
                     :from-end t)))
    (mapcar #'(lambda (name)
               (funcall
                (if (std-class-p class)
                    #'std-compute-effective-slot-definition
                    #'compute-effective-slot-definition)
                class
                name
                ;; Slot of inherited class must override initfunction,
                ;; documentation of base class
                (nreverse
                 (remove name all-slots
                         :key 'slot-definition-name
                         :test-not #'eq))))
            all-names)))

(defun std-compute-effective-slot-definition (class name direct-slots)
  (let ((initer (find-if-not #'null direct-slots
                             :key 'slot-definition-initfunction))
        (documentation-slot (find-if-not #'null direct-slots
                                         :key 'slot-definition-documentation))
        (types (delete-duplicates
                (delete t (mapcar #'slot-definition-type direct-slots))
                :test #'equal)))
    (make-effective-slot-definition
     class
     :name name
     :initform (if initer
                   (slot-definition-initform initer)
                   nil)
     :initfunction (if initer
                       (slot-definition-initfunction initer)
                       nil)
     :initargs (remove-duplicates
                (mapappend 'slot-definition-initargs
                           direct-slots))
     :allocation (slot-definition-allocation (car direct-slots))
     :allocation-class (when (slot-boundp (car direct-slots)
                                          'sys::allocation-class)
                         ;;for some classes created in Java
                         ;;(e.g. SimpleCondition) this slot is unbound
                         (slot-definition-allocation-class (car direct-slots)))
     :type (cond ((null types) t)
                 ((= 1 (length types)) types)
                 (t (list* 'and types)))
     :documentation (if documentation-slot
                        (documentation documentation-slot t)
                        nil))))

;;; Standard instance slot access

;;; N.B. The location of the effective-slots slots in the class metaobject for
;;; standard-class must be determined without making any further slot
;;; references.

(defun find-slot-definition (class slot-name)
  (dolist (slot (class-slots class) nil)
    (when (eq slot-name (slot-definition-name slot))
      (return slot))))

(defun slot-location (class slot-name)
  (let ((slot (find-slot-definition class slot-name)))
    (if slot
        (slot-definition-location slot)
        nil)))

(defun instance-slot-location (instance slot-name)
  (let ((layout (std-instance-layout instance)))
    (and layout (layout-slot-location layout slot-name))))

(defun slot-value (object slot-name)
  (let* ((class (class-of object))
         (metaclass (class-of class)))
    (if (or (eq metaclass +the-standard-class+)
            (eq metaclass +the-structure-class+)
            (eq metaclass +the-funcallable-standard-class+))
        (std-slot-value object slot-name)
        (slot-value-using-class class object
                                (find-slot-definition class slot-name)))))

(defun %set-slot-value (object slot-name new-value)
  (let* ((class (class-of object))
         (metaclass (class-of class)))
    (if (or (eq metaclass +the-standard-class+)
            (eq metaclass +the-structure-class+)
            (eq metaclass +the-funcallable-standard-class+))
        (setf (std-slot-value object slot-name) new-value)
        (setf (slot-value-using-class class object
                                      (find-slot-definition class slot-name))
              new-value))))

(defsetf slot-value %set-slot-value)

(defun slot-boundp (object slot-name)
  (let ((class (class-of object)))
    (if (std-class-p class)
        (std-slot-boundp object slot-name)
        (slot-boundp-using-class class object
                                 (find-slot-definition class slot-name)))))

(defun std-slot-makunbound (instance slot-name)
  (let ((location (instance-slot-location instance slot-name)))
    (cond ((fixnump location)
           (setf (standard-instance-access instance location) +slot-unbound+))
          ((consp location)
           (setf (cdr location) +slot-unbound+))
          (t
           (slot-missing (class-of instance) instance slot-name 'slot-makunbound))))
  instance)

(defun slot-makunbound (object slot-name)
  (let ((class (class-of object)))
    (if (std-class-p class)
        (std-slot-makunbound object slot-name)
        (slot-makunbound-using-class class object
                                     (find-slot-definition class slot-name)))))

(defun std-slot-exists-p (instance slot-name)
  (not (null (find slot-name (class-slots (class-of instance))
                   :key 'slot-definition-name))))

(defun slot-exists-p (object slot-name)
  (let ((class (class-of object)))
    (if (std-class-p class)
        (std-slot-exists-p object slot-name)
        (slot-exists-p-using-class class object slot-name))))

(defun instance-slot-p (slot)
  (eq (slot-definition-allocation slot) :instance))

(defun std-allocate-instance (class)
  (sys::%std-allocate-instance class))

(defun allocate-funcallable-instance (class)
  (let ((instance (sys::%allocate-funcallable-instance class)))
    ;; KLUDGE: without this, the build fails with unbound-slot
    (when (or (eq class +the-standard-generic-function-class+)
              (subtypep class +the-standard-generic-function-class+))
      (setf (std-slot-value instance 'sys::method-class)
            +the-standard-method-class+))
    (set-funcallable-instance-function
     instance
     #'(lambda (&rest args)
         (declare (ignore args))
         (error 'program-error "Called a funcallable-instance with unset function.")))
    instance))

(declaim (notinline class-prototype))
(defun class-prototype (class)
  (unless (class-finalized-p class) (error "Class ~A not finalized" (class-name class)))
  (std-allocate-instance class))

(defun maybe-finalize-class-subtree (class)
  (when (every #'class-finalized-p (class-direct-superclasses class))
    (finalize-inheritance class)
    (dolist (subclass (class-direct-subclasses class))
      (maybe-finalize-class-subtree subclass))))

(defun make-instance-standard-class (metaclass
                                     &rest initargs
                                     &key name direct-superclasses direct-slots
                                       direct-default-initargs
                                       documentation)
  (declare (ignore metaclass))
  (let ((class (std-allocate-instance +the-standard-class+)))
    (unless *clos-booting*
      (check-initargs (list #'allocate-instance #'initialize-instance)
                      (list* class initargs)
                      class t initargs
                      *make-instance-initargs-cache* 'make-instance))
    (%set-class-name name class)
    ;; KLUDGE: necessary in define-primordial-class, otherwise
    ;; StandardClass.getClassLayout() throws an error
    (unless *clos-booting* (%set-class-layout nil class))
    (%set-class-direct-subclasses ()  class)
    (%set-class-direct-methods ()  class)
    (%set-class-documentation class documentation)
    (std-after-initialization-for-classes class
                                          :direct-superclasses direct-superclasses
                                          :direct-slots direct-slots
                                          :direct-default-initargs direct-default-initargs)
    class))

(defun make-or-find-instance-funcallable-standard-class
    (metaclass &rest initargs &key name direct-superclasses direct-slots
                                direct-default-initargs documentation)
  (declare (ignore metaclass initargs))
  (or (find-class name nil)
      (let ((class (std-allocate-instance +the-funcallable-standard-class+)))
        (%set-class-name name class)
        (unless *clos-booting* (%set-class-layout nil class))
        (%set-class-direct-subclasses ()  class)
        (%set-class-direct-methods ()  class)
        (%set-class-documentation class documentation)
        (std-after-initialization-for-classes class
                                              :direct-superclasses direct-superclasses
                                              :direct-slots direct-slots
                                              :direct-default-initargs direct-default-initargs)
        class)))

;(defun convert-to-direct-slot-definition (class canonicalized-slot)
;  (apply #'make-instance
;         (apply #'direct-slot-definition-class
;                class canonicalized-slot)
;         canonicalized-slot))

(defun canonicalize-direct-superclass-list (class direct-superclasses)
  (cond (direct-superclasses)
        ((subtypep (class-of class) +the-funcallable-standard-class+)
         (list +the-funcallable-standard-object-class+))
        ((subtypep (class-of class) +the-standard-class+)
         (list +the-standard-object-class+))))

(defun std-after-initialization-for-classes (class
                                             &key direct-superclasses direct-slots
                                             direct-default-initargs
                                             &allow-other-keys)
  (let ((supers (canonicalize-direct-superclass-list class direct-superclasses)))
    (setf (class-direct-superclasses class) supers)
    (dolist (superclass supers)
      (add-direct-subclass superclass class)))
  (let ((slots (mapcar #'(lambda (slot-properties)
                          (apply #'make-direct-slot-definition class slot-properties))
                       direct-slots)))
    (setf (class-direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (slot-definition-readers direct-slot))
        (add-reader-method class reader direct-slot))
      (dolist (writer (slot-definition-writers direct-slot))
        (add-writer-method class writer direct-slot))))
  (setf (class-direct-default-initargs class) direct-default-initargs)
  (maybe-finalize-class-subtree class)
  (values))

(defmacro define-primordial-class (name superclasses direct-slots)
  "Primitive class definition tool.
No non-standard metaclasses, accessor methods, duplicate slots,
non-existent superclasses, default initargs, or other complicated stuff.
Handle with care."
  (let ((class (gensym)))
    `(let ((,class (make-instance-standard-class
                    nil
                    :name ',name
                    :direct-superclasses ',(mapcar #'find-class superclasses)
                    :direct-slots ,(canonicalize-direct-slots direct-slots))))
       (%set-find-class ',name ,class)
       ,class)))

(defmacro define-funcallable-primordial-class (name superclasses direct-slots)
  "Primitive funcallable class definition tool.
No non-standard metaclasses, accessor methods, duplicate slots,
non-existent superclasses, default initargs, or other complicated stuff.
Handle with care.
Will not modify existing classes to avoid breaking std-generic-function-p."
  (let ((class (gensym)))
    `(let ((,class (make-or-find-instance-funcallable-standard-class
                    nil
                    :name ',name
                    :direct-superclasses ',(mapcar #'find-class superclasses)
                    :direct-slots ,(canonicalize-direct-slots direct-slots))))
       (%set-find-class ',name ,class)
       ,class)))

(define-primordial-class eql-specializer (specializer)
  ((object :initform nil)
   (direct-methods :initform nil)))

(define-primordial-class method-combination (metaobject)
  ((sys::name :initarg :name :initform nil)
   (sys::%documentation :initarg :documentation :initform nil)
   (options :initarg :options :initform nil)))

(define-primordial-class short-method-combination (method-combination)
  ((operator :initarg :operator)
   (identity-with-one-argument :initarg :identity-with-one-argument)))

(define-primordial-class long-method-combination (method-combination)
  ((sys::lambda-list :initarg :lambda-list)
   (method-group-specs :initarg :method-group-specs)
   (args-lambda-list :initarg :args-lambda-list)
   (generic-function-symbol :initarg :generic-function-symbol)
   (function :initarg :function)
   (arguments :initarg :arguments)
   (declarations :initarg :declarations)
   (forms :initarg :forms)))

(define-primordial-class standard-accessor-method (standard-method)
  ((sys::%slot-definition :initarg :slot-definition :initform nil)))

(define-primordial-class standard-reader-method (standard-accessor-method)
  ())
(defconstant +the-standard-reader-method-class+
  (find-class 'standard-reader-method))

(define-primordial-class standard-writer-method (standard-accessor-method)
  ())
(defconstant +the-standard-writer-method-class+
  (find-class 'standard-writer-method))

(define-primordial-class structure-class (class)
  ())
(defconstant +the-structure-class+ (find-class 'structure-class))

(define-primordial-class forward-referenced-class (class)
  ;; The standard-class layout.  Not all of these slots are necessary,
  ;; but at least NAME and DIRECT-SUBCLASSES are.
  ((sys::name :initarg :name :initform nil)
   (sys::layout :initform nil)
   (sys::direct-superclasses :initform nil)
   (sys::direct-subclasses :initform nil)
   (sys::precedence-list :initform nil)
   (sys::direct-methods :initform nil)
   (sys::direct-slots :initform nil)
   (sys::slots :initform nil)
   (sys::direct-default-initargs :initform nil)
   (sys::default-initargs :initform nil)
   (sys::finalized-p :initform nil)
   (sys::%documentation :initform nil)))
(defconstant +the-forward-referenced-class+
  (find-class 'forward-referenced-class))

(define-funcallable-primordial-class generic-function
    (metaobject funcallable-standard-object)
  ())

(defvar *extensible-built-in-classes*
  (list (find-class 'sequence)
        (find-class 'java:java-object)))

(defvar *make-instance-initargs-cache*
  (make-hash-table :test #'eq)
  "Cached sets of allowable initargs, keyed on the class they belong to.")
(defvar *reinitialize-instance-initargs-cache*
  (make-hash-table :test #'eq)
  "Cached sets of allowable initargs, keyed on the class they belong to.")

(defun expand-long-defcombin (name args)
  (destructuring-bind (lambda-list method-groups &rest body) args
    `(apply #'define-long-form-method-combination
            ',name
            ',lambda-list
            (list ,@(mapcar #'canonicalize-method-group-spec method-groups))
            ',body)))

;;; The class method-combination and its subclasses are defined in
;;; StandardClass.java, but we cannot use make-instance and slot-value
;;; yet.

(defun %make-long-method-combination (&key name documentation lambda-list
                                       method-group-specs args-lambda-list
                                       generic-function-symbol function
                                       arguments declarations forms)
  (let ((instance (std-allocate-instance (find-class 'long-method-combination))))
    (setf (std-slot-value instance 'sys::name) name)
    (setf (std-slot-value instance 'sys:%documentation) documentation)
    (setf (std-slot-value instance 'sys::lambda-list) lambda-list)
    (setf (std-slot-value instance 'method-group-specs) method-group-specs)
    (setf (std-slot-value instance 'args-lambda-list) args-lambda-list)
    (setf (std-slot-value instance 'generic-function-symbol)
          generic-function-symbol)
    (setf (std-slot-value instance 'function) function)
    (setf (std-slot-value instance 'arguments) arguments)
    (setf (std-slot-value instance 'declarations) declarations)
    (setf (std-slot-value instance 'forms) forms)
    (setf (std-slot-value instance 'options) nil)
    instance))

(defun method-combination-name (method-combination)
  (check-type method-combination method-combination)
  (std-slot-value method-combination 'sys::name))

(defun method-combination-documentation (method-combination)
  (check-type method-combination method-combination)
  (std-slot-value method-combination 'sys:%documentation))

(defun short-method-combination-operator (method-combination)
  (check-type method-combination short-method-combination)
  (std-slot-value method-combination 'operator))

(defun short-method-combination-identity-with-one-argument (method-combination)
  (check-type method-combination short-method-combination)
  (std-slot-value method-combination 'identity-with-one-argument))

(defun long-method-combination-lambda-list (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'sys::lambda-list))

(defun long-method-combination-method-group-specs (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'method-group-specs))

(defun long-method-combination-args-lambda-list (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'args-lambda-list))

(defun long-method-combination-generic-function-symbol (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'generic-function-symbol))

(defun long-method-combination-function (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'function))

(defun long-method-combination-arguments (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'arguments))

(defun long-method-combination-declarations (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'declarations))

(defun long-method-combination-forms (method-combination)
  (check-type method-combination long-method-combination)
  (std-slot-value method-combination 'forms))


(defun expand-short-defcombin (whole)
  (let* ((name (cadr whole))
         (documentation
          (getf (cddr whole) :documentation ""))
         (identity-with-one-arg
          (getf (cddr whole) :identity-with-one-argument nil))
         (operator
          (getf (cddr whole) :operator name)))
    `(progn
       (let ((instance (std-allocate-instance
                        (find-class 'short-method-combination))))
         (setf (std-slot-value instance 'sys::name) ',name)
         (setf (std-slot-value instance 'sys:%documentation) ',documentation)
         (setf (std-slot-value instance 'operator) ',operator)
         (setf (std-slot-value instance 'identity-with-one-argument)
               ',identity-with-one-arg)
         (setf (std-slot-value instance 'options) nil)
         (setf (get ',name 'method-combination-object) instance)
         ',name))))

(defmacro define-method-combination (&whole form name &rest args)
  (if (and (cddr form)
           (listp (caddr form)))
      (expand-long-defcombin name args)
      (expand-short-defcombin form)))

(define-method-combination +      :identity-with-one-argument t)
(define-method-combination and    :identity-with-one-argument t)
(define-method-combination append :identity-with-one-argument nil)
(define-method-combination list   :identity-with-one-argument nil)
(define-method-combination max    :identity-with-one-argument t)
(define-method-combination min    :identity-with-one-argument t)
(define-method-combination nconc  :identity-with-one-argument t)
(define-method-combination or     :identity-with-one-argument t)
(define-method-combination progn  :identity-with-one-argument t)

;;;
;;; long form of define-method-combination (from Sacla and XCL)
;;;
(defun method-group-p (selecter qualifiers)
  ;; selecter::= qualifier-pattern | predicate
  (etypecase selecter
    (list (or (equal selecter qualifiers)
              (let ((last (last selecter)))
                (when (eq '* (cdr last))
                  (let* ((prefix `(,@(butlast selecter) ,(car last)))
                         (pos (mismatch prefix qualifiers)))
                    (or (null pos) (= pos (length prefix))))))))
    ((eql *) t)
    (symbol (funcall (symbol-function selecter) qualifiers))))

(defun check-variable-name (name)
  (flet ((valid-variable-name-p (name)
                                (and (symbolp name) (not (constantp name)))))
    (assert (valid-variable-name-p name))))

(defun canonicalize-method-group-spec (spec)
  ;; spec ::= (name {qualifier-pattern+ | predicate} [[long-form-option]])
  ;; long-form-option::= :description description | :order order |
  ;;                     :required required-p
  ;; a canonicalized-spec is a simple plist.
  (let* ((rest spec)
         (name (prog2 (check-variable-name (car rest))
                 (car rest)
                 (setq rest (cdr rest))))
         (option-names '(:description :order :required))
         (selecters (let ((end (or (position-if #'(lambda (it)
                                                   (member it option-names))
                                                rest)
                                   (length rest))))
                      (prog1 (subseq rest 0 end)
                        (setq rest (subseq rest end)))))
         (description (getf rest :description ""))
         (order (getf rest :order :most-specific-first))
         (required-p (getf rest :required)))
    `(list :name ',name
           :predicate (lambda (qualifiers)
                        (loop for item in ',selecters
                          thereis (method-group-p item qualifiers)))
           :description ',description
           :order ',order
           :required ',required-p
           :*-selecter ,(equal selecters '(*)))))

(defun extract-required-part (lambda-list)
  (flet ((skip (key lambda-list)
               (if (eq (first lambda-list) key)
                   (cddr lambda-list)
                   lambda-list)))
    (let* ((trimmed-lambda-list
            (skip '&environment (skip '&whole lambda-list)))
           (after-required-lambda-list
            (member-if #'(lambda (it) (member it lambda-list-keywords))
                       trimmed-lambda-list)))
      (if after-required-lambda-list
        (ldiff trimmed-lambda-list after-required-lambda-list)
        trimmed-lambda-list))))

(defun extract-specified-part (key lambda-list)
  (case key
    ((&eval &whole)
     (list (second (member key lambda-list))))
    (t
     (let ((here (cdr (member key lambda-list))))
       (ldiff here
              (member-if #'(lambda (it) (member it lambda-list-keywords))
                         here))))))

(defun extract-optional-part (lambda-list)
  (extract-specified-part '&optional lambda-list))

(defun parse-define-method-combination-args-lambda-list (lambda-list)
  ;; Define-method-combination Arguments Lambda Lists
  ;; http://www.lispworks.com/reference/HyperSpec/Body/03_dj.htm
  (let ((required (extract-required-part lambda-list))
        (whole    (extract-specified-part '&whole    lambda-list))
        (optional (extract-specified-part '&optional lambda-list))
        (rest     (extract-specified-part '&rest     lambda-list))
        (keys     (extract-specified-part '&key      lambda-list))
        (aux      (extract-specified-part '&aux      lambda-list)))
    (values (first whole)
            required
            (mapcar #'(lambda (spec)
                       (if (consp spec)
                           `(,(first spec) ,(second spec) ,@(cddr spec))
                           `(,spec nil)))
                    optional)
            (first rest)
            (mapcar #'(lambda (spec)
                       (let ((key (if (consp spec) (car spec) spec))
                             (rest (when (consp spec) (rest spec))))
                         `(,(if (consp key) key `(,(make-keyword key) ,key))
                           ,(car rest)
                           ,@(cdr rest))))
                    keys)
            (mapcar #'(lambda (spec)
                       (if (consp spec)
                           `(,(first spec) ,(second spec))
                           `(,spec nil)))
                    aux))))

(defun wrap-with-call-method-macro (gf args-var emf-form)
  `(macrolet
       ((call-method (method &optional next-method-list)
          `(funcall
            ,(cond
              ((listp method)
               (assert (eq (first method) 'make-method))
               ;; by generating an inline expansion we prevent allocation
               ;; of a method instance which will be discarded immediately
               ;; after reading the METHOD-FUNCTION slot
               (compute-method-function
                    `(lambda (&rest ,(gensym))
                       ;; the MAKE-METHOD body form gets evaluated in
                       ;; the null lexical environment augmented
                       ;; with a binding for CALL-METHOD
                       ,(wrap-with-call-method-macro ,gf
                                                     ',args-var
                                                     (second method)))))
              (t (method-function method)))
            ,',args-var
            ,(unless (null next-method-list)
                     ;; by not generating an emf when there are no next methods,
                     ;; we ensure next-method-p returns NIL
                     (compute-effective-method
                      ,gf (generic-function-method-combination ,gf)
                      (process-next-method-list next-method-list))))))
     ,emf-form))

(defun assert-unambiguous-method-sorting (group-name methods)
  (let ((specializers (make-hash-table :test 'equal)))
    (dolist (method methods)
      (push method (gethash (method-specializers method) specializers)))
    (loop for specializer-methods being each hash-value of specializers
       using (hash-key method-specializers)
       unless (= 1 (length specializer-methods))
       do (error "Ambiguous method sorting in method group ~A due to multiple ~
                  methods with specializers ~S: ~S"
                 group-name method-specializers specializer-methods))))

(defmacro with-method-groups (method-group-specs methods-form &body forms)
  (flet ((grouping-form (spec methods-var)
           (let ((predicate (coerce-to-function (getf spec :predicate)))
                 (group (gensym))
                 (leftovers (gensym))
                 (method (gensym)))
             `(let ((,group '())
                    (,leftovers '()))
                (dolist (,method ,methods-var)
                  (if (funcall ,predicate (method-qualifiers ,method))
                      (push ,method ,group)
                      (push ,method ,leftovers)))
                (ecase ,(getf spec :order)
                  (:most-specific-last )
                  (:most-specific-first (setq ,group (nreverse ,group))))
                ,@(when (getf spec :required)
                        `((when (null ,group)
                            (error "Method group ~S must not be empty."
                                   ',(getf spec :name)))))
                (setq ,methods-var (nreverse ,leftovers))
                ,group))))
    (let ((rest (gensym))
          (method (gensym)))
      `(let* ((,rest ,methods-form)
              ,@(mapcar #'(lambda (spec)
                           `(,(getf spec :name) ,(grouping-form spec rest)))
                        method-group-specs))
         (dolist (,method ,rest)
           (invalid-method-error ,method
                                 "Method ~S with qualifiers ~S does not belong to any method group."
                                 ,method (method-qualifiers ,method)))
         ,@(unless (and (= 1 (length method-group-specs))
                        (getf (car method-group-specs) :*-selecter))
             (mapcar #'(lambda (spec)
                         `(assert-unambiguous-method-sorting ',(getf spec :name) ,(getf spec :name)))
                     method-group-specs))
         ,@forms))))

(defun method-combination-type-lambda-with-args-emf
    (&key args-lambda-list generic-function-symbol forms &allow-other-keys)
  (multiple-value-bind
        (whole required optional rest keys aux)
      (parse-define-method-combination-args-lambda-list args-lambda-list)
    (unless rest
      (when keys
        (setf rest (gensym))))
    (let* ((gf-lambda-list (gensym))
           (args-var (gensym))
           (args-len-var (gensym))
           (binding-forms (gensym))
           (needs-args-len-var (gensym))
           (emf-form (gensym)))
      `(let* ((,gf-lambda-list (slot-value ,generic-function-symbol
                                           'sys::lambda-list))
              (nreq (length (extract-required-part ,gf-lambda-list)))
              (nopt (length (extract-optional-part ,gf-lambda-list)))
              (,binding-forms)
              (,needs-args-len-var)
              (,emf-form
               (let* (,@(when whole
                              `((,whole (progn
                                          (push `(,',whole ,',args-var)
                                                ,binding-forms)
                                          ',args-var))))
                      ,@(when rest
                              ;; ### TODO: use a fresh symbol for the rest
                              ;;   binding being generated and pushed into binding-forms
                              `((,rest (progn
                                         (push `(,',rest
                                                 (subseq ,',args-var
                                                          ,(+ nreq nopt)))
                                               ,binding-forms)
                                         ',rest))))
                        ,@(loop for var in required and i upfrom 0
                               for var-binding = (gensym)
                             collect `(,var (when (< ,i nreq)
                                              (push `(,',var-binding
                                                      (nth ,,i ,',args-var))
                                                    ,binding-forms)
                                              ',var-binding)))
                        ,@(loop for (var initform supplied-var) in optional
                             and i upfrom 0
                             for supplied-binding = (or supplied-var (gensym))
                             for var-binding = (gensym)
                             ;; check for excess parameters
                             ;; only assign initform if the parameter
                             ;; isn't in excess: the spec says explicitly
                             ;; to bind parameters in excess to forms evaluating
                             ;; to nil.
                             ;; This leaves initforms to be used with
                             ;; parameters not supplied in excess, but
                             ;; not available in the arguments list
                             ;;
                             ;; Also, if specified, bind "supplied-p"
                             collect `(,supplied-binding
                                       (when (< ,i nopt)
                                         (setq ,needs-args-len-var t)
                              ;; ### TODO: use a fresh symbol for the supplied binding
                              ;;   binding being generated and pushed into binding-forms
                                         (push `(,',supplied-binding
                                                 (< ,(+ ,i nreq) ,',args-len-var))
                                               ,binding-forms)
                                         ',supplied-binding))
                             collect `(,var (when (< ,i nopt)
                                              (push `(,',var-binding
                                                      (if ,',supplied-binding
                                                          (nth ,(+ ,i nreq)
                                                               ,',args-var)
                                                          ,',initform))
                                                    ,binding-forms)
                                              ',var-binding)))
                        ,@(loop for ((key var) initform supplied-var) in keys
                             for supplied-binding = (or supplied-var (gensym))
                             for var-binding = (gensym)
                             ;; Same as optional parameters:
                             ;; even though keywords can't be supplied in
                             ;; excess, we should bind "supplied-p" in case
                             ;; the key isn't supplied in the arguments list
                             collect `(,supplied-binding
                                       (progn
                              ;; ### TODO: use a fresh symbol for the rest
                              ;;   binding being generated and pushed into binding-forms
                                         (push `(,',supplied-binding
                                                 (member ,',key ,',rest))
                                               ,binding-forms)
                                         ',supplied-binding))
                             collect `(,var (progn
                                              (push `(,',var-binding
                                                      (if ,',supplied-binding
                                                          (cadr ,',supplied-binding)
                                                          ,',initform))
                                                    ,binding-forms)
                                              ',var-binding)))
                        ,@(loop for (var initform) in aux
                             for var-binding = (gensym)
                             collect `(,var (progn
                                              (push '(,var-binding ,initform)
                                                    ,binding-forms)
                                              ',var-binding))))
                 ,@forms)))
         `(lambda (,',args-var)
            ;; set up bindings to ensure the expressions to which the
            ;; variables of the arguments option have been bound are
            ;; evaluated exactly once.
            (let* (,@(when ,needs-args-len-var
                           `((,',args-len-var (length ,',args-var))))
                   ,@(reverse ,binding-forms))
              ;; This is the lambda which *is* the effective method
              ;; hence gets called on every method invocation
              ;; be as efficient in this method as we can be
              ,(wrap-with-call-method-macro ,generic-function-symbol
                                            ',args-var ,emf-form)))))))

(defun method-combination-type-lambda
  (&rest all-args
   &key name lambda-list args-lambda-list generic-function-symbol
        method-group-specs declarations forms &allow-other-keys)
  (declare (ignore name))
  (let ((methods (gensym))
        (args-var (gensym))
        (emf-form (gensym)))
    `(lambda (,generic-function-symbol ,methods ,@lambda-list)
       ;; This is the lambda which computes the effective method
       ,@declarations
       (with-method-groups ,method-group-specs
           ,methods
         ,(if (null args-lambda-list)
              `(let ((,emf-form (progn ,@forms)))
                 `(lambda (,',args-var)
                    ;; This is the lambda which *is* the effective method
                    ;; hence gets called on every method invocation
                    ;; be as efficient in this method as we can be
                    ,(wrap-with-call-method-macro ,generic-function-symbol
                                                  ',args-var ,emf-form)))
              (apply #'method-combination-type-lambda-with-args-emf all-args))))))

(defun declarationp (expr)
  (and (consp expr) (eq (car expr) 'DECLARE)))

(defun long-form-method-combination-args (args)
  ;; define-method-combination name lambda-list (method-group-specifier*) args
  ;; args ::= [(:arguments . args-lambda-list)]
  ;;          [(:generic-function generic-function-symbol)]
  ;;          [[declaration* | documentation]] form*
  (let ((rest args))
    (labels ((nextp (key) (and (consp (car rest)) (eq key (caar rest))))
             (args-lambda-list ()
               (when (nextp :arguments)
                 (prog1 (cdr (car rest)) (setq rest (cdr rest)))))
             (generic-function-symbol ()
                (if (nextp :generic-function)
                    (prog1 (second (car rest)) (setq rest (cdr rest)))
                    (gensym)))
             (declaration* ()
               (let ((end (position-if-not #'declarationp rest)))
                 (when end
                   (prog1 (subseq rest 0 end) (setq rest (nthcdr end rest))))))
             (documentation? ()
               (when (stringp (car rest))
                 (prog1 (car rest) (setq rest (cdr rest)))))
             (form* () rest))
      (let ((declarations '()))
        `(:args-lambda-list ,(args-lambda-list)
                            :generic-function-symbol ,(generic-function-symbol)
                            :documentation ,(prog2 (setq declarations (declaration*))
                                              (documentation?))
                            :declarations (,@declarations ,@(declaration*))
                            :forms ,(form*))))))

(defun define-long-form-method-combination (name lambda-list method-group-specs
                                                 &rest args)
  (let* ((initargs `(:name ,name
                     :lambda-list ,lambda-list
                     :method-group-specs ,method-group-specs
                     ,@(long-form-method-combination-args args)))
         (lambda-expression (apply #'method-combination-type-lambda initargs)))
    (setf (get name 'method-combination-object)
          (apply '%make-long-method-combination
                 :function (coerce-to-function lambda-expression) initargs))
    name))

(defun std-find-method-combination (gf name options)
  (declare (ignore gf))
  (when (and (eql name 'standard) options)
    ;; CLHS DEFGENERIC
    (error "The standard method combination does not accept any arguments."))
  (let ((mc (get name 'method-combination-object)))
    (cond
      ((null mc) (error "Method combination ~S not found" name))
      ((null options) mc)
      ((typep mc 'short-method-combination)
       (make-instance
        'short-method-combination
        :name name
        :documentation (method-combination-documentation mc)
        :operator (short-method-combination-operator mc)
        :identity-with-one-argument
        (short-method-combination-identity-with-one-argument mc)
        :options options))
      ((typep mc 'long-method-combination)
       (make-instance
        'long-method-combination
        :name name
        :documentation (method-combination-documentation mc)
        :lambda-list (long-method-combination-lambda-list mc)
        :method-group-specs (long-method-combination-method-group-specs mc)
        :args-lambda-list (long-method-combination-args-lambda-list mc)
        :generic-function-symbol (long-method-combination-generic-function-symbol mc)
        :function (long-method-combination-function mc)
        :arguments (long-method-combination-arguments mc)
        :declarations (long-method-combination-declarations mc)
        :forms (long-method-combination-forms mc)
        :options options)))))

(declaim (notinline find-method-combination))
(defun find-method-combination (gf name options)
  (std-find-method-combination gf name options))

(defconstant +the-standard-method-combination+
  (let ((instance (std-allocate-instance (find-class 'method-combination))))
    (setf (std-slot-value instance 'sys::name) 'standard)
    (setf (std-slot-value instance 'sys:%documentation)
          "The standard method combination.")
    (setf (std-slot-value instance 'options) nil)
    instance)
  "The standard method combination.
Do not use this object for identity since it changes between
compile-time and run-time.  To detect the standard method combination,
compare the method combination name to the symbol 'standard.")
(setf (get 'standard 'method-combination-object) +the-standard-method-combination+)

(define-funcallable-primordial-class standard-generic-function (generic-function)
  ((sys::name :initarg :name :initform nil)
   (sys::lambda-list :initarg :lambda-list :initform nil)
   (sys::required-args :initarg :required-args :initform nil)
   (sys::optional-args :initarg :optional-args :initform nil)
   (sys::initial-methods :initarg :initial-methods :initform nil)
   (sys::methods :initarg :methods :initform nil)
   (sys::method-class :initarg :method-class
                      :initform +the-standard-method-class+)
   (sys::%method-combination :initarg :method-combination
                             :initform +the-standard-method-combination+)
   (sys::argument-precedence-order :initarg :argument-precedence-order
                                   :initform nil)
   (sys::declarations :initarg :declarations :initform nil)
   (sys::%documentation :initarg :documentation :initform nil)))
(defconstant +the-standard-generic-function-class+
  (find-class 'standard-generic-function))

(defun std-generic-function-p (gf)
  (eq (class-of gf) +the-standard-generic-function-class+))

(defparameter *eql-specializer-table* (make-hash-table :test 'eql))

(defun intern-eql-specializer (object)
  (or (gethash object *eql-specializer-table*)
      (setf (gethash object *eql-specializer-table*)
            ;; we will be called during generic function invocation
            ;; setup, so have to rely on plain functions here.
            (let ((instance (std-allocate-instance (find-class 'eql-specializer))))
              (setf (std-slot-value instance 'object) object)
              (setf (std-slot-value instance 'direct-methods) nil)
              instance))))

(defun eql-specializer-object (eql-specializer)
  (check-type eql-specializer eql-specializer)
  (std-slot-value eql-specializer 'object))

;;; Initial versions of some method metaobject readers.  Defined on
;;; AMOP pg. 218ff, will be redefined when generic functions are set up.

(defun std-method-function (method)
  (std-slot-value method 'sys::%function))

(defun std-method-generic-function (method)
  (std-slot-value method 'sys::%generic-function))

(defun std-method-specializers (method)
  (std-slot-value method 'sys::specializers))

(defun std-method-qualifiers (method)
  (std-slot-value method 'sys::qualifiers))

(defun std-accessor-method-slot-definition (accessor-method)
  (std-slot-value accessor-method 'sys::%slot-definition))

;;; Additional method readers
(defun std-method-fast-function (method)
  (std-slot-value method 'sys::fast-function))

(defun std-function-keywords (method)
  (values (std-slot-value method 'sys::keywords)
          (std-slot-value method 'sys::other-keywords-p)))

;;; Preliminary accessor definitions, will be redefined as generic
;;; functions later in this file

(declaim (notinline method-generic-function))
(defun method-generic-function (method)
  (std-method-generic-function method))

(declaim (notinline method-function))
(defun method-function (method)
  (std-method-function method))

(declaim (notinline method-specializers))
(defun method-specializers (method)
  (std-method-specializers method))

(declaim (notinline method-qualifiers))
(defun method-qualifiers (method)
  (std-method-qualifiers method))



;;; MOP (p. 216) specifies the following reader generic functions:
;;;   generic-function-argument-precedence-order
;;;   generic-function-declarations
;;;   generic-function-lambda-list
;;;   generic-function-method-class
;;;   generic-function-method-combination
;;;   generic-function-methods
;;;   generic-function-name

;;; Additionally, we define the following reader functions:
;;;   generic-function-required-arguments
;;;   generic-function-optional-arguments

;;; These are defined as functions here and redefined as generic
;;; functions via atomic-defgeneric once we're all set up.

(defun generic-function-name (gf)
  (std-slot-value gf 'sys::name))

(defun generic-function-lambda-list (gf)
  (std-slot-value gf 'sys::lambda-list))

(defun generic-function-methods (gf)
  (std-slot-value gf 'sys::methods))

(defun generic-function-method-class (gf)
  (std-slot-value gf 'sys::method-class))

(defun generic-function-method-combination (gf)
  (std-slot-value gf 'sys::%method-combination))

(defun generic-function-argument-precedence-order (gf)
  (std-slot-value gf 'sys::argument-precedence-order))

(defun generic-function-required-arguments (gf)
  (std-slot-value gf 'sys::required-args))

(defun generic-function-optional-arguments (gf)
  (std-slot-value gf 'sys::optional-args))

(defun (setf method-lambda-list) (new-value method)
  (setf (std-slot-value method 'sys::lambda-list) new-value))

(defun (setf method-qualifiers) (new-value method)
  (setf (std-slot-value method 'sys::qualifiers) new-value))

(defun method-documentation (method)
  (std-slot-value method 'sys:%documentation))

(defun (setf method-documentation) (new-value method)
  (setf (std-slot-value method 'sys:%documentation) new-value))

;;; defgeneric

(defmacro defgeneric (function-name lambda-list
                                    &rest options-and-method-descriptions)
  (let ((options ())
        (methods ())
        (declarations ())
        (documentation nil))
    (dolist (item options-and-method-descriptions)
      (case (car item)
        (declare
         (setf declarations (append declarations (cdr item))))
        (:documentation
         (when documentation
           (error 'program-error
                  :format-control "Documentation option was specified twice for generic function ~S."
                  :format-arguments (list function-name)))
         (setf documentation t)
         (push item options))
        (:method
            ;; KLUDGE (rudi 2013-04-02): this only works with subclasses
            ;; of standard-generic-function, since the initial-methods
            ;; slot is not mandated by AMOP
            (push
             `(push (defmethod ,function-name ,@(cdr item))
               (std-slot-value (fdefinition ',function-name) 'sys::initial-methods))
             methods))
        (t
         (push item options))))
    (when declarations (push (list :declarations declarations) options))
    (setf options (nreverse options)
          methods (nreverse methods))
    ;; Since DEFGENERIC currently shares its argument parsing with
    ;; DEFMETHOD, we perform this check here.
    (when (find '&aux lambda-list)
      (error 'program-error
             :format-control "&AUX is not allowed in a generic function lambda list: ~S"
             :format-arguments (list lambda-list)))
    `(prog1
       (%defgeneric
        ',function-name
        :lambda-list ',lambda-list
        ,@(canonicalize-defgeneric-options options))
       (sys::record-source-information-for-type ',function-name '(:generic-function ,function-name))
       ,@methods)))

(defun canonicalize-defgeneric-options (options)
  (mapappend #'canonicalize-defgeneric-option options))

(defun canonicalize-defgeneric-option (option)
  (case (car option)
    (:generic-function-class
     (list :generic-function-class `(find-class ',(cadr option))))
    (:method-class
     (list :method-class `(find-class ',(cadr option))))
    (:method-combination
     (list :method-combination `',(cdr option)))
    (:argument-precedence-order
     (list :argument-precedence-order `',(cdr option)))
    (t
     (list `',(car option) `',(cadr option)))))

;; From OpenMCL (called canonicalize-argument-precedence-order there,
;; but AMOP specifies argument-precedence-order to return a permutation
;; of the required arguments, not a list of indices, so we calculate
;; them on demand).
(defun argument-precedence-order-indices (apo req)
  (cond ((equal apo req) nil)
        ((not (eql (length apo) (length req)))
         (error 'program-error
                :format-control "Specified argument precedence order ~S does not match lambda list."
                :format-arguments (list apo)))
        (t (let ((res nil))
             (dolist (arg apo (nreverse res))
               (let ((index (position arg req)))
                 (if (or (null index) (memq index res))
                     (error 'program-error
                            :format-control "Specified argument precedence order ~S does not match lambda list."
                            :format-arguments (list apo)))
                 (push index res)))))))

(defun find-generic-function (name &optional (errorp t))
  (let ((function (and (fboundp name) (fdefinition name))))
    (when function
      (when (typep function 'generic-function)
        (return-from find-generic-function function))
      (when (and *traced-names* (find name *traced-names* :test #'equal))
        (setf function (untraced-function name))
        (when (typep function 'generic-function)
          (return-from find-generic-function function)))))
  (if errorp
      (error "There is no generic function named ~S." name)
      nil))

(defun lambda-lists-congruent-p (lambda-list1 lambda-list2)
  (let* ((plist1 (analyze-lambda-list lambda-list1))
         (args1 (getf plist1 :required-args))
         (plist2 (analyze-lambda-list lambda-list2))
         (args2 (getf plist2 :required-args)))
    (= (length args1) (length args2))))

(defun %defgeneric (function-name &rest all-keys)
  (when (fboundp function-name)
    (let ((gf (fdefinition function-name)))
      (when (typep gf 'standard-generic-function)
        ;; Remove methods defined by previous DEFGENERIC forms, as
        ;; specified by CLHS, 7.7 (Macro DEFGENERIC).  KLUDGE: only
        ;; works for subclasses of standard-generic-function.  Since
        ;; AMOP doesn't specify a reader for initial methods, we have to
        ;; skip this step otherwise.
        (dolist (method (std-slot-value gf 'sys::initial-methods))
          (std-remove-method gf method)
          (map-dependents gf
                          #'(lambda (dep)
                              (update-dependent gf dep
                                                'remove-method method))))
        (setf (std-slot-value gf 'sys::initial-methods) '()))))
  (apply 'ensure-generic-function function-name all-keys))

;;; Bootstrap version of ensure-generic-function, handling only
;;; standard-generic-function.  This function is replaced later.
(declaim (notinline ensure-generic-function))
(defun ensure-generic-function (function-name
                                &rest all-keys
                                &key
                                (lambda-list nil lambda-list-supplied-p)
                                (generic-function-class +the-standard-generic-function-class+)
                                (method-class +the-standard-method-class+)
                                (method-combination +the-standard-method-combination+ mc-p)
                                argument-precedence-order
                                (documentation nil documentation-supplied-p)
                                &allow-other-keys)
  (setf all-keys (copy-list all-keys))  ; since we modify it
  (remf all-keys :generic-function-class)
  (let ((gf (find-generic-function function-name nil)))
    (if gf
        (progn
          (when lambda-list-supplied-p
            (unless (or (null (generic-function-methods gf))
                        (lambda-lists-congruent-p lambda-list
                                                  (generic-function-lambda-list gf)))
              (error 'simple-error
                     :format-control "The lambda list ~S is incompatible with the existing methods of ~S."
                     :format-arguments (list lambda-list gf)))
            (setf (std-slot-value gf 'sys::lambda-list) lambda-list)
            (let* ((plist (analyze-lambda-list lambda-list))
                   (required-args (getf plist ':required-args)))
              (setf (std-slot-value gf 'sys::required-args) required-args)
              (setf (std-slot-value gf 'sys::optional-args)
                    (getf plist :optional-args))))
          (setf (std-slot-value gf 'sys::argument-precedence-order)
                (or argument-precedence-order (generic-function-required-arguments gf)))
          (when documentation-supplied-p
            (setf (std-slot-value gf 'sys::%documentation) documentation))
          (finalize-standard-generic-function gf)
          gf)
        (progn
          (when (and (null *clos-booting*)
                     (and (fboundp function-name)
                          ;; since we're overwriting an autoloader,
                          ;; we're probably meant to redefine it,
                          ;; so throwing an error here might be a bad idea.
                          ;; also, resolving the symbol isn't
                          ;; a good option either: we've seen that lead to
                          ;; recursive loading of the same file
                          (and (not (autoloadp function-name))
                               (and (consp function-name)
                                    (eq 'setf (first function-name))
                                    (not (autoload-ref-p (second function-name)))))))
            (error 'program-error
                   :format-control "~A already names an ordinary function, macro, or special operator."
                   :format-arguments (list function-name)))
          (when mc-p
            (error "Preliminary ensure-method does not support :method-combination argument."))
          (apply #'make-instance-standard-generic-function
                 generic-function-class
                 :name function-name
                 :method-class method-class
                 :method-combination method-combination
                 all-keys)))))

(defun collect-eql-specializer-objects (generic-function)
  (let ((result nil))
    (dolist (method (generic-function-methods generic-function))
      (dolist (specializer (method-specializers method))
        (when (typep specializer 'eql-specializer)
          (pushnew (eql-specializer-object specializer)
                   result
                   :test 'eql))))
    result))

(defun finalize-standard-generic-function (gf)
  (%reinit-emf-cache gf (collect-eql-specializer-objects gf))
  (set-funcallable-instance-function
   gf
   (if (std-generic-function-p gf)
       (std-compute-discriminating-function gf)
       (compute-discriminating-function gf)))
  ;; FIXME Do we need to warn on redefinition somewhere else?
  (let ((*warn-on-redefinition* nil))
    (setf (fdefinition (generic-function-name gf)) gf))
  (values))

(defun make-instance-standard-generic-function (generic-function-class
                                                &key name lambda-list
                                                (method-class +the-standard-method-class+)
                                                (method-combination +the-standard-method-combination+)
                                                argument-precedence-order
                                                declarations
                                                documentation)
  ;; to avoid circularities, we do not call generic functions in here.
  (declare (ignore generic-function-class))
  (check-argument-precedence-order lambda-list argument-precedence-order)
  (let ((gf (allocate-funcallable-instance +the-standard-generic-function-class+)))
    (unless (classp method-class) (setf method-class (find-class method-class)))
    (unless (typep method-combination 'method-combination)
      (setf method-combination
            (find-method-combination
             gf (car method-combination) (cdr method-combination))))
    (setf (std-slot-value gf 'sys::name) name)
    (setf (std-slot-value gf 'sys::lambda-list) lambda-list)
    (setf (std-slot-value gf 'sys::initial-methods) ())
    (setf (std-slot-value gf 'sys::methods) ())
    (setf (std-slot-value gf 'sys::method-class) method-class)
    (setf (std-slot-value gf 'sys::%method-combination) method-combination)
    (setf (std-slot-value gf 'sys::declarations) declarations)
    (setf (std-slot-value gf 'sys::%documentation) documentation)
    (let* ((plist (analyze-lambda-list (generic-function-lambda-list gf)))
           (required-args (getf plist ':required-args)))
      (setf (std-slot-value gf 'sys::required-args) required-args)
      (setf (std-slot-value gf 'sys::optional-args) (getf plist :optional-args))
      (setf (std-slot-value gf 'sys::argument-precedence-order)
            (or argument-precedence-order required-args)))
    (finalize-standard-generic-function gf)
    gf))

(defun canonicalize-specializers (specializers)
  (mapcar #'canonicalize-specializer specializers))

(defun canonicalize-specializer (specializer)
  (cond ((classp specializer)
         specializer)
        ((typep specializer 'eql-specializer)
         specializer)
        ((symbolp specializer)
         (find-class specializer))
        ((and (consp specializer)
              (eq (car specializer) 'eql))
         (let ((object (cadr specializer)))
           (when (and (consp object)
                      (eq (car object) 'quote))
             (setf object (cadr object)))
           (intern-eql-specializer object)))
        ((and (consp specializer)
              (eq (car specializer) 'java:jclass))
         (let ((jclass (eval specializer)))
           (java::ensure-java-class jclass)))
        (t
         (error "Unknown specializer: ~S" specializer))))

(defun parse-defmethod (args)
  (let ((function-name (car args))
        (qualifiers ())
        (specialized-lambda-list ())
        (body ())
        (parse-state :qualifiers))
    (dolist (arg (cdr args))
      (ecase parse-state
        (:qualifiers
         (if (and (atom arg) (not (null arg)))
             (push arg qualifiers)
             (progn
               (setf specialized-lambda-list arg)
               (setf parse-state :body))))
        (:body (push arg body))))
    (setf qualifiers (nreverse qualifiers)
          body (nreverse body))
    (multiple-value-bind (real-body declarations documentation)
        (parse-body body)
      (values function-name
              qualifiers
              (extract-lambda-list specialized-lambda-list)
              (extract-specializer-names specialized-lambda-list)
              documentation
              declarations
              (list* 'block
                     (fdefinition-block-name function-name)
                     real-body)))))

(defun required-portion (gf args)
  (let ((number-required (length (generic-function-required-arguments gf))))
    (when (< (length args) number-required)
      (error 'program-error
             :format-control "Not enough arguments for generic function ~S."
             :format-arguments (list (generic-function-name gf))))
    (subseq args 0 number-required)))

(defun extract-lambda-list (specialized-lambda-list)
  (let* ((plist (analyze-lambda-list specialized-lambda-list))
         (requireds (getf plist :required-names))
         (rv (getf plist :rest-var))
         (ks (getf plist :key-args))
         (keysp (getf plist :keysp))
         (aok (getf plist :allow-other-keys))
         (opts (getf plist :optional-args))
         (auxs (getf plist :auxiliary-args)))
    `(,@requireds
      ,@(if opts `(&optional ,@opts) ())
      ,@(if rv `(&rest ,rv) ())
      ,@(if (or ks keysp aok) `(&key ,@ks) ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun extract-specializer-names (specialized-lambda-list)
  (let ((plist (analyze-lambda-list specialized-lambda-list)))
    (getf plist ':specializers)))

(defun get-keyword-from-arg (arg)
  (if (listp arg)
      (if (listp (car arg))
          (caar arg)
          (make-keyword (car arg)))
      (make-keyword arg)))

(defun analyze-lambda-list (lambda-list)
  (let ((keys ())           ; Just the keywords
        (key-args ())       ; Keywords argument specs
        (keysp nil)         ;
        (required-names ()) ; Just the variable names
        (required-args ())  ; Variable names & specializers
        (specializers ())   ; Just the specializers
        (rest-var nil)
        (optionals ())
        (auxs ())
        (allow-other-keys nil)
        (state :required))
    (dolist (arg lambda-list)
      (if (member arg lambda-list-keywords)
          (ecase arg
            (&optional
             (unless (eq state :required)
               (error 'program-error
                      :format-control "~A followed by &OPTIONAL not allowed ~
                                       in lambda list ~S"
                      :format-arguments (list state lambda-list)))
             (setq state '&optional))
            (&rest
             (unless (or (eq state :required)
                         (eq state '&optional))
               (error 'program-error
                      :format-control "~A followed by &REST not allowed ~
                                       in lambda list ~S"
                      :format-arguments (list state lambda-list)))
             (setq state '&rest))
            (&key
             (unless (or (eq state :required)
                         (eq state '&optional)
                         (eq state '&rest))
               (error 'program-error
                      :format-control "~A followed by &KEY not allowed
                                       in lambda list ~S"
                      :format-arguments (list state lambda-list)))
             (setq keysp t)
             (setq state '&key))
            (&allow-other-keys
             (unless (eq state '&key)
               (error 'program-error
                      :format-control "&ALLOW-OTHER-KEYS not allowed while
                                       parsing ~A in lambda list ~S"
                      :format-arguments (list state lambda-list)))
             (setq allow-other-keys 't))
            (&aux
             ;; &aux comes last; any other previous state is fine
             (setq state '&aux)))
          (case state
            (:required
             (push-on-end arg required-args)
             (if (listp arg)
                 (progn (push-on-end (car arg) required-names)
                   (push-on-end (cadr arg) specializers))
                 (progn (push-on-end arg required-names)
                   (push-on-end 't specializers))))
            (&optional (push-on-end arg optionals))
            (&rest (setq rest-var arg))
            (&key
             (push-on-end (get-keyword-from-arg arg) keys)
             (push-on-end arg key-args))
            (&aux (push-on-end arg auxs)))))
    (list  :required-names required-names
           :required-args required-args
           :specializers specializers
           :rest-var rest-var
           :keywords keys
           :key-args key-args
           :keysp keysp
           :auxiliary-args auxs
           :optional-args optionals
           :allow-other-keys allow-other-keys)))

#+nil
(defun check-method-arg-info (gf arg-info method)
  (multiple-value-bind (nreq nopt keysp restp allow-other-keys-p keywords)
      (analyze-lambda-list (if (consp method)
                               (early-method-lambda-list method)
                               (method-lambda-list method)))
    (flet ((lose (string &rest args)
                 (error 'simple-program-error
                        :format-control "~@<attempt to add the method~2I~_~S~I~_~
                        to the generic function~2I~_~S;~I~_~
                        but ~?~:>"
                        :format-arguments (list method gf string args)))
           (comparison-description (x y)
                                   (if (> x y) "more" "fewer")))
      (let ((gf-nreq (arg-info-number-required arg-info))
            (gf-nopt (arg-info-number-optional arg-info))
            (gf-key/rest-p (arg-info-key/rest-p arg-info))
            (gf-keywords (arg-info-keys arg-info)))
        (unless (= nreq gf-nreq)
          (lose
           "the method has ~A required arguments than the generic function."
           (comparison-description nreq gf-nreq)))
        (unless (= nopt gf-nopt)
          (lose
           "the method has ~A optional arguments than the generic function."
           (comparison-description nopt gf-nopt)))
        (unless (eq (or keysp restp) gf-key/rest-p)
          (lose
           "the method and generic function differ in whether they accept~_~
            &REST or &KEY arguments."))
        (when (consp gf-keywords)
          (unless (or (and restp (not keysp))
                      allow-other-keys-p
                      (every (lambda (k) (memq k keywords)) gf-keywords))
            (lose "the method does not accept each of the &KEY arguments~2I~_~
            ~S."
                  gf-keywords)))))))

(defun check-method-lambda-list (name method-lambda-list gf-lambda-list)
  (let* ((gf-restp (not (null (memq '&rest gf-lambda-list))))
         (gf-plist (analyze-lambda-list gf-lambda-list))
         (gf-keysp (getf gf-plist :keysp))
         (gf-keywords (getf gf-plist :keywords))
         (method-plist (analyze-lambda-list method-lambda-list))
         (method-restp (not (null (memq '&rest method-lambda-list))))
         (method-keysp (getf method-plist :keysp))
         (method-keywords (getf method-plist :keywords))
         (method-allow-other-keys-p (getf method-plist :allow-other-keys)))
    (unless (= (length (getf gf-plist :required-args))
               (length (getf method-plist :required-args)))
      (error "The method-lambda-list ~S ~
              has the wrong number of required arguments ~
              for the generic function ~S." method-lambda-list name))
    (unless (= (length (getf gf-plist :optional-args))
               (length (getf method-plist :optional-args)))
      (error "The method-lambda-list ~S ~
              has the wrong number of optional arguments ~
              for the generic function ~S." method-lambda-list name))
    (unless (eq (or gf-restp gf-keysp) (or method-restp method-keysp))
      (error "The method-lambda-list ~S ~
              and the generic function ~S ~
              differ in whether they accept &REST or &KEY arguments."
             method-lambda-list name))
    (when (consp gf-keywords)
      (unless (or (and method-restp (not method-keysp))
                  method-allow-other-keys-p
                  (every (lambda (k) (memq k method-keywords)) gf-keywords))
        (error "The method-lambda-list ~S does not accept ~
                all of the keyword arguments defined for the ~
                generic function." method-lambda-list name)))))

(defun check-argument-precedence-order (lambda-list argument-precedence-order)
  (when argument-precedence-order
    (if lambda-list
        ;; raising the required program-errors is a side-effect of
        ;; calculating the given permutation of apo vs req
        (argument-precedence-order-indices
         argument-precedence-order
         (getf (analyze-lambda-list lambda-list) :required-args))
        ;; AMOP pg. 198
        (error 'program-error "argument precedence order specified without lambda list"))))

(defvar *gf-initialize-instance* nil
  "Cached value of the INITIALIZE-INSTANCE generic function.
Initialized with the true value near the end of the file.")
(defvar *gf-allocate-instance* nil
  "Cached value of the ALLOCATE-INSTANCE generic function.
Initialized with the true value near the end of the file.")
(defvar *gf-shared-initialize* nil
  "Cached value of the SHARED-INITIALIZE generic function.
Initialized with the true value near the end of the file.")
(defvar *gf-reinitialize-instance* nil
  "Cached value of the REINITIALIZE-INSTANCE generic function.
Initialized with the true value near the end of the file.")

(declaim (ftype (function * method) ensure-method))
(defun ensure-method (name &rest all-keys)
  (let ((method-lambda-list (getf all-keys :lambda-list))
        (gf (find-generic-function name nil)))
    (when (or (eq gf *gf-initialize-instance*)
              (eq gf *gf-allocate-instance*)
              (eq gf *gf-shared-initialize*)
              (eq gf *gf-reinitialize-instance*))
      ;; ### Clearly, this can be targeted much more exact
      ;; as we only need to remove the specializing class and all
      ;; its subclasses from the hash.
      (clrhash *make-instance-initargs-cache*)
      (clrhash *reinitialize-instance-initargs-cache*))
    (if gf
        (check-method-lambda-list name method-lambda-list
                                  (generic-function-lambda-list gf))
        (setf gf (ensure-generic-function name :lambda-list method-lambda-list)))
    (let ((method
           (if (eq (generic-function-method-class gf) +the-standard-method-class+)
               (apply #'make-instance-standard-method gf all-keys)
               (apply #'make-instance (generic-function-method-class gf) all-keys))))
      (if (and
           (eq (generic-function-method-class gf) +the-standard-method-class+)
           (std-generic-function-p gf))
          (progn
            (std-add-method gf method)
            (map-dependents gf
                            #'(lambda (dep)
                                (update-dependent gf dep 'add-method method))))
          (add-method gf method))
      method)))

(defun make-instance-standard-method (gf
                                      &key
                                      lambda-list
                                      qualifiers
                                      specializers
                                      documentation
                                      function
                                      fast-function)
  (declare (ignore gf))
  (let ((method (std-allocate-instance +the-standard-method-class+))
        (analyzed-args (analyze-lambda-list lambda-list)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (setf (std-slot-value method 'sys::specializers)
          (canonicalize-specializers specializers))
    (setf (method-documentation method) documentation)
    (setf (std-slot-value method 'sys::%generic-function) nil) ; set by add-method
    (setf (std-slot-value method 'sys::%function) function)
    (setf (std-slot-value method 'sys::fast-function) fast-function)
    (setf (std-slot-value method 'sys::keywords) (getf analyzed-args :keywords))
    (setf (std-slot-value method 'sys::other-keywords-p)
          (getf analyzed-args :allow-other-keys))
    method))

;;; To be redefined as generic functions later
(declaim (notinline add-direct-method))
(defun add-direct-method (specializer method)
  (if (typep specializer 'eql-specializer)
      (pushnew method (std-slot-value specializer 'direct-methods))
      (pushnew method (class-direct-methods specializer))))

(declaim (notinline remove-direct-method))
(defun remove-direct-method (specializer method)
  (if (typep specializer 'eql-specializer)
      (setf (std-slot-value specializer 'direct-methods)
            (remove method (std-slot-value specializer 'direct-methods)))
      (setf (class-direct-methods specializer)
            (remove method (class-direct-methods specializer)))))

(defun std-add-method (gf method)
  ;; calls sites need to make sure that method is either a method of the
  ;; given gf or does not have a gf.
  (let ((old-method (%find-method gf (std-method-qualifiers method)
                                 (method-specializers method) nil)))
    (when old-method
      (if (and (std-generic-function-p gf)
               (eq (class-of old-method) +the-standard-method-class+))
          (std-remove-method gf old-method)
          (remove-method gf old-method))))
  (setf (std-slot-value method 'sys::%generic-function) gf)
  (push method (std-slot-value gf 'sys::methods))
  (dolist (specializer (method-specializers method))
    (add-direct-method specializer method))
  (finalize-standard-generic-function gf)
  gf)

(defun std-remove-method (gf method)
  (setf (std-slot-value gf 'sys::methods)
        (remove method (generic-function-methods gf)))
  (setf (std-slot-value method 'sys::%generic-function) nil)
  (dolist (specializer (method-specializers method))
    (remove-direct-method specializer method))
  (finalize-standard-generic-function gf)
  gf)

(defun %find-method (gf qualifiers specializers &optional (errorp t))
  ;; "If the specializers argument does not correspond in length to the number
  ;; of required arguments of the generic-function, an an error of type ERROR
  ;; is signaled."
  (unless (= (length specializers) (length (generic-function-required-arguments gf)))
    (error "The specializers argument has length ~S, but ~S has ~S required parameters."
           (length specializers)
           gf
           (length (generic-function-required-arguments gf))))
  (let* ((canonical-specializers (canonicalize-specializers specializers))
         (method
          (find-if #'(lambda (method)
                      (and (equal qualifiers
                                  (method-qualifiers method))
                           (equal canonical-specializers
                                  (method-specializers method))))
                   (generic-function-methods gf))))
    (if (and (null method) errorp)
        (error "No such method for ~S." (generic-function-name gf))
        method)))

(defun fast-callable-p (gf)
  (and (eq (method-combination-name (generic-function-method-combination gf))
           'standard)
       (null (intersection (generic-function-lambda-list gf)
                           '(&rest &optional &key &allow-other-keys &aux)))))

(defun std-compute-discriminating-function (gf)
  ;; In this function, we know that gf is of class
  ;; standard-generic-function, so we can access the instance's slots
  ;; via std-slot-value.  This breaks circularities when redefining
  ;; generic function accessors.
  (let ((methods (std-slot-value gf 'sys::methods)))
    (cond
      ((and (= (length methods) 1)
            (eq (type-of (car methods)) 'standard-reader-method)
            (eq (type-of (car (std-method-specializers (car methods))))
                'standard-class))
       (let* ((method (first methods))
              (slot-definition (std-slot-value method 'sys::%slot-definition))
              (slot-name (std-slot-value slot-definition 'sys:name))
              (class (car (std-method-specializers method))))
         #'(lambda (instance)
             ;; TODO: elide this test for low values of SAFETY
             (unless (typep instance class)
               (no-applicable-method gf (list instance)))
             ;; hash table lookup for slot position in Layout object via
             ;; StandardObject.SLOT_VALUE, so should be reasonably fast
             (std-slot-value instance slot-name))))
      ((and (= (length methods) 1)
            (eq (type-of (car methods)) 'standard-writer-method)
            (eq (type-of (second (std-method-specializers (car methods))))
                'standard-class))
       (let* ((method (first methods))
              (slot-definition (std-slot-value method 'sys::%slot-definition))
              (slot-name (std-slot-value slot-definition 'sys:name))
              (class (car (std-method-specializers method))))
         #'(lambda (new-value instance)
             ;; TODO: elide this test for low values of SAFETY
             (unless (typep instance class)
               (no-applicable-method gf (list new-value instance)))
             ;; hash table lookup for slot position in Layout object via
             ;; StandardObject.SET_SLOT_VALUE, so should be reasonably fast
             (setf (std-slot-value instance slot-name) new-value))))
      (t
       (let* ((number-required (length (generic-function-required-arguments gf)))
              (lambda-list (generic-function-lambda-list gf))
              (exact (null (intersection lambda-list
                                         '(&rest &optional &key
                                           &allow-other-keys))))
              (no-aux (null (some 
                             (lambda (method) 
                               (find '&aux (std-slot-value method 'sys::lambda-list)))
                             methods))))
         (if (and exact
                  no-aux)
             (cond
               ((= number-required 1)
                (cond
                  ((and (eq (method-combination-name
                             (std-slot-value gf 'sys::%method-combination))
                            'standard)
                        (= (length methods) 1)
                        (std-method-fast-function (%car methods)))
                   (let* ((method (%car methods))
                          (specializer (car (std-method-specializers method)))
                          (function (std-method-fast-function method)))
                     (if (typep specializer 'eql-specializer)
                         (let ((specializer-object (eql-specializer-object specializer)))
                           #'(lambda (arg)
                               (declare (optimize speed))
                               (if (eql arg specializer-object)
                                   (funcall function arg)
                                   (no-applicable-method gf (list arg)))))
                         #'(lambda (arg)
                             (declare (optimize speed))
                             (unless (simple-typep arg specializer)
                               ;; FIXME no applicable method
                               (error 'simple-type-error
                                      :datum arg
                                      :expected-type specializer))
                             (funcall function arg)))))
                  (t
                   #'(lambda (arg)
                       (declare (optimize speed))
                       (let* ((args (list arg))
                              (emfun (get-cached-emf gf args)))
                         (if emfun
                             (funcall emfun args)
                             (slow-method-lookup gf args)))))))
               ((= number-required 2)
                #'(lambda (arg1 arg2)
                    (declare (optimize speed))
                    (let* ((args (list arg1 arg2))
                           (emfun (get-cached-emf gf args)))
                      (if emfun
                          (funcall emfun args)
                          (slow-method-lookup gf args)))))
               ((= number-required 3)
                #'(lambda (arg1 arg2 arg3)
                    (declare (optimize speed))
                    (let* ((args (list arg1 arg2 arg3))
                           (emfun (get-cached-emf gf args)))
                      (if emfun
                          (funcall emfun args)
                          (slow-method-lookup gf args)))))
               (t
                #'(lambda (&rest args)
                    (declare (optimize speed))
                    (let ((len (length args)))
                      (unless (= len number-required)
                        (error 'program-error
                               :format-control "Not enough arguments for generic function ~S."
                               :format-arguments (list (generic-function-name gf)))))
                    (let ((emfun (get-cached-emf gf args)))
                      (if emfun
                          (funcall emfun args)
                          (slow-method-lookup gf args))))))
             #'(lambda (&rest args)
                 (declare (optimize speed))
                 (let ((len (length args)))
                   (unless (>= len number-required)
                     (error 'program-error
                            :format-control "Not enough arguments for generic function ~S."
                            :format-arguments (list (generic-function-name gf)))))
                 (let ((emfun (get-cached-emf gf args)))
                   (if emfun
                       (funcall emfun args)
                       (slow-method-lookup gf args))))))))))

(defun sort-methods (methods gf required-classes)
  (if (or (null methods) (null (%cdr methods)))
      methods
      (sort methods
            (if (std-generic-function-p gf)
                (let ((method-indices
                       (argument-precedence-order-indices
                        (generic-function-argument-precedence-order gf)
                        (getf (analyze-lambda-list (generic-function-lambda-list gf))
                              ':required-args))))
                  #'(lambda (m1 m2)
                      (std-method-more-specific-p
                       m1 m2 required-classes method-indices)))
                #'(lambda (m1 m2)
                    (method-more-specific-p gf m1 m2 required-classes))))))

(defun method-applicable-p (method args)
  (do* ((specializers (method-specializers method) (cdr specializers))
        (args args (cdr args)))
       ((null specializers) t)
    (let ((specializer (car specializers)))
      (if (typep specializer 'eql-specializer)
          (unless (eql (car args) (eql-specializer-object specializer))
            (return nil))
          (unless (subclassp (class-of (car args)) specializer)
            (return nil))))))

(defun std-compute-applicable-methods (gf args)
  (let ((required-classes (mapcar #'class-of (required-portion gf args)))
        (methods '()))
    (dolist (method (generic-function-methods gf))
      (when (method-applicable-p method args)
        (push method methods)))
    (sort-methods methods gf required-classes)))

(declaim (notinline compute-applicable-methods))
(defun compute-applicable-methods (gf args)
  (std-compute-applicable-methods gf args))

;;; METHOD-APPLICABLE-USING-CLASSES-P
;;;
;;; If the first return value is T, METHOD is definitely applicable to
;;; arguments that are instances of CLASSES.  If the first value is
;;; NIL and the second value is T, METHOD is definitely not applicable
;;; to arguments that are instances of CLASSES; if the second value is
;;; NIL the applicability of METHOD cannot be determined by inspecting
;;; the classes of its arguments only.
;;;
(defun method-applicable-using-classes-p (method classes)
  (do* ((specializers (method-specializers method) (cdr specializers))
        (classes classes (cdr classes))
        (knownp t))
       ((null specializers)
        (if knownp (values t t) (values nil nil)))
    (let ((specializer (car specializers)))
      (if (typep specializer 'eql-specializer)
          (if (eql (class-of (eql-specializer-object specializer)) 
                   (car classes))
              (setf knownp nil)
              (return (values nil t)))
          (unless (subclassp (car classes) specializer)
            (return (values nil t)))))))

(defun check-applicable-method-keyword-args (gf args
                                             keyword-args
                                             applicable-keywords)
  (when (oddp (length keyword-args))
    (error 'program-error
           :format-control "Odd number of keyword arguments in call to ~S ~
with arguments list ~S"
           :format-arguments (list gf args)))
  (unless (getf keyword-args :allow-other-keys)
    (loop for key in keyword-args by #'cddr
       unless (or (member key applicable-keywords)
                  (eq key :allow-other-keys))
       do (error 'program-error
                 :format-control "Invalid keyword argument ~S in call ~
to ~S with argument list ~S."
                 :format-arguments (list key gf args)))))

(defun compute-applicable-keywords (gf applicable-methods)
  (let ((applicable-keywords
         (getf (analyze-lambda-list (generic-function-lambda-list gf))
               :keywords)))
    (loop for method in applicable-methods
       do (multiple-value-bind
                (keywords allow-other-keys)
              (function-keywords method)
            (when allow-other-keys
              (setf applicable-keywords :any)
              (return))
            (setf applicable-keywords
                  (union applicable-keywords keywords))))
    applicable-keywords))

(defun wrap-emfun-for-keyword-args-check (gf emfun non-keyword-args
                                          applicable-keywords)
  #'(lambda (args)
      (check-applicable-method-keyword-args
         gf args
         (nthcdr non-keyword-args args) applicable-keywords)
      (funcall emfun args)))

(defun slow-method-lookup (gf args)
  (let ((applicable-methods
          (if (std-generic-function-p gf)
              (std-compute-applicable-methods gf args)
              (or (compute-applicable-methods-using-classes gf (mapcar #'class-of args))
                  (compute-applicable-methods gf args)))))
    (if applicable-methods
        (let* ((emfun (funcall (if (std-generic-function-p gf)
                                   #'std-compute-effective-method
                                   #'compute-effective-method)
                               gf (generic-function-method-combination gf)
                               applicable-methods))
               (non-keyword-args (+ (length (generic-function-required-arguments gf))
                                    (length (generic-function-optional-arguments gf))))
               (gf-lambda-list (generic-function-lambda-list gf))
               (checks-required (and (member '&key gf-lambda-list)
                                     (not (member '&allow-other-keys
                                                  gf-lambda-list))))
              (applicable-keywords
               (when checks-required
                 ;; Don't do applicable keyword checks when this is
                 ;; one of the 'exceptional four' or when the gf allows
                 ;; other keywords.
                 (compute-applicable-keywords gf applicable-methods))))
          (when (and checks-required
                     (not (eq applicable-keywords :any)))
            (setf emfun
                  (wrap-emfun-for-keyword-args-check gf emfun non-keyword-args
                                                     applicable-keywords)))
          (cache-emf gf args emfun)
          (funcall emfun args))
        (apply #'no-applicable-method gf args))))

(defun sub-specializer-p (c1 c2 c-arg)
  (find c2 (cdr (memq c1 (%class-precedence-list c-arg)))))

(defun std-method-more-specific-p (method1 method2 required-classes argument-precedence-order)
  (if argument-precedence-order
      (let ((specializers-1 (std-method-specializers method1))
            (specializers-2 (std-method-specializers method2)))
        (dolist (index argument-precedence-order)
          (let ((spec1 (nth index specializers-1))
                (spec2 (nth index specializers-2)))
            (unless (eq spec1 spec2)
              (cond ((typep spec1 'eql-specializer)
                     (return t))
                    ((typep spec2 'eql-specializer)
                     (return nil))
                    (t
                     (return (sub-specializer-p spec1 spec2
                                                (nth index required-classes)))))))))
      (do ((specializers-1 (std-method-specializers method1) (cdr specializers-1))
           (specializers-2 (std-method-specializers method2) (cdr specializers-2))
           (classes required-classes (cdr classes)))
          ((null specializers-1) nil)
        (let ((spec1 (car specializers-1))
              (spec2 (car specializers-2)))
          (unless (eq spec1 spec2)
            (cond ((typep spec1 'eql-specializer)
                   (return t))
                  ((typep spec2 'eql-specializer)
                   (return nil))
                  (t
                   (return (sub-specializer-p spec1 spec2 (car classes))))))))))

(defun primary-method-p (method)
  (null (intersection '(:before :after :around) (method-qualifiers method))))

(defun before-method-p (method)
  (equal '(:before) (method-qualifiers method)))

(defun after-method-p (method)
  (equal '(:after) (method-qualifiers method)))

(defun around-method-p (method)
  (equal '(:around) (method-qualifiers method)))

(defun process-next-method-list (next-method-list)
  (mapcar #'(lambda (next-method-form)
              (cond
                ((listp next-method-form)
                 (assert (eq (first next-method-form) 'make-method))
                 (let* ((rest-sym (gensym)))
                   (make-instance-standard-method
                    nil ;; ignored
                    :lambda-list (list '&rest rest-sym)
                    :function (compute-method-function `(lambda (&rest ,rest-sym)
                                                          ,(second next-method-form))))))
                (t
                 (assert (typep next-method-form 'method))
                 next-method-form)))
          next-method-list))

(defun std-compute-effective-method (gf method-combination methods)
  (assert (typep method-combination 'method-combination))
  (let* ((mc-name (method-combination-name method-combination))
         (options (slot-value method-combination 'options))
         (order (car options))
         (primaries '())
         (arounds '())
         around
         emf-form
         (long-method-combination-p
          (typep method-combination 'long-method-combination)))
    (unless long-method-combination-p
      (dolist (m methods)
        (let ((qualifiers (method-qualifiers m)))
          (cond ((null qualifiers)
                 (if (eq mc-name 'standard)
                     (push m primaries)
                     (error "Method combination type mismatch: missing qualifier for method combination ~S." method-combination)))
                ((cdr qualifiers)
                 (error "Invalid method qualifiers."))
                ((eq (car qualifiers) :around)
                 (push m arounds))
                ((eq (car qualifiers) mc-name)
                 (push m primaries))
                ((memq (car qualifiers) '(:before :after)))
                (t
                 (error "Invalid method qualifiers."))))))
    (unless (eq order :most-specific-last)
      (setf primaries (nreverse primaries)))
    (setf arounds (nreverse arounds))
    (setf around (car arounds))
    (when (and (null primaries) (not long-method-combination-p))
      (error "No primary methods for the generic function ~S." gf))
    (cond
      (around
       (let ((next-emfun
              (funcall
               (if (std-generic-function-p gf)
                   #'std-compute-effective-method
                   #'compute-effective-method)
               gf method-combination (remove around methods))))
         (setf emf-form
               (generate-emf-lambda (method-function around) next-emfun))))
      ((eq mc-name 'standard)
       (let* ((next-emfun (compute-primary-emfun (cdr primaries)))
              (befores (remove-if-not #'before-method-p methods))
              (reverse-afters
               (reverse (remove-if-not #'after-method-p methods))))
         (setf emf-form
               (cond
                 ((and (null befores) (null reverse-afters))
                  (let ((fast-function (std-method-fast-function (car primaries))))
                    (if fast-function
                        (ecase (length (generic-function-required-arguments gf))
                          (1
                           #'(lambda (args)
                               (declare (optimize speed))
                               (funcall fast-function (car args))))
                          (2
                           #'(lambda (args)
                               (declare (optimize speed))
                               (funcall fast-function (car args) (cadr args)))))
                        (generate-emf-lambda (std-method-function (car primaries))
                                             next-emfun))))
                 (t
                  (let ((method-function (method-function (car primaries))))
                    #'(lambda (args)
                        (declare (optimize speed))
                        (dolist (before befores)
                          (funcall (method-function before) args nil))
                        (multiple-value-prog1
                            (funcall method-function args next-emfun)
                          (dolist (after reverse-afters)
                            (funcall (method-function after) args nil))))))))))
      (long-method-combination-p
       (let ((function (long-method-combination-function method-combination))
             (arguments (slot-value method-combination 'options)))
         (assert function)
         (setf emf-form
               (if arguments
                   (apply function gf methods arguments)
                   (funcall function gf methods)))))
      (t
       (unless (typep method-combination 'short-method-combination)
         (error "Unsupported method combination type ~A." mc-name))
       (let ((operator (short-method-combination-operator method-combination))
             (ioa (short-method-combination-identity-with-one-argument method-combination)))
         (setf emf-form
               (if (and ioa (null (cdr primaries)))
                   (generate-emf-lambda (method-function (car primaries)) nil)
                   `(lambda (args)
                      (,operator ,@(mapcar
                                    (lambda (primary)
                                      `(funcall ,(method-function primary) args nil))
                                    primaries))))))))
    (assert (not (null emf-form)))
    (or #+nil (ignore-errors (autocompile emf-form))
        (coerce-to-function emf-form))))

(defun generate-emf-lambda (method-function next-emfun)
  #'(lambda (args)
      (declare (optimize speed))
      (funcall method-function args next-emfun)))

;;; compute an effective method function from a list of primary methods:

(defun compute-primary-emfun (methods)
  (if (null methods)
      nil
      (let ((next-emfun (compute-primary-emfun (cdr methods))))
        #'(lambda (args)
           (funcall (std-method-function (car methods)) args next-emfun)))))

(defvar *call-next-method-p*)
(defvar *next-method-p-p*)

(defun walk-form (form)
  (cond ((atom form)
         (cond ((eq form 'call-next-method)
                (setf *call-next-method-p* t))
               ((eq form 'next-method-p)
                (setf *next-method-p-p* t))))
        (t
         (walk-form (%car form))
         (walk-form (%cdr form)))))

(defun compute-method-function (lambda-expression)
  (let ((lambda-list (allow-other-keys (cadr lambda-expression)))
        (body (cddr lambda-expression))
        (*call-next-method-p* nil)
        (*next-method-p-p* nil))
    (multiple-value-bind (body declarations) (parse-body body)
      (let ((ignorable-vars '()))
        (dolist (var lambda-list)
          (if (memq var lambda-list-keywords)
              (return)
              (push var ignorable-vars)))
        (push `(declare (ignorable ,@ignorable-vars)) declarations))
      (walk-form body)
      (cond ((or *call-next-method-p* *next-method-p-p*)
             `(lambda (args next-emfun)
                (flet ((call-next-method (&rest cnm-args)
                         (if (null next-emfun)
                             (error "No next method for generic function.")
                             (funcall next-emfun (or cnm-args args))))
                       (next-method-p ()
                         (not (null next-emfun))))
                  (declare (ignorable (function call-next-method)
                                      (function next-method-p)))
                  (apply #'(lambda ,lambda-list ,@declarations ,@body) args))))
            ((null (intersection lambda-list '(&rest &optional &key &allow-other-keys &aux)))
             ;; Required parameters only.
             (case (length lambda-list)
               (1
                `(lambda (args next-emfun)
                   (declare (ignore next-emfun))
                   (let ((,(%car lambda-list) (%car args)))
                     (declare (ignorable ,(%car lambda-list)))
                     ,@declarations ,@body)))
               (2
                `(lambda (args next-emfun)
                   (declare (ignore next-emfun))
                   (let ((,(%car lambda-list) (%car args))
                         (,(%cadr lambda-list) (%cadr args)))
                     (declare (ignorable ,(%car lambda-list)
                                         ,(%cadr lambda-list)))
                     ,@declarations ,@body)))
               (3
                `(lambda (args next-emfun)
                   (declare (ignore next-emfun))
                   (let ((,(%car lambda-list) (%car args))
                         (,(%cadr lambda-list) (%cadr args))
                         (,(%caddr lambda-list) (%caddr args)))
                     (declare (ignorable ,(%car lambda-list)
                                         ,(%cadr lambda-list)
                                         ,(%caddr lambda-list)))
                     ,@declarations ,@body)))
               (t
                `(lambda (args next-emfun)
                   (declare (ignore next-emfun))
                   (apply #'(lambda ,lambda-list ,@declarations ,@body) args)))))
            (t
             `(lambda (args next-emfun)
                (declare (ignore next-emfun))
                (apply #'(lambda ,lambda-list ,@declarations ,@body) args)))))))

(defun compute-method-fast-function (lambda-expression)
  (let ((lambda-list (allow-other-keys (cadr lambda-expression))))
    (when (intersection lambda-list '(&rest &optional &key &allow-other-keys &aux))
      (return-from compute-method-fast-function nil))
    ;; Only required args.
    (let ((body (cddr lambda-expression))
          (*call-next-method-p* nil)
          (*next-method-p-p* nil))
      (multiple-value-bind (body declarations) (parse-body body)
        (walk-form body)
        (when (or *call-next-method-p* *next-method-p-p*)
          (return-from compute-method-fast-function nil))
        (let ((decls `(declare (ignorable ,@lambda-list))))
          (setf lambda-expression
                (list* (car lambda-expression)
                       (cadr lambda-expression)
                       decls
                       (cddr lambda-expression))))
        (case (length lambda-list)
          (1
;;            `(lambda (args next-emfun)
;;               (let ((,(%car lambda-list) (%car args)))
;;                 (declare (ignorable ,(%car lambda-list)))
;;                 ,@declarations ,@body)))
           lambda-expression)
          (2
;;            `(lambda (args next-emfun)
;;               (let ((,(%car lambda-list) (%car args))
;;                     (,(%cadr lambda-list) (%cadr args)))
;;                 (declare (ignorable ,(%car lambda-list)
;;                                     ,(%cadr lambda-list)))
;;                 ,@declarations ,@body)))
           lambda-expression)
;;           (3
;;            `(lambda (args next-emfun)
;;               (let ((,(%car lambda-list) (%car args))
;;                     (,(%cadr lambda-list) (%cadr args))
;;                     (,(%caddr lambda-list) (%caddr args)))
;;                 (declare (ignorable ,(%car lambda-list)
;;                                     ,(%cadr lambda-list)
;;                                     ,(%caddr lambda-list)))
;;                 ,@declarations ,@body)))
          (t
           nil))))))

(declaim (notinline make-method-lambda))
(defun make-method-lambda (generic-function method lambda-expression env)
  (declare (ignore generic-function method env))
  (values (compute-method-function lambda-expression) nil))


;; From CLHS section 7.6.5:
;; "When a generic function or any of its methods mentions &key in a lambda
;; list, the specific set of keyword arguments accepted by the generic function
;; varies according to the applicable methods. The set of keyword arguments
;; accepted by the generic function for a particular call is the union of the
;; keyword arguments accepted by all applicable methods and the keyword
;; arguments mentioned after &key in the generic function definition, if any."
;; Adapted from Sacla.
(defun allow-other-keys (lambda-list)
  (if (and (member '&key lambda-list)
           (not (member '&allow-other-keys lambda-list)))
      (let* ((key-end (or (position '&aux lambda-list) (length lambda-list)))
             (aux-part (subseq lambda-list key-end)))
        `(,@(subseq lambda-list 0 key-end) &allow-other-keys ,@aux-part))
      lambda-list))

(defmacro defmethod (&rest args &environment env)
  (multiple-value-bind
      (function-name qualifiers lambda-list specializers documentation declarations body)
      (parse-defmethod args)
    (let* ((specializers-form '())
           (lambda-expression `(lambda ,lambda-list ,@declarations ,body))
           (gf (or (find-generic-function function-name nil)
                   (class-prototype (find-class 'standard-generic-function))))
           (method-function
             (make-method-lambda gf (class-prototype (generic-function-method-class gf))
                                 lambda-expression env))
           (fast-function (compute-method-fast-function lambda-expression))
           )
      (dolist (specializer specializers)
        (cond ((and (consp specializer) (eq (car specializer) 'eql))
               (push `(list 'eql ,(cadr specializer)) specializers-form))
              (t
               (push `',specializer specializers-form))))
      (setf specializers-form `(list ,@(nreverse specializers-form)))
      `(progn
	 (sys::record-source-information-for-type ',function-name '(:method ,function-name ,qualifiers ,specializers))
         (ensure-method ',function-name
                        :lambda-list ',lambda-list
                        :qualifiers ',qualifiers
                        :specializers (canonicalize-specializers ,specializers-form)
                        ,@(if documentation `(:documentation ,documentation))
                        :function (function ,method-function)
                        ,@(if fast-function `(:fast-function (function ,fast-function)))
                        )))))

;;; Reader and writer methods

(defun make-instance-standard-accessor-method (method-class
                                               &key
                                               lambda-list
                                               qualifiers
                                               specializers
                                               documentation
                                               function
                                               fast-function
                                               slot-definition)
  (let ((method (std-allocate-instance method-class)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (setf (std-slot-value method 'sys::specializers)
          (canonicalize-specializers specializers))
    (setf (method-documentation method) documentation)
    (setf (std-slot-value method 'sys::%generic-function) nil)
    (setf (std-slot-value method 'sys::%function) function)
    (setf (std-slot-value method 'sys::fast-function) fast-function)
    (setf (std-slot-value method 'sys::%slot-definition) slot-definition)
    (setf (std-slot-value method 'sys::keywords) nil)
    (setf (std-slot-value method 'sys::other-keywords-p) nil)
    method))

(defun add-reader-method (class function-name slot-definition)
  (let* ((slot-name (slot-definition-name slot-definition))
         (lambda-expression
          (if (std-class-p class)
              `(lambda (object) (std-slot-value object ',slot-name))
              `(lambda (object) (slot-value object ',slot-name))))
         (method-function (compute-method-function lambda-expression))
         (fast-function (compute-method-fast-function lambda-expression))
         (method-lambda-list '(object))
         (gf (find-generic-function function-name nil))
         (initargs `(:lambda-list ,method-lambda-list
                     :qualifiers ()
                     :specializers (,class)
                     :function ,(if (autoloadp 'compile)
                                    method-function
                                    (autocompile method-function))
                     :fast-function ,(if (autoloadp 'compile)
                                         fast-function
                                         (autocompile fast-function))
                     :slot-definition ,slot-definition))
         (method-class (if (std-class-p class)
                           +the-standard-reader-method-class+
                           (apply #'reader-method-class class slot-definition
                                  initargs))))
    ;; required by AMOP pg. 225
    (assert (subtypep method-class +the-standard-reader-method-class+))
    (if gf
        (check-method-lambda-list function-name
                                  method-lambda-list
                                  (generic-function-lambda-list gf))
        (setf gf (ensure-generic-function function-name
                                          :lambda-list method-lambda-list)))
    (let ((method
           (if (eq method-class +the-standard-reader-method-class+)
               (apply #'make-instance-standard-accessor-method method-class
                      initargs)
               (apply #'make-instance method-class
                      :generic-function nil ; handled by add-method
                      initargs))))
      (if (std-generic-function-p gf)
          (progn
            (std-add-method gf method)
            (map-dependents gf
                            #'(lambda (dep)
                                (update-dependent gf dep 'add-method method))))
          (add-method gf method))
      method)))

(defun add-writer-method (class function-name slot-definition)
  (let* ((slot-name (slot-definition-name slot-definition))
         (lambda-expression
          (if (std-class-p class)
              `(lambda (new-value object)
                 (setf (std-slot-value object ',slot-name) new-value))
              `(lambda (new-value object)
                 (setf (slot-value object ',slot-name) new-value))))
         (method-function (compute-method-function lambda-expression))
         (fast-function (compute-method-fast-function lambda-expression))
         (method-lambda-list '(new-value object))
         (gf (find-generic-function function-name nil))
         (initargs `(:lambda-list ,method-lambda-list
                     :qualifiers ()
                     :specializers (,+the-T-class+ ,class)
                     :function ,(if (autoloadp 'compile)
                                    method-function
                                    (autocompile method-function))
                     :fast-function ,(if (autoloadp 'compile)
                                         fast-function
                                         (autocompile fast-function))
                     :slot-definition ,slot-definition))
         (method-class (if (std-class-p class)
                           +the-standard-writer-method-class+
                           (apply #'writer-method-class class slot-definition
                                  initargs))))
    ;; required by AMOP pg. 242
    (assert (subtypep method-class +the-standard-writer-method-class+))
    (if gf
        (check-method-lambda-list function-name
                                  method-lambda-list
                                  (generic-function-lambda-list gf))
        (setf gf (ensure-generic-function function-name
                                          :lambda-list method-lambda-list)))
    (let ((method
           (if (eq method-class +the-standard-writer-method-class+)
               (apply #'make-instance-standard-accessor-method method-class
                      initargs)
               (apply #'make-instance method-class
                      :generic-function nil ; handled by add-method
                      initargs))))
      (if (std-generic-function-p gf)
          (progn
            (std-add-method gf method)
            (map-dependents gf
                            #'(lambda (dep)
                                (update-dependent gf dep 'add-method method))))
          (add-method gf method))
      method)))

(defmacro atomic-defgeneric (function-name &rest rest)
  "Macro to define a generic function and 'swap it into place' after
it's been fully defined with all its methods.

Note: the user should really use the (:method ..) method description
way of defining methods; there's not much use in atomically defining
generic functions without providing sensible behaviour."
  (let ((temp-sym (gensym)))
    `(progn
       (defgeneric ,temp-sym ,@rest)
       (sys::record-source-information-for-type ',function-name '(:generic-function ,function-name))
       ,@(loop for method-form in rest
	       when (eq (car method-form) :method)
		    collect
		    (multiple-value-bind (function-name qualifiers lambda-list specializers documentation declarations body) 
			(mop::parse-defmethod `(,function-name ,@(rest method-form)))
		      `(sys::record-source-information-for-type ',function-name '(:method ,function-name ,qualifiers ,specializers))))
       (let ((gf (symbol-function ',temp-sym)))
         ;; FIXME (rudi 2012-07-08): fset gets the source location info
         ;; to charpos 23 always (but (setf fdefinition) leaves the
         ;; outdated source position in place, which is even worse).
         (fset ',function-name gf)
         (setf (std-slot-value gf 'sys::name) ',function-name)
         (fmakunbound ',temp-sym)
         gf))))

(defmacro redefine-class-forwarder (name slot &optional body-alist)
  "Define a generic function on a temporary symbol as an accessor
for the slot `slot'. Then, when definition is complete (including
allocation of methods), swap the definition in place.

`body-alist' can be used to override the default method bodies for given
metaclasses.  In substitute method bodies, `class' names the class
instance and, for setters, `new-value' the new value."
  (let* ((setterp (consp name))
         (%name
          (intern (concatenate 'string
                               "%"
                               (if setterp (symbol-name 'set-) "")
                               (symbol-name (if setterp (cadr name) name)))
                  (find-package "SYS")))
         (bodies
          (append body-alist
                  (if setterp
                      `((built-in-class . (,%name new-value class))
                        (forward-referenced-class . (,%name new-value class))
                        (structure-class . (,%name new-value class))
                        (standard-class . (setf (slot-value class ',slot)
                                                new-value))
                        (funcallable-standard-class . (setf (slot-value class ',slot)
                                                            new-value)))
                      `((built-in-class . (,%name class))
                        (forward-referenced-class . (,%name class))
                        (structure-class . (,%name class))
                        (standard-class . (slot-value class ',slot))
                        (funcallable-standard-class . (slot-value class ',slot)))))))
    `(atomic-defgeneric ,name (,@(when setterp (list 'new-value)) class)
        ,@(mapcar #'(lambda (class-name)
                      `(:method (,@(when setterp (list 'new-value))
                                 (class ,class-name))
                         ,(cdr (assoc class-name bodies))))
                  '(built-in-class forward-referenced-class structure-class
                    standard-class funcallable-standard-class)))))

;;; The slot names here must agree with the ones defined in
;;; StandardClass.java:layoutStandardClass.
(redefine-class-forwarder class-name sys:name)
;;; AMOP pg. 230
(redefine-class-forwarder (setf class-name) sys:name
   ((standard-class . (progn (reinitialize-instance class :name new-value) new-value))
    (funcallable-standard-class . (progn (reinitialize-instance class :name new-value) new-value))))
(redefine-class-forwarder class-slots sys:slots)
(redefine-class-forwarder (setf class-slots) sys:slots)
(redefine-class-forwarder class-direct-slots sys:direct-slots)
(redefine-class-forwarder (setf class-direct-slots) sys:direct-slots)
(redefine-class-forwarder class-layout sys:layout)
(redefine-class-forwarder (setf class-layout) sys:layout)
(redefine-class-forwarder class-direct-superclasses sys:direct-superclasses)
(redefine-class-forwarder (setf class-direct-superclasses) sys:direct-superclasses)
(redefine-class-forwarder class-direct-subclasses sys:direct-subclasses)
(redefine-class-forwarder (setf class-direct-subclasses) sys:direct-subclasses)
(redefine-class-forwarder class-direct-methods sys:direct-methods)
(redefine-class-forwarder (setf class-direct-methods) sys:direct-methods)
(redefine-class-forwarder class-precedence-list sys:precedence-list)
(redefine-class-forwarder (setf class-precedence-list) sys:precedence-list)
(redefine-class-forwarder class-finalized-p sys:finalized-p)
(redefine-class-forwarder (setf class-finalized-p) sys:finalized-p)
(redefine-class-forwarder class-default-initargs sys:default-initargs)
(redefine-class-forwarder (setf class-default-initargs) sys:default-initargs)
(redefine-class-forwarder class-direct-default-initargs sys:direct-default-initargs)
(redefine-class-forwarder (setf class-direct-default-initargs) sys:direct-default-initargs)

;;; Class definition

(defun check-duplicate-slots (slots)
  (flet ((canonical-slot-name (canonical-slot)
           (getf canonical-slot :name)))
    (dolist (s1 slots)
      (let ((name1 (canonical-slot-name s1)))
        (dolist (s2 (cdr (memq s1 slots)))
          (when (eq name1 (canonical-slot-name s2))
            (error 'program-error "Duplicate slot ~S" name1)))))))

(defun check-duplicate-default-initargs (initargs)
  (let ((names ()))
    (dolist (initarg initargs)
      (push (car initarg) names))
    (do* ((names names (cdr names))
          (name (car names) (car names)))
         ((null names))
      (when (memq name (cdr names))
        (error 'program-error
               :format-control "Duplicate initialization argument name ~S in :DEFAULT-INITARGS."
               :format-arguments (list name))))))

(defun canonicalize-direct-superclasses (direct-superclasses)
  (let ((classes '()))
    (dolist (class-specifier direct-superclasses)
      (let ((class (if (classp class-specifier)
                       class-specifier
                       (find-class class-specifier nil))))
        (unless class
          (setf class (make-instance +the-forward-referenced-class+
                                     :name class-specifier))
          (setf (find-class class-specifier) class))
        (when (and (typep class 'built-in-class)
                   (not (member class *extensible-built-in-classes*)))
          (error "Attempt to define a subclass of built-in-class ~S."
                 class-specifier))
        (push class classes)))
    (nreverse classes)))

(atomic-defgeneric add-direct-subclass (superclass subclass)
  (:method ((superclass class) (subclass class))
    (setf (class-direct-subclasses superclass)
          (adjoin subclass (class-direct-subclasses superclass)))))

(atomic-defgeneric remove-direct-subclass (superclass subclass)
  (:method ((superclass class) (subclass class))
    (setf (class-direct-subclasses superclass)
          (remove subclass (class-direct-subclasses superclass)))))

 ;;; AMOP pg. 182
(defun ensure-class (name &rest all-keys &key &allow-other-keys)
  (let ((class (find-class name nil)))
    ;; CLHS DEFCLASS: "If a class with the same proper name already
    ;; exists [...] the existing class is redefined."  Ansi-tests
    ;; CLASS-0309 and CLASS-0310.1 demand this behavior.
    (if (and class (eql (class-name class) name))
        (apply #'ensure-class-using-class class name all-keys)
        (apply #'ensure-class-using-class nil name all-keys))))

;;; AMOP pg. 183ff.
(defgeneric ensure-class-using-class (class name &key direct-default-initargs
                                      direct-slots direct-superclasses
                                      metaclass &allow-other-keys))

(defmethod ensure-class-using-class :before (class name  &key direct-slots
                                             direct-default-initargs 
                                             &allow-other-keys)
  (check-duplicate-slots direct-slots)
  (check-duplicate-default-initargs direct-default-initargs))

(defmethod ensure-class-using-class ((class null) name &rest all-keys
                                     &key (metaclass +the-standard-class+)
                                     direct-superclasses
                                     &allow-other-keys)
  (setf all-keys (copy-list all-keys))  ; since we modify it
  (remf all-keys :metaclass)
  (unless (classp metaclass) (setf metaclass (find-class metaclass)))
  (let ((class (apply (if (eq metaclass +the-standard-class+)
                          #'make-instance-standard-class
                          #'make-instance)
                      metaclass :name name
                      :direct-superclasses (canonicalize-direct-superclasses
                                            direct-superclasses)
                      all-keys)))
    (%set-find-class name class)
    class))

(defmethod ensure-class-using-class ((class built-in-class) name &rest all-keys
                                     &key &allow-other-keys)
  (declare (ignore all-keys))
  (error "The symbol ~S names a built-in class." name))

(defmethod ensure-class-using-class ((class forward-referenced-class) name
                                     &rest all-keys
                                     &key (metaclass +the-standard-class+)
                                     direct-superclasses &allow-other-keys)
  (setf all-keys (copy-list all-keys))  ; since we modify it
  (remf all-keys :metaclass)
  (unless (classp metaclass) (setf metaclass (find-class metaclass)))
  (apply #'change-class class metaclass all-keys)
  (apply #'reinitialize-instance class
         :name name
         :direct-superclasses (canonicalize-direct-superclasses
                               direct-superclasses)
         all-keys)
  class)

(defmethod ensure-class-using-class ((class class) name
                                     &rest all-keys
                                     &key (metaclass +the-standard-class+ metaclassp)
                                     direct-superclasses
                                     &allow-other-keys)
  (declare (ignore name))
  (setf all-keys (copy-list all-keys))  ; since we modify it
  (remf all-keys :metaclass)
  (unless (classp metaclass) (setf metaclass (find-class metaclass)))
  (when (and metaclassp (not (eq (class-of class) metaclass)))
    (error 'program-error
           "Trying to redefine class ~S with different metaclass."
           (class-name class)))
  (apply #'reinitialize-instance class
         :direct-superclasses (canonicalize-direct-superclasses direct-superclasses)
         all-keys)
  class)

(defmacro defclass (&whole form name direct-superclasses direct-slots &rest options)
  (unless (>= (length form) 3)
    (error 'program-error "Wrong number of arguments for DEFCLASS."))
  (check-declaration-type name)
  `(progn
     (sys::record-source-information-for-type ',name :class)
     (ensure-class ',name
                 :direct-superclasses
                 (canonicalize-direct-superclasses ',direct-superclasses)
                 :direct-slots
                 ,(canonicalize-direct-slots direct-slots)
                 ,@(canonicalize-defclass-options options))))


;;; AMOP pg. 180
(defgeneric direct-slot-definition-class (class &rest initargs))

(defmethod direct-slot-definition-class ((class class) &rest initargs)
  (declare (ignore initargs))
  +the-standard-direct-slot-definition-class+)

;;; AMOP pg. 181
(defgeneric effective-slot-definition-class (class &rest initargs))

(defmethod effective-slot-definition-class ((class class) &rest initargs)
  (declare (ignore initargs))
  +the-standard-effective-slot-definition-class+)

;;; AMOP pg. 224
(defgeneric reader-method-class (class direct-slot &rest initargs))

(defmethod reader-method-class ((class standard-class)
                                (direct-slot standard-direct-slot-definition)
                                &rest initargs)
  (declare (ignore initargs))
  +the-standard-reader-method-class+)

(defmethod reader-method-class ((class funcallable-standard-class)
                                (direct-slot standard-direct-slot-definition)
                                &rest initargs)
  (declare (ignore initargs))
  +the-standard-reader-method-class+)

;;; AMOP pg. 242
(defgeneric writer-method-class (class direct-slot &rest initargs))

(defmethod writer-method-class ((class standard-class)
                                (direct-slot standard-direct-slot-definition)
                                &rest initargs)
  (declare (ignore initargs))
  +the-standard-writer-method-class+)

(defmethod writer-method-class ((class funcallable-standard-class)
                                (direct-slot standard-direct-slot-definition)
                                &rest initargs)
  (declare (ignore initargs))
  +the-standard-writer-method-class+)

;;; Applicable methods

(atomic-defgeneric compute-applicable-methods (gf args)
  (:method ((gf standard-generic-function) args)
    (std-compute-applicable-methods gf args)))

(defgeneric compute-applicable-methods-using-classes (gf classes)
  (:method ((gf standard-generic-function) classes)
    (let ((methods '()))
      (dolist (method (generic-function-methods gf))
	(multiple-value-bind (applicable knownp)
	    (method-applicable-using-classes-p method classes)
	  (cond (applicable
		 (push method methods))
		((not knownp)
		 (return-from compute-applicable-methods-using-classes
		   (values nil nil))))))
      (values (sort-methods methods gf classes)
	      t))))


;;; Slot access
;;; 
;;; See AMOP pg. 156ff. for an overview.
;;; 
;;; AMOP specifies these generic functions to dispatch on slot objects
;;; (with the exception of slot-exists-p-using-class), although its
;;; sample implementation Closette dispatches on slot names.  We let
;;; slot-value and friends call their gf counterparts with the effective
;;; slot definition, but leave the definitions dispatching on slot name
;;; in place for user convenience.

;;; AMOP pg. 235
(defgeneric slot-value-using-class (class instance slot))

(defmethod slot-value-using-class ((class standard-class) instance (slot symbol))
  (std-slot-value instance slot))
(defmethod slot-value-using-class ((class standard-class) instance
                                   (slot standard-effective-slot-definition))
  (let* ((location (slot-definition-location slot))
         (value (if (consp location)
                    (cdr location)      ; :allocation :class
                    (standard-instance-access instance location))))
    (if (eq value +slot-unbound+)
        ;; fix SLOT-UNBOUND.5 from ansi test suite
        (nth-value 0 (slot-unbound class instance (slot-definition-name slot)))
        value)))

(defmethod slot-value-using-class ((class funcallable-standard-class)
                                   instance (slot symbol))
  (std-slot-value instance slot))
(defmethod slot-value-using-class ((class funcallable-standard-class) instance
                                   (slot standard-effective-slot-definition))
  (let* ((location (slot-definition-location slot))
         (value (if (consp location)
                    (cdr location)      ; :allocation :class
                    (funcallable-standard-instance-access instance location))))
    (if (eq value +slot-unbound+)
        ;; fix SLOT-UNBOUND.5 from ansi test suite
        (nth-value 0 (slot-unbound class instance (slot-definition-name slot)))
        value)))

(defmethod slot-value-using-class ((class structure-class) instance
                                   (slot symbol))
  (std-slot-value instance slot))
(defmethod slot-value-using-class ((class structure-class) instance
                                   (slot standard-effective-slot-definition))
  (std-slot-value instance (slot-definition-name slot)))

;;; AMOP pg. 231
(defgeneric (setf slot-value-using-class) (new-value class instance slot))

(defmethod (setf slot-value-using-class) (new-value
                                          (class standard-class)
                                          instance
                                          (slot symbol))
  (setf (std-slot-value instance slot) new-value))
(defmethod (setf slot-value-using-class) (new-value
                                          (class standard-class)
                                          instance
                                          (slot standard-effective-slot-definition))
  (let ((location (slot-definition-location slot)))
    (if (consp location)                ; :allocation :class
        (setf (cdr location) new-value)
        (setf (standard-instance-access instance location) new-value))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class funcallable-standard-class)
                                          instance
                                          (slot symbol))
  (setf (std-slot-value instance slot) new-value))
(defmethod (setf slot-value-using-class) (new-value
                                          (class funcallable-standard-class)
                                          instance
                                          (slot standard-effective-slot-definition))
  (let ((location (slot-definition-location slot)))
    (if (consp location)                ; :allocation :class
        (setf (cdr location) new-value)
        (setf (funcallable-standard-instance-access instance location)
              new-value))))

(defmethod (setf slot-value-using-class) (new-value
                                          (class structure-class)
                                          instance
                                          (slot symbol))
  (setf (std-slot-value instance slot) new-value))
(defmethod (setf slot-value-using-class) (new-value
                                          (class structure-class)
                                          instance
                                          (slot standard-effective-slot-definition))
  (setf (std-slot-value instance (slot-definition-name slot)) new-value))

;;; slot-exists-p-using-class is not specified by AMOP, and obviously
;;; cannot be specialized on the slot type.  Hence, its implementation
;;; differs from slot-(boundp|makunbound|value)-using-class
(defgeneric slot-exists-p-using-class (class instance slot-name))

(defmethod slot-exists-p-using-class (class instance slot-name)
  nil)

(defmethod slot-exists-p-using-class ((class standard-class) instance slot-name)
  (std-slot-exists-p instance slot-name))
(defmethod slot-exists-p-using-class ((class funcallable-standard-class) instance slot-name)
  (std-slot-exists-p instance slot-name))

(defmethod slot-exists-p-using-class ((class structure-class) instance slot-name)
  (dolist (dsd (class-slots class))
    (when (eq (sys::dsd-name dsd) slot-name)
      (return-from slot-exists-p-using-class t)))
  nil)


(defgeneric slot-boundp-using-class (class instance slot))
(defmethod slot-boundp-using-class ((class standard-class) instance (slot symbol))
  (std-slot-boundp instance slot))
(defmethod slot-boundp-using-class ((class standard-class) instance
                                    (slot standard-effective-slot-definition))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (not (eq (cdr location) +slot-unbound+)) ; :allocation :class
        (not (eq (standard-instance-access instance location) +slot-unbound+)))))

(defmethod slot-boundp-using-class ((class funcallable-standard-class) instance
                                    (slot symbol))
  (std-slot-boundp instance slot))
(defmethod slot-boundp-using-class ((class funcallable-standard-class) instance
                                    (slot standard-effective-slot-definition))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (not (eq (cdr location) +slot-unbound+)) ; :allocation :class
        (not (eq (funcallable-standard-instance-access instance location)
                 +slot-unbound+)))))

(defmethod slot-boundp-using-class ((class structure-class) instance slot)
  "Structure slots can't be unbound, so this method always returns T."
  (declare (ignore class instance slot))
  t)

(defgeneric slot-makunbound-using-class (class instance slot))
(defmethod slot-makunbound-using-class ((class standard-class)
                                        instance
                                        (slot symbol))
  (std-slot-makunbound instance slot))
(defmethod slot-makunbound-using-class ((class standard-class)
                                        instance
                                        (slot standard-effective-slot-definition))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (setf (cdr location) +slot-unbound+)
        (setf (standard-instance-access instance location) +slot-unbound+))))

(defmethod slot-makunbound-using-class ((class funcallable-standard-class)
                                        instance
                                        (slot symbol))
  (std-slot-makunbound instance slot))
(defmethod slot-makunbound-using-class ((class funcallable-standard-class)
                                        instance
                                        (slot symbol))
  (let ((location (slot-definition-location slot)))
    (if (consp location)
        (setf (cdr location) +slot-unbound+)
        (setf (funcallable-standard-instance-access instance location)
              +slot-unbound+))))

(defmethod slot-makunbound-using-class ((class structure-class)
                                        instance
                                        slot)
  (declare (ignore class instance slot))
  (error "Structure slots can't be unbound"))

(defgeneric slot-missing (class instance slot-name operation &optional new-value))

(defmethod slot-missing ((class t) instance slot-name operation &optional new-value)
  (declare (ignore new-value))
  (error "The slot ~S is missing from the class ~S." slot-name class))

(defgeneric slot-unbound (class instance slot-name))

(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :instance instance :name slot-name))

;;; Instance creation and initialization

;;; AMOP pg. 168ff.
(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys))

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (std-allocate-instance class))

(defmethod allocate-instance ((class funcallable-standard-class) &rest initargs)
  (declare (ignore initargs))
  (allocate-funcallable-instance class))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (%make-structure (class-name class)
                   (make-list (length (class-slots class))
                              :initial-element +slot-unbound+)))

(defmethod allocate-instance ((class built-in-class) &rest initargs)
  (declare (ignore initargs))
  (error "Cannot allocate instances of a built-in class: ~S" class))

(defmethod allocate-instance :before ((class class) &rest initargs)
  (declare (ignore initargs))
  (unless (class-finalized-p class)
    (finalize-inheritance class)))

;; "The set of valid initialization arguments for a class is the set of valid
;; initialization arguments that either fill slots or supply arguments to
;; methods, along with the predefined initialization argument :ALLOW-OTHER-KEYS."
;; 7.1.2

(defun calculate-allowable-initargs (gf-list args instance
                                             shared-initialize-param
                                             initargs)
  (let* ((methods
          (nconc
             (std-compute-applicable-methods #'shared-initialize
                                             (list* instance
                                                    shared-initialize-param
                                                    initargs))
             (mapcan #'(lambda (gf)
                         (if (std-generic-function-p gf)
                             (std-compute-applicable-methods gf args)
                             (compute-applicable-methods gf args)))
                     gf-list)))
         (method-keyword-args
          (reduce #'merge-initargs-sets
                  (mapcar #'method-lambda-list methods)
                  :key #'extract-lambda-list-keywords
                  :initial-value nil))
         (slots-initargs
          (mapappend #'slot-definition-initargs
                     (class-slots (class-of instance)))))
    (merge-initargs-sets
     (merge-initargs-sets slots-initargs method-keyword-args)
     '(:allow-other-keys))))  ;; allow-other-keys is always allowed

(defun check-initargs (gf-list args instance
                       shared-initialize-param initargs
                       cache call-site)
  "Checks the validity of `initargs' for the generic functions in `gf-list'
when called with `args' by calculating the applicable methods for each gf.
The applicable methods for SHARED-INITIALIZE based on `instance',
`shared-initialize-param' and `initargs' are added to the list of
applicable methods."
  (when (oddp (length initargs))
    (error 'program-error
           :format-control "Odd number of keyword arguments."))
  (unless (getf initargs :allow-other-keys)
    (multiple-value-bind (allowable-initargs present-p)
                         (when cache
                           (gethash (class-of instance) cache))
       (unless present-p
         (setf allowable-initargs
               (calculate-allowable-initargs gf-list args instance
                                             shared-initialize-param initargs))
         (when cache
           (setf (gethash (class-of instance) cache)
                 allowable-initargs)))
       (unless (eq t allowable-initargs)
         (do* ((tail initargs (cddr tail))
               (initarg (car tail) (car tail)))
              ((null tail))
              (unless (memq initarg allowable-initargs)
                (error 'program-error
                       :format-control "Invalid initarg ~S in call to ~S with arglist ~S."
                       :format-arguments (list initarg call-site args))))))))

(defun merge-initargs-sets (list1 list2)
  (cond
   ((eq list1 t)  t)
   ((eq list2 t)  t)
   (t             (union list1 list2))))

(defun extract-lambda-list-keywords (lambda-list)
  "Returns a list of keywords acceptable as keyword arguments,
or T when any keyword is acceptable due to presence of
&allow-other-keys."
  (when (member '&allow-other-keys lambda-list)
    (return-from extract-lambda-list-keywords t))
  (loop with keyword-args = (cdr (memq '&key lambda-list))
        for key in keyword-args
        when (eq key '&aux) do (loop-finish)
        when (eq key '&allow-other-keys) do (return t)
        when (listp key) do (setq key (car key))
        collect (if (symbolp key)
                    (make-keyword key)
                  (car key))))


(defgeneric make-instance (class &rest initargs &key &allow-other-keys))

(defmethod make-instance :before ((class class) &rest initargs)
  (when (oddp (length initargs))
    (error 'program-error :format-control "Odd number of keyword arguments."))
  (unless (class-finalized-p class)
    (finalize-inheritance class)))

(defun augment-initargs-with-defaults (class initargs)
  (let ((default-initargs '()))
    (dolist (initarg (class-default-initargs class))
      (let ((key (first initarg))
            (fn (third initarg)))
        (when (eq (getf initargs key +slot-unbound+) +slot-unbound+)
          (push key default-initargs)
          (push (funcall fn) default-initargs))))
    (append initargs (nreverse default-initargs))))

(defmethod make-instance ((class standard-class) &rest initargs)
  (setf initargs (augment-initargs-with-defaults class initargs))
  (let ((instance (std-allocate-instance class)))
    (check-initargs (list #'allocate-instance #'initialize-instance)
                    (list* instance initargs)
                    instance t initargs
                    *make-instance-initargs-cache* 'make-instance)
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod make-instance ((class funcallable-standard-class) &rest initargs)
  (setf initargs (augment-initargs-with-defaults class initargs))
  (let ((instance (allocate-funcallable-instance class)))
    (check-initargs (list #'allocate-instance #'initialize-instance)
                    (list* instance initargs)
                    instance t initargs
                    *make-instance-initargs-cache* 'make-instance)
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &rest initargs
                                          &key &allow-other-keys))

(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &rest initargs
                                            &key &allow-other-keys))

;; "The system-supplied primary method for REINITIALIZE-INSTANCE checks the
;; validity of initargs and signals an error if an initarg is supplied that is
;; not declared as valid. The method then calls the generic function SHARED-
;; INITIALIZE with the following arguments: the instance, nil (which means no
;; slots should be initialized according to their initforms), and the initargs
;; it received."
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (check-initargs (list #'reinitialize-instance) (list* instance initargs)
                  instance () initargs
                  *reinitialize-instance-initargs-cache* 'reinitialize-instance)
  (apply #'shared-initialize instance () initargs))

(defun std-shared-initialize (instance slot-names all-keys)
  (when (oddp (length all-keys))
    (error 'program-error :format-control "Odd number of keyword arguments."))
  ;; do a quick scan of the arguments list to see if it's a real
  ;; 'initialization argument list' (which is not the same as
  ;; checking initarg validity
  (do* ((tail all-keys (cddr tail))
        (initarg (car tail) (car tail)))
      ((null tail))
    (unless (symbolp initarg)
      (error 'program-error
             :format-control "Initarg ~S not a symbol."
             :format-arguments (list initarg))))
  (dolist (slot (class-slots (class-of instance)))
    (let ((slot-name (slot-definition-name slot)))
      (multiple-value-bind (init-key init-value foundp)
          (get-properties all-keys (slot-definition-initargs slot))
        (if foundp
            (setf (std-slot-value instance slot-name) init-value)
            (unless (std-slot-boundp instance slot-name)
              (let ((initfunction (slot-definition-initfunction slot)))
                (when (and initfunction (or (eq slot-names t)
                                            (memq slot-name slot-names)))
                  (setf (std-slot-value instance slot-name)
                        (funcall initfunction)))))))))
  instance)

(defgeneric shared-initialize (instance slot-names
                                        &rest initargs
                                        &key &allow-other-keys))

(defmethod shared-initialize ((instance standard-object) slot-names
                              &rest initargs)
  (std-shared-initialize instance slot-names initargs))

(defmethod shared-initialize ((slot slot-definition) slot-names
                              &rest args
                              &key name initargs initform initfunction
                              readers writers allocation
                              &allow-other-keys)
  ;;Keyword args are duplicated from init-slot-definition only to have
  ;;them checked.
  (declare (ignore slot-names)) ;;TODO?
  (declare (ignore name initargs initform initfunction readers writers allocation))
  ;;For built-in slots
  (apply #'init-slot-definition slot :allow-other-keys t args)
  ;;For user-defined slots
  (call-next-method))

;;; change-class

(defgeneric change-class (instance new-class &key &allow-other-keys))

(defmethod change-class ((old-instance standard-object) (new-class standard-class)
                         &rest initargs)
  (let ((old-slots (class-slots (class-of old-instance)))
        (new-slots (class-slots new-class))
        (new-instance (allocate-instance new-class)))
    ;; "The values of local slots specified by both the class CTO and the class
    ;; CFROM are retained. If such a local slot was unbound, it remains
    ;; unbound."
    (dolist (new-slot new-slots)
      (when (instance-slot-p new-slot)
        (let* ((slot-name (slot-definition-name new-slot))
               (old-slot (find slot-name old-slots :key 'slot-definition-name)))
          ;; "The values of slots specified as shared in the class CFROM and as
          ;; local in the class CTO are retained."
          (when (and old-slot (slot-boundp old-instance slot-name))
            (setf (slot-value new-instance slot-name)
                  (slot-value old-instance slot-name))))))
    (swap-slots old-instance new-instance)
    (rotatef (std-instance-layout new-instance)
             (std-instance-layout old-instance))
    (apply #'update-instance-for-different-class
           new-instance old-instance initargs)
    old-instance))

(defmethod change-class ((instance standard-object) (new-class symbol) &rest initargs)
  (apply #'change-class instance (find-class new-class) initargs))

(defgeneric update-instance-for-different-class (old new
                                                     &rest initargs
                                                     &key &allow-other-keys))

(defmethod update-instance-for-different-class
  ((old standard-object) (new standard-object) &rest initargs)
  (let ((added-slots
         (remove-if #'(lambda (slot-name)
                       (slot-exists-p old slot-name))
                    (mapcar 'slot-definition-name
                            (class-slots (class-of new))))))
    (check-initargs (list #'update-instance-for-different-class)
                    (list old new initargs)
                    new added-slots initargs
                    nil 'update-instance-for-different-class)
    (apply #'shared-initialize new added-slots initargs)))

;;; make-instances-obsolete

(defgeneric make-instances-obsolete (class))

(defmethod make-instances-obsolete ((class standard-class))
  (%make-instances-obsolete class))
(defmethod make-instances-obsolete ((class funcallable-standard-class))
  (%make-instances-obsolete class))
(defmethod make-instances-obsolete ((class symbol))
  (make-instances-obsolete (find-class class))
  class)

;;; update-instance-for-redefined-class

(defgeneric update-instance-for-redefined-class (instance
                                                 added-slots
                                                 discarded-slots
                                                 property-list
                                                 &rest initargs
                                                 &key
                                                 &allow-other-keys))

(defmethod update-instance-for-redefined-class ((instance standard-object)
						added-slots
						discarded-slots
						property-list
						&rest initargs)
  (check-initargs (list #'update-instance-for-redefined-class)
                  (list* instance added-slots discarded-slots
                         property-list initargs)
                  instance added-slots initargs
                  nil 'update-instance-for-redefined-class)
  (apply #'shared-initialize instance added-slots initargs))

;;;  Methods having to do with class metaobjects.

(defmethod initialize-instance :after ((class standard-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

(defmethod initialize-instance :after ((class funcallable-standard-class)
                                       &rest args)
  (apply #'std-after-initialization-for-classes class args))

(defmethod reinitialize-instance :before ((class standard-class)
                                          &rest all-keys
                                          &key direct-superclasses)
  (check-initargs (list #'allocate-instance
                        #'initialize-instance)
                  (list* class all-keys)
                  class t all-keys
                  nil 'reinitialize-instance)
  (dolist (superclass (set-difference (class-direct-superclasses class)
                                      direct-superclasses))
    (remove-direct-subclass superclass class))
  (dolist (superclass (set-difference direct-superclasses
                                      (class-direct-superclasses class)))
    (add-direct-subclass superclass class)))

(defmethod reinitialize-instance :before ((class funcallable-standard-class)
                                          &rest all-keys
                                          &key direct-superclasses)
  (check-initargs (list #'allocate-instance
                        #'initialize-instance)
                  (list* class all-keys)
                  class t all-keys
                  nil 'reinitialize-instance)
  (dolist (superclass (set-difference (class-direct-superclasses class)
                                      direct-superclasses))
    (remove-direct-subclass superclass class))
  (dolist (superclass (set-difference direct-superclasses
                                      (class-direct-superclasses class)))
    (add-direct-subclass superclass class)))

(defun std-after-reinitialization-for-classes (class
                                               &rest all-keys
                                               &key (direct-superclasses nil direct-superclasses-p)
                                               (direct-slots nil direct-slots-p)
                                               (direct-default-initargs nil direct-default-initargs-p)
                                               &allow-other-keys)
  (remhash class *make-instance-initargs-cache*)
  (remhash class *reinitialize-instance-initargs-cache*)
  (%make-instances-obsolete class)
  (setf (class-finalized-p class) nil)
  (when direct-superclasses-p
    (let* ((old-supers (class-direct-superclasses class))
           (new-supers (canonicalize-direct-superclass-list
                        class direct-superclasses)))
      (setf (class-direct-superclasses class) new-supers)
      (dolist (old-superclass (set-difference old-supers new-supers))
        (remove-direct-subclass old-superclass class))
      (dolist (new-superclass (set-difference new-supers old-supers))
        (add-direct-subclass new-superclass class))))
  (when direct-slots-p
    ;; FIXME: maybe remove old reader and writer methods?
    (let ((slots (mapcar #'(lambda (slot-properties)
                             (apply #'make-direct-slot-definition class slot-properties))
                         direct-slots)))
      (setf (class-direct-slots class) slots)
      (dolist (direct-slot slots)
        (dolist (reader (slot-definition-readers direct-slot))
          (add-reader-method class reader direct-slot))
        (dolist (writer (slot-definition-writers direct-slot))
          (add-writer-method class writer direct-slot)))))
  (when direct-default-initargs-p
    (setf (class-direct-default-initargs class) direct-default-initargs))
  (maybe-finalize-class-subtree class)
  (map-dependents class #'(lambda (dep) (update-dependent class dep all-keys))))

(defmethod reinitialize-instance :after ((class standard-class)
                                         &rest all-keys)
  (apply #'std-after-reinitialization-for-classes class all-keys))

(defmethod reinitialize-instance :after ((class funcallable-standard-class)
                                         &rest all-keys)
  (apply #'std-after-reinitialization-for-classes class all-keys))

(defmethod reinitialize-instance :before ((gf standard-generic-function)
                                          &key
                                            (lambda-list nil lambda-list-supplied-p)
                                          &allow-other-keys)
  (when lambda-list-supplied-p
    (unless (or (null (generic-function-methods gf))
                (lambda-lists-congruent-p lambda-list
                                          (generic-function-lambda-list gf)))
      (error "The lambda list ~S is incompatible with the existing methods of ~S."
             lambda-list gf))))

(defmethod reinitialize-instance :after ((gf standard-generic-function)
                                         &rest all-keys)
  (map-dependents gf #'(lambda (dep) (update-dependent gf dep all-keys))))

;;; Finalize inheritance

(atomic-defgeneric finalize-inheritance (class)
    (:method ((class standard-class))
       (std-finalize-inheritance class))
    (:method ((class funcallable-standard-class))
       (std-finalize-inheritance class)))

;;; Default initargs

;;; AMOP pg. 174
(atomic-defgeneric compute-default-initargs (class)
  (:method ((class standard-class))
    (std-compute-default-initargs class))
  (:method ((class funcallable-standard-class))
    (std-compute-default-initargs class)))

;;; Class precedence lists

(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class standard-class))
  (std-compute-class-precedence-list class))
(defmethod compute-class-precedence-list ((class funcallable-standard-class))
  (std-compute-class-precedence-list class))

;;; Slot inheritance

(defgeneric compute-slots (class))
(defmethod compute-slots ((class standard-class))
  (std-compute-slots class))
(defmethod compute-slots ((class funcallable-standard-class))
  (std-compute-slots class))

(defgeneric compute-effective-slot-definition (class name direct-slots))
(defmethod compute-effective-slot-definition
  ((class standard-class) name direct-slots)
  (std-compute-effective-slot-definition class name direct-slots))
(defmethod compute-effective-slot-definition
  ((class funcallable-standard-class) name direct-slots)
  (std-compute-effective-slot-definition class name direct-slots))

;;; Methods having to do with generic function invocation.

(defgeneric compute-discriminating-function (gf))
(defmethod compute-discriminating-function ((gf standard-generic-function))
  (std-compute-discriminating-function gf))

(defgeneric method-more-specific-p (gf method1 method2 required-classes))

(defmethod method-more-specific-p ((gf standard-generic-function)
                                   method1 method2 required-classes)
  (let ((method-indices
         (argument-precedence-order-indices
          (generic-function-argument-precedence-order gf)
          (getf (analyze-lambda-list (generic-function-lambda-list gf))
                ':required-args))))
    (std-method-more-specific-p method1 method2 required-classes method-indices)))

;;; AMOP pg. 176
(defgeneric compute-effective-method (gf method-combination methods))
(defmethod compute-effective-method ((gf standard-generic-function) method-combination methods)
  (std-compute-effective-method gf method-combination methods))

(defgeneric compute-applicable-methods (gf args))
(defmethod compute-applicable-methods ((gf standard-generic-function) args)
  (std-compute-applicable-methods gf args))

;;; AMOP pg. 207
(atomic-defgeneric make-method-lambda (generic-function method lambda-expression environment)
  (:method ((generic-function standard-generic-function)
            (method standard-method)
            lambda-expression environment)
    (declare (ignore environment))
    (values (compute-method-function lambda-expression) nil)))


;;; Slot definition accessors

(defmacro slot-definition-dispatch (slot-definition std-form generic-form)
  `(let (($cl (class-of ,slot-definition)))
     (case $cl
       ((+the-standard-slot-definition-class+
         +the-standard-direct-slot-definition-class+
         +the-standard-effective-slot-definition-class+)
        ,std-form)
       (t ,generic-form))))

(atomic-defgeneric slot-definition-allocation (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::allocation)
      (slot-value slot-definition 'sys::allocation))))

(atomic-defgeneric (setf slot-definition-allocation) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::allocation) value)
      (setf (slot-value slot-definition 'sys::allocation) value))))

(atomic-defgeneric slot-definition-initargs (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::initargs)
      (slot-value slot-definition 'sys::initargs))))

(atomic-defgeneric (setf slot-definition-initargs) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::initargs) value)
      (setf (slot-value slot-definition 'sys::initargs) value))))

(atomic-defgeneric slot-definition-initform (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::initform)
      (slot-value slot-definition 'sys::initform))))

(atomic-defgeneric (setf slot-definition-initform) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::initform) value)
      (setf (slot-value slot-definition 'sys::initform) value))))

(atomic-defgeneric slot-definition-initfunction (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::initfunction)
      (slot-value slot-definition 'sys::initfunction))))

(atomic-defgeneric (setf slot-definition-initfunction) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::initfunction) value)
      (setf (slot-value slot-definition 'sys::initfunction) value))))

(atomic-defgeneric slot-definition-name (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys:name)
      (slot-value slot-definition 'sys:name))))

(atomic-defgeneric (setf slot-definition-name) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys:name) value)
      (setf (slot-value slot-definition 'sys:name) value))))

(atomic-defgeneric slot-definition-readers (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::readers)
      (slot-value slot-definition 'sys::readers))))

(atomic-defgeneric (setf slot-definition-readers) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::readers) value)
      (setf (slot-value slot-definition 'sys::readers) value))))

(atomic-defgeneric slot-definition-writers (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::writers)
      (slot-value slot-definition 'sys::writers))))

(atomic-defgeneric (setf slot-definition-writers) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::writers) value)
      (setf (slot-value slot-definition 'sys::writers) value))))

(atomic-defgeneric slot-definition-allocation-class (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::allocation-class)
      (slot-value slot-definition 'sys::allocation-class))))

(atomic-defgeneric (setf slot-definition-allocation-class)
                       (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::allocation-class) value)
      (setf (slot-value slot-definition 'sys::allocation-class) value))))

(atomic-defgeneric slot-definition-location (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::location)
      (slot-value slot-definition 'sys::location))))

(atomic-defgeneric (setf slot-definition-location) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::location) value)
      (setf (slot-value slot-definition 'sys::location) value))))

(atomic-defgeneric slot-definition-type (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys::%type)
      (slot-value slot-definition 'sys::%type))))

(atomic-defgeneric (setf slot-definition-type) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys::%type) value)
      (setf (slot-value slot-definition 'sys::%type) value))))

(atomic-defgeneric slot-definition-documentation (slot-definition)
  (:method ((slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (std-slot-value slot-definition 'sys:%documentation)
      (slot-value slot-definition 'sys:%documentation))))

(atomic-defgeneric (setf slot-definition-documentation) (value slot-definition)
  (:method (value (slot-definition slot-definition))
    (slot-definition-dispatch slot-definition
      (setf (std-slot-value slot-definition 'sys:%documentation) value)
      (setf (slot-value slot-definition 'sys:%documentation) value))))


;;; Conditions.

(defmacro define-condition (name (&rest parent-types) (&rest slot-specs) &body options)
  (let ((parent-types (or parent-types '(condition)))
        (report nil))
    (dolist (option options)
      (when (eq (car option) :report)
        (setf report (cadr option))
	(setf options (delete option options :test #'equal))
        (return)))
    (typecase report
      (null
       `(progn
	  (sys::record-source-information-for-type  ',name :condition)
          (defclass ,name ,parent-types ,slot-specs ,@options)
          ',name))
      (string
       `(progn
	  (sys::record-source-information-for-type  ',name :condition)
          (defclass ,name ,parent-types ,slot-specs ,@options)
          (defmethod print-object ((condition ,name) stream)
            (if *print-escape*
                (call-next-method)
                (progn (write-string ,report stream) condition)))
          ',name))
      (t
       `(progn
	  (sys::record-source-information-for-type  ',name :condition)
          (defclass ,name ,parent-types ,slot-specs ,@options)
          (defmethod print-object ((condition ,name) stream)
            (if *print-escape*
                (call-next-method)
                (funcall #',report condition stream)))
          ',name)))))

(defun make-condition (type &rest initargs)
  (or (%make-condition type initargs)
      (let ((class (if (symbolp type) (find-class type) type)))
        (apply #'make-instance class initargs))))

;; Adapted from SBCL.
;; Originally defined in signal.lisp. Redefined here now that we have MAKE-CONDITION.
(defun coerce-to-condition (datum arguments default-type fun-name)
  (cond ((typep datum 'condition)
         (when arguments
           (error 'simple-type-error
                  :datum arguments
                  :expected-type 'null
                  :format-control "You may not supply additional arguments when giving ~S to ~S."
                  :format-arguments (list datum fun-name)))
         datum)
        ((symbolp datum)
         (apply #'make-condition datum arguments))
        ((or (stringp datum) (functionp datum))
         (make-condition default-type
                         :format-control datum
                         :format-arguments arguments))
        (t
         (error 'simple-type-error
                :datum datum
                :expected-type '(or symbol string)
                :format-control "Bad argument to ~S: ~S."
                :format-arguments (list fun-name datum)))))

(defgeneric make-load-form (object &optional environment))

(defmethod make-load-form ((object t) &optional environment)
  (declare (ignore environment))
  (apply #'no-applicable-method #'make-load-form (list object)))

(defmethod make-load-form ((class class) &optional environment)
  (declare (ignore environment))
  (let ((name (class-name class)))
    (unless (and name (eq (find-class name nil) class))
      (error 'simple-type-error
             :format-control "Can't use anonymous or undefined class as a constant: ~S."
             :format-arguments (list class)))
    `(find-class ',name)))

(defun invalid-method-error (method format-control &rest args)
  (let ((message (apply #'format nil format-control args)))
    (error "Invalid method error for ~S:~%    ~A" method message)))

(defun method-combination-error (format-control &rest args)
  (let ((message (apply #'format nil format-control args)))
    (error "Method combination error in CLOS dispatch:~%    ~A" message)))


(atomic-defgeneric no-applicable-method (generic-function &rest args)
  (:method (generic-function &rest args)
      (error "There is no applicable method for the generic function ~S ~
              when called with arguments ~S."
             generic-function
             args)))


;;; FIXME (rudi 2012-01-28): this can be a function, it only needs to
;;; use standard accessor functions
(defgeneric find-method (generic-function
                         qualifiers
                         specializers
                         &optional errorp))

(defmethod find-method ((generic-function standard-generic-function)
                        qualifiers specializers &optional (errorp t))
  (%find-method generic-function qualifiers specializers errorp))

(defgeneric find-method ((generic-function symbol)
                         qualifiers specializers &optional (errorp t))
  (find-method (find-generic-function generic-function errorp)
               qualifiers specializers errorp))

;;; AMOP pg. 167
(defgeneric add-method (generic-function method))

(defmethod add-method :before ((generic-function generic-function)
                               (method method))
  (when (and (method-generic-function method)
             (not (eql generic-function (method-generic-function method))))
    (error 'simple-error
           :format-control "~S is already a method of ~S, cannot add to ~S."
           :format-arguments (list method (method-generic-function method)
                                   generic-function)))
  (check-method-lambda-list (generic-function-name generic-function)
                            (method-lambda-list method)
                            (generic-function-lambda-list generic-function)))

(defmethod add-method ((generic-function standard-generic-function)
                       (method standard-method))
  (std-add-method generic-function method))

(defmethod add-method :after ((generic-function generic-function)
                              (method method))
  (map-dependents generic-function
                  #'(lambda (dep) (update-dependent generic-function dep
                                                    'add-method method))))

(defgeneric remove-method (generic-function method))

(defmethod remove-method ((generic-function standard-generic-function)
                          (method standard-method))
  (std-remove-method generic-function method))

(defmethod remove-method :after ((generic-function generic-function)
                                 (method method))
  (map-dependents generic-function
                  #'(lambda (dep) (update-dependent generic-function dep
                                                    'remove-method method))))

;; See describe.lisp.
(defgeneric describe-object (object stream))

;; FIXME
(defgeneric no-next-method (generic-function method &rest args))

(atomic-defgeneric function-keywords (method)
  (:method ((method standard-method))
    (std-function-keywords method)))

(setf *gf-initialize-instance* (symbol-function 'initialize-instance))
(setf *gf-allocate-instance* (symbol-function 'allocate-instance))
(setf *gf-shared-initialize* (symbol-function 'shared-initialize))
(setf *gf-reinitialize-instance* (symbol-function 'reinitialize-instance))
(setf *clos-booting* nil)

(atomic-defgeneric class-prototype (class)
  (:method ((class standard-class))
    (allocate-instance class))
  (:method ((class funcallable-standard-class))
    (allocate-instance class))
  (:method ((class structure-class))
    (allocate-instance class))
  (:method :before (class)
    (unless (class-finalized-p class)
      (error "~@<~S is not finalized.~:@>" class))))





(defmethod shared-initialize :before ((instance generic-function)
                                      slot-names
                                      &key lambda-list argument-precedence-order
                                      &allow-other-keys)
  (check-argument-precedence-order lambda-list argument-precedence-order))

(defmethod shared-initialize :after ((instance standard-generic-function)
                                     slot-names
                                     &key lambda-list argument-precedence-order
                                       (method-combination '(standard))
                                     &allow-other-keys)
  (let* ((plist (analyze-lambda-list lambda-list))
         (required-args (getf plist ':required-args)))
    (setf (std-slot-value instance 'sys::required-args) required-args)
    (setf (std-slot-value instance 'sys::optional-args)
          (getf plist :optional-args)) 
    (setf (std-slot-value instance 'sys::argument-precedence-order)
          (or argument-precedence-order required-args)))
  (unless (typep (generic-function-method-combination instance)
                 'method-combination)
    ;; this fixes (make-instance 'standard-generic-function) -- the
    ;; constructor of StandardGenericFunction sets this slot to '(standard)
    (setf (std-slot-value instance 'sys::%method-combination)
          (find-method-combination
           instance (car method-combination) (cdr method-combination))))
  (finalize-standard-generic-function instance))

;;; Readers for generic function metaobjects
;;; AMOP pg. 216ff.
(atomic-defgeneric generic-function-argument-precedence-order (generic-function)
  (:method ((generic-function standard-generic-function))
    (std-slot-value generic-function 'sys::argument-precedence-order)))

(atomic-defgeneric generic-function-declarations (generic-function)
  (:method ((generic-function standard-generic-function))
    (std-slot-value generic-function 'sys::declarations)))

(atomic-defgeneric generic-function-lambda-list (generic-function)
  (:method ((generic-function standard-generic-function))
    (std-slot-value generic-function 'sys::lambda-list)))

(atomic-defgeneric generic-function-method-class (generic-function)
  (:method ((generic-function standard-generic-function))
    (std-slot-value generic-function 'sys::method-class)))

(atomic-defgeneric generic-function-method-combination (generic-function)
  (:method ((generic-function standard-generic-function))
    (std-slot-value generic-function 'sys::%method-combination)))

(atomic-defgeneric generic-function-methods (generic-function)
  (:method ((generic-function standard-generic-function))
    (std-slot-value generic-function 'sys::methods)))

(atomic-defgeneric generic-function-name (generic-function)
  (:method ((generic-function standard-generic-function))
    (slot-value generic-function 'sys::name)))

(atomic-defgeneric generic-function-required-arguments (generic-function)
  (:method ((generic-function standard-generic-function))
    (std-slot-value generic-function 'sys::required-args)))

(atomic-defgeneric generic-function-optional-arguments (generic-function)
  (:method ((generic-function standard-generic-function))
    (std-slot-value generic-function 'sys::optional-args)))

;;; AMOP pg. 231
(defgeneric (setf generic-function-name) (new-value gf)
  (:method (new-value (gf generic-function))
    (reinitialize-instance gf :name new-value)))

;;; Readers for Method Metaobjects
;;; AMOP pg. 218ff.

(atomic-defgeneric method-function (method)
  (:method ((method standard-method))
    (std-method-function method)))

(atomic-defgeneric method-generic-function (method)
  (:method ((method standard-method))
    (std-method-generic-function method)))

(atomic-defgeneric method-lambda-list (method)
  (:method ((method standard-method))
    (std-slot-value method 'sys::lambda-list)))

(atomic-defgeneric method-specializers (method)
  (:method ((method standard-method))
    (std-method-specializers method)))

(atomic-defgeneric method-qualifiers (method)
  (:method ((method standard-method))
    (std-method-qualifiers method)))

(atomic-defgeneric accessor-method-slot-definition (method)
  (:method ((method standard-accessor-method))
    (std-accessor-method-slot-definition method)))


;;; find-method-combination

;;; AMOP pg. 191
(atomic-defgeneric find-method-combination (gf name options)
  (:method (gf (name symbol) options)
    (std-find-method-combination gf name options)))

;;; specializer-direct-method and friends.

;;; AMOP pg. 237
(defgeneric specializer-direct-generic-functions (specializer))

(defmethod specializer-direct-generic-functions ((specializer class))
  (delete-duplicates (mapcar #'method-generic-function
                             (class-direct-methods specializer))))

(defmethod specializer-direct-generic-functions ((specializer eql-specializer))
  (delete-duplicates (mapcar #'method-generic-function
                             (slot-value specializer 'direct-methods))))

;;; AMOP pg. 238
(defgeneric specializer-direct-methods (specializer))

(defmethod specializer-direct-methods ((specializer class))
  (class-direct-methods specializer))

(defmethod specializer-direct-methods ((specializer eql-specializer))
  (slot-value specializer 'direct-methods))

;;; AMOP pg. 165
(atomic-defgeneric add-direct-method (specializer method)
  (:method ((specializer class) (method method))
    (pushnew method (class-direct-methods specializer)))
  (:method ((specializer eql-specializer) (method method))
    (pushnew method (slot-value specializer 'direct-methods))))


;;; AMOP pg. 227
(atomic-defgeneric remove-direct-method (specializer method)
  (:method ((specializer class) (method method))
    (setf (class-direct-methods specializer)
          (remove method (class-direct-methods specializer))))
  (:method ((specializer eql-specializer) (method method))
    (setf (slot-value specializer 'direct-methods)
          (remove method (slot-value specializer 'direct-methods)))))

;;; The Dependent Maintenance Protocol (AMOP pg. 160ff.)

(defvar *dependents* (make-hash-table :test 'eq :weakness :key))

;;; AMOP pg. 164
(defgeneric add-dependent (metaobject dependent))
(defmethod add-dependent ((metaobject standard-class) dependent)
  (pushnew dependent (gethash metaobject *dependents* nil)))
(defmethod add-dependent ((metaobject funcallable-standard-class) dependent)
  (pushnew dependent (gethash metaobject *dependents* nil)))
(defmethod add-dependent ((metaobject standard-generic-function) dependent)
  (pushnew dependent (gethash metaobject *dependents* nil)))

;;; AMOP pg. 225
(defgeneric remove-dependent (metaobject dependent))
(defmethod remove-dependent ((metaobject standard-class) dependent)
  (setf (gethash metaobject *dependents*)
        (delete dependent (gethash metaobject *dependents* nil) :test #'eq)))
(defmethod remove-dependent ((metaobject funcallable-standard-class) dependent)
  (setf (gethash metaobject *dependents*)
        (delete dependent (gethash metaobject *dependents* nil) :test #'eq)))
(defmethod remove-dependent ((metaobject standard-generic-function) dependent)
  (setf (gethash metaobject *dependents*)
        (delete dependent (gethash metaobject *dependents* nil) :test #'eq)))

;;; AMOP pg. 210
(atomic-defgeneric map-dependents (metaobject function)
  (:method ((metaobject standard-class) function)
    (dolist (dependent (gethash metaobject *dependents* nil))
      (funcall function dependent)))
  (:method ((metaobject funcallable-standard-class) function)
    (dolist (dependent (gethash metaobject *dependents* nil))
      (funcall function dependent)))
  (:method ((metaobject standard-generic-function) function)
    (dolist (dependent (gethash metaobject *dependents* nil))
      (funcall function dependent))))

;;; AMOP pg. 239
(defgeneric update-dependent (metaobject dependent &rest initargs))


;;; ensure-generic-function(-using-class), AMOP pg. 185ff.
(defgeneric ensure-generic-function-using-class (generic-function function-name
                                                 &key
                                                   argument-precedence-order
                                                   declarations documentation
                                                   generic-function-class
                                                   lambda-list method-class
                                                   method-combination
                                                   name
                                                 &allow-other-keys))

(defmethod ensure-generic-function-using-class
    ((generic-function generic-function)
     function-name
     &rest all-keys
     &key (generic-function-class (class-of generic-function))
     (method-class (generic-function-method-class generic-function))
     (method-combination (generic-function-method-combination generic-function))
     &allow-other-keys)
  (setf all-keys (copy-list all-keys))  ; since we modify it
  (remf all-keys :generic-function-class)
  (unless (classp generic-function-class)
    (setf generic-function-class (find-class generic-function-class)))
  (unless (classp method-class) (setf method-class (find-class method-class)))
  (unless (eq generic-function-class (class-of generic-function))
    (error "The class ~S is incompatible with the existing class (~S) of ~S."
           generic-function-class (class-of generic-function) generic-function))
  ;; We used to check for changes in method class here, but CLHS says:
  ;; "If function-name specifies a generic function that has a different
  ;; value for the :method-class argument, the value is changed, but any
  ;; existing methods are not changed."
  (unless (typep method-combination 'method-combination)
    (setf method-combination
          (find-method-combination generic-function
                                   (car method-combination)
                                   (cdr method-combination))))
  (apply #'reinitialize-instance generic-function
         :method-combination method-combination
         :method-class method-class
         all-keys)
  generic-function)

(defmethod ensure-generic-function-using-class ((generic-function null)
                                                function-name
                                                &rest all-keys
                                                &key (generic-function-class +the-standard-generic-function-class+)
                                                &allow-other-keys)
  (setf all-keys (copy-list all-keys))  ; since we modify it
  (remf all-keys :generic-function-class)
  (unless (classp generic-function-class)
    (setf generic-function-class (find-class generic-function-class)))
  (when (and (null *clos-booting*) (fboundp function-name))
    (if (or (autoloadp function-name)
            (and (consp function-name)
                 (eq 'setf (first function-name))
                 (autoload-ref-p (second function-name))))
        (fmakunbound function-name)
        (error 'program-error
               :format-control "~A already names an ordinary function, macro, or special operator."
               :format-arguments (list function-name))))
  (apply (if (eq generic-function-class +the-standard-generic-function-class+)
             #'make-instance-standard-generic-function
             #'make-instance)
         generic-function-class :name function-name all-keys))

(defun ensure-generic-function (function-name &rest all-keys
                                &key
                                  lambda-list generic-function-class
                                  method-class
                                  method-combination
                                  argument-precedence-order
                                  declarations
                                  documentation
                                &allow-other-keys)
  (declare (ignore lambda-list generic-function-class method-class
                   method-combination argument-precedence-order declarations
                   documentation))
  (apply #'ensure-generic-function-using-class
         (find-generic-function function-name nil)
         function-name all-keys))

;;; SLIME compatibility functions.

(defun %method-generic-function (method)
  (method-generic-function method))

(defun %method-function (method)
  (method-function method))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (require "MOP"))

(provide "CLOS")

