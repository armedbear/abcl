;;; clos.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves
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

(export '(class-precedence-list class-slots))
(defconstant +the-standard-class+ (find-class 'standard-class))
(defconstant +the-standard-object-class+ (find-class 'standard-object))
(defconstant +the-standard-method-class+ (find-class 'standard-method))
(defconstant +the-standard-reader-method-class+
  (find-class 'standard-reader-method))
(defconstant +the-standard-generic-function-class+
  (find-class 'standard-generic-function))
(defconstant +the-T-class+ (find-class 'T))
(defconstant +the-slot-definition-class+ (find-class 'slot-definition))
(defconstant +the-direct-slot-definition-class+ (find-class 'direct-slot-definition))
(defconstant +the-effective-slot-definition-class+ (find-class 'effective-slot-definition))

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

(defun no-applicable-method (generic-function &rest args)
  (error "There is no applicable method for the generic function ~S when called with arguments ~S."
         generic-function
         args))



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

(defsetf std-instance-layout %set-std-instance-layout)
(defsetf standard-instance-access %set-standard-instance-access)

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
            (other-options ()))
        (do ((olist (cdr spec) (cddr olist)))
            ((null olist))
          (case (car olist)
            (:initform
             (when initform
               (error 'program-error
                      "duplicate slot option :INITFORM for slot named ~S"
                      name))
             (setq initfunction
                   `(function (lambda () ,(cadr olist))))
             (setq initform `',(cadr olist)))
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
             (setf type (cadr olist))) ;; FIXME type is ignored
            (:documentation
             (when documentation
               (error 'program-error
                      "duplicate slot option :DOCUMENTATION for slot named ~S"
                      name))
             (setf documentation (cadr olist))) ;; FIXME documentation is ignored
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
             (error 'program-error
                    "invalid initialization argument ~S for slot named ~S"
                    (car olist) name))))
        `(list
          :name ',name
          ,@(when initfunction
              `(:initform ,initform
                          :initfunction ,initfunction))
          ,@(when initargs `(:initargs ',initargs))
          ,@(when readers `(:readers ',readers))
          ,@(when writers `(:writers ',writers))
          ,@other-options))))

(defun maybe-note-name-defined (name)
  (when (fboundp 'note-name-defined)
    (note-name-defined name)))

(defun canonicalize-direct-superclasses (direct-superclasses)
  (let ((classes '()))
    (dolist (class-specifier direct-superclasses)
      (if (classp class-specifier)
          (push class-specifier classes)
          (let ((class (find-class class-specifier nil)))
            (unless class
              (setf class (make-forward-referenced-class class-specifier)))
            (push class classes))))
    (nreverse classes)))

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
      `(list ,@(mapappend
                #'(lambda (x) x)
                (mapplist
                 #'(lambda (key value)
                    `(',key ,(make-initfunction value)))
                 (cdr option))))))
    ((:documentation :report)
     (list (car option) `',(cadr option)))
    (t (list (car option) `(quote ,(cdr option))))))
;     (error 'program-error
;            :format-control "invalid DEFCLASS option ~S"
;            :format-arguments (list (car option))))))

(defun make-initfunction (initform)
  `(function (lambda () ,initform)))

(defun slot-definition-allocation (slot-definition)
  (%slot-definition-allocation slot-definition))

(defun slot-definition-initargs (slot-definition)
  (%slot-definition-initargs slot-definition))

(defun slot-definition-initform (slot-definition)
  (%slot-definition-initform slot-definition))

(defun slot-definition-initfunction (slot-definition)
  (%slot-definition-initfunction slot-definition))

(defun slot-definition-name (slot-definition)
  (%slot-definition-name slot-definition))

(defun init-slot-definition (slot &key name
			     (initargs ())
			     (initform nil)
			     (initfunction nil)
			     (readers ())
			     (writers ())
			     (allocation :instance)
			     (allocation-class nil)
				    &allow-other-keys)
  (set-slot-definition-name slot name)
  (set-slot-definition-initargs slot initargs)
  (set-slot-definition-initform slot initform)
  (set-slot-definition-initfunction slot initfunction)
  (set-slot-definition-readers slot readers)
  (set-slot-definition-writers slot writers)
  (set-slot-definition-allocation slot allocation)
  (set-slot-definition-allocation-class slot allocation-class)
  slot)

(defun make-direct-slot-definition (class &rest args)
  (let ((slot-class (direct-slot-definition-class class)))
    (if (eq slot-class +the-direct-slot-definition-class+)
	(let ((slot (make-slot-definition +the-direct-slot-definition-class+)))
	  (apply #'init-slot-definition slot :allocation-class class args)
	  slot)
	(progn
	  (let ((slot (apply #'make-instance slot-class :allocation-class class
			     args)))
	    slot)))))

(defun make-effective-slot-definition (class &rest args)
  (let ((slot-class (effective-slot-definition-class class)))
    (if (eq slot-class +the-effective-slot-definition-class+)
	(let ((slot (make-slot-definition +the-effective-slot-definition-class+)))
	  (apply #'init-slot-definition slot args)
	  slot)
	(progn
	  (let ((slot (apply #'make-instance slot-class args)))
	    slot)))))

;;; finalize-inheritance

(defun std-compute-class-default-initargs (class)
  (mapcan #'(lambda (c)
              (copy-list
               (class-direct-default-initargs c)))
          (class-precedence-list class)))

(defun std-finalize-inheritance (class)
  (setf (class-precedence-list class)
   (funcall (if (eq (class-of class) +the-standard-class+)
                #'std-compute-class-precedence-list
                #'compute-class-precedence-list)
            class))
  (dolist (class (class-precedence-list class))
    (when (typep class 'forward-referenced-class)
      (return-from std-finalize-inheritance)))
  (setf (class-slots class)
                   (funcall (if (eq (class-of class) +the-standard-class+)
                                #'std-compute-slots
                     #'compute-slots) class))
  (let ((old-layout (class-layout class))
        (length 0)
        (instance-slots '())
        (shared-slots '()))
    (dolist (slot (class-slots class))
      (case (slot-definition-allocation slot)
        (:instance
         (set-slot-definition-location slot length)
         (incf length)
         (push (slot-definition-name slot) instance-slots))
        (:class
         (unless (%slot-definition-location slot)
           (let ((allocation-class (%slot-definition-allocation-class slot)))
             (set-slot-definition-location slot
                                           (if (eq allocation-class class)
                                               (cons (slot-definition-name slot) +slot-unbound+)
                                               (slot-location allocation-class (slot-definition-name slot))))))
         (push (%slot-definition-location slot) shared-slots))))
    (when old-layout
      ;; Redefined class: initialize added shared slots.
      (dolist (location shared-slots)
        (let* ((slot-name (car location))
               (old-location (layout-slot-location old-layout slot-name)))
          (unless old-location
            (let* ((slot-definition (find slot-name (class-slots class) :key #'slot-definition-name))
                   (initfunction (slot-definition-initfunction slot-definition)))
              (when initfunction
                (setf (cdr location) (funcall initfunction))))))))
    (setf (class-layout class)
          (make-layout class (nreverse instance-slots) (nreverse shared-slots))))
  (setf (class-default-initargs class)
        (std-compute-class-default-initargs class))
  (setf (class-finalized-p class) t))

;;; Class precedence lists

(defun std-compute-class-precedence-list (class)
  (let ((classes-to-order (collect-superclasses* class)))
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
  (let* ((all-slots (mapappend #'class-direct-slots
                               (class-precedence-list class)))
         (all-names (remove-duplicates
                     (mapcar #'slot-definition-name all-slots))))
    (mapcar #'(lambda (name)
               (funcall
                (if (eq (class-of class) +the-standard-class+)
                    #'std-compute-effective-slot-definition
                    #'compute-effective-slot-definition)
                class
                (remove name all-slots
                        :key #'slot-definition-name
                        :test-not #'eq)))
            all-names)))

(defun std-compute-effective-slot-definition (class direct-slots)
  (let ((initer (find-if-not #'null direct-slots
                             :key #'slot-definition-initfunction)))
    (make-effective-slot-definition
     class
     :name (slot-definition-name (car direct-slots))
     :initform (if initer
                   (slot-definition-initform initer)
                   nil)
     :initfunction (if initer
                       (slot-definition-initfunction initer)
                       nil)
     :initargs (remove-duplicates
                (mapappend #'slot-definition-initargs
                           direct-slots))
     :allocation (slot-definition-allocation (car direct-slots))
     :allocation-class (%slot-definition-allocation-class (car direct-slots)))))

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
        (%slot-definition-location slot)
        nil)))

(defun instance-slot-location (instance slot-name)
  (let ((layout (std-instance-layout instance)))
    (and layout (layout-slot-location layout slot-name))))

(defun slot-value (object slot-name)
  (if (eq (class-of (class-of object)) +the-standard-class+)
      (std-slot-value object slot-name)
      (slot-value-using-class (class-of object) object slot-name)))

(defsetf std-slot-value set-std-slot-value)

(defun %set-slot-value (object slot-name new-value)
  (if (eq (class-of (class-of object)) +the-standard-class+)
      (setf (std-slot-value object slot-name) new-value)
      (set-slot-value-using-class new-value (class-of object)
                                  object slot-name)))

(defsetf slot-value %set-slot-value)

(defun slot-boundp (object slot-name)
  (if (eq (class-of (class-of object)) +the-standard-class+)
      (std-slot-boundp object slot-name)
      (slot-boundp-using-class (class-of object) object slot-name)))

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
  (if (eq (class-of (class-of object)) +the-standard-class+)
      (std-slot-makunbound object slot-name)
      (slot-makunbound-using-class (class-of object) object slot-name)))

(defun std-slot-exists-p (instance slot-name)
  (not (null (find slot-name (class-slots (class-of instance))
                   :key #'slot-definition-name))))

(defun slot-exists-p (object slot-name)
  (if (eq (class-of (class-of object)) +the-standard-class+)
      (std-slot-exists-p object slot-name)
      (slot-exists-p-using-class (class-of object) object slot-name)))

(defun instance-slot-p (slot)
  (eq (slot-definition-allocation slot) :instance))

(defun make-instance-standard-class (metaclass
				     &rest initargs
                                     &key name direct-superclasses direct-slots
                                     direct-default-initargs
                                     documentation)
  (declare (ignore metaclass))
  (let ((class (std-allocate-instance +the-standard-class+)))
    (check-initargs class t initargs)
    (%set-class-name name class)
    (%set-class-layout nil class)
    (%set-class-direct-subclasses ()  class)
    (%set-class-direct-methods ()  class)
    (%set-class-documentation class documentation)
    (std-after-initialization-for-classes class
                                          :direct-superclasses direct-superclasses
                                          :direct-slots direct-slots
                                          :direct-default-initargs direct-default-initargs)
    class))

;(defun convert-to-direct-slot-definition (class canonicalized-slot)
;  (apply #'make-instance
;         (apply #'direct-slot-definition-class
;                class canonicalized-slot)
;         canonicalized-slot))

(defun std-after-initialization-for-classes (class
                                             &key direct-superclasses direct-slots
                                             direct-default-initargs
                                             &allow-other-keys)
  (let ((supers (or direct-superclasses
                    (list +the-standard-object-class+))))
    (setf (class-direct-superclasses class) supers)
    (dolist (superclass supers)
      (pushnew class (class-direct-subclasses superclass))))
  (let ((slots (mapcar #'(lambda (slot-properties)
                          (apply #'make-direct-slot-definition class slot-properties))
                       direct-slots)))
    (setf (class-direct-slots class) slots)
    (dolist (direct-slot slots)
      (dolist (reader (%slot-definition-readers direct-slot))
        (add-reader-method class reader (slot-definition-name direct-slot)))
      (dolist (writer (%slot-definition-writers direct-slot))
        (add-writer-method class writer (slot-definition-name direct-slot)))))
  (setf (class-direct-default-initargs class) direct-default-initargs)
  (funcall (if (eq (class-of class) +the-standard-class+)
               #'std-finalize-inheritance
               #'finalize-inheritance)
           class)
  (values))

(defun canonical-slot-name (canonical-slot)
  (getf canonical-slot :name))

(defvar *extensible-built-in-classes*
  (list (find-class 'sequence)
        (find-class 'java:java-object)))

(defun ensure-class (name &rest all-keys &key metaclass &allow-other-keys)
  ;; Check for duplicate slots.
  (remf all-keys :metaclass)
  (let ((slots (getf all-keys :direct-slots)))
    (dolist (s1 slots)
      (let ((name1 (canonical-slot-name s1)))
        (dolist (s2 (cdr (memq s1 slots)))
          (when (eq name1 (canonical-slot-name s2))
            (error 'program-error "Duplicate slot ~S" name1))))))
  ;; Check for duplicate argument names in :DEFAULT-INITARGS.
  (let ((names ()))
    (do* ((initargs (getf all-keys :direct-default-initargs) (cddr initargs))
          (name (car initargs) (car initargs)))
         ((null initargs))
      (push name names))
    (do* ((names names (cdr names))
          (name (car names) (car names)))
         ((null names))
      (when (memq name (cdr names))
        (error 'program-error
               :format-control "Duplicate initialization argument name ~S in :DEFAULT-INITARGS."
               :format-arguments (list name)))))
  (let ((direct-superclasses (getf all-keys :direct-superclasses)))
    (dolist (class direct-superclasses)
      (when (and (typep class 'built-in-class)
		 (not (member class *extensible-built-in-classes*)))
        (error "Attempt to define a subclass of a built-in-class: ~S" class))))
  (let ((old-class (find-class name nil)))
    (cond ((and old-class (eq name (class-name old-class)))
           (cond ((typep old-class 'built-in-class)
                  (error "The symbol ~S names a built-in class." name))
                 ((typep old-class 'forward-referenced-class)
                  (let ((new-class (apply #'make-instance-standard-class
                                          +the-standard-class+
                                          :name name all-keys)))
                    (%set-find-class name new-class)
                    (dolist (subclass (class-direct-subclasses old-class))
                      (setf (class-direct-superclasses subclass)
                            (substitute new-class old-class
                                        (class-direct-superclasses subclass))))
                    new-class))
                 (t
                  ;; We're redefining the class.
                  (%make-instances-obsolete old-class)
		  (check-initargs old-class t all-keys)
                  (apply #'std-after-initialization-for-classes old-class all-keys)
                  old-class)))
          (t
           (let ((class (apply (if metaclass
                                   #'make-instance
                                   #'make-instance-standard-class)
                               (or metaclass
                                   +the-standard-class+)
                               :name name all-keys)))
             (%set-find-class name class)
             class)))))

(defmacro defclass (&whole form name direct-superclasses direct-slots &rest options)
  (unless (>= (length form) 3)
    (error 'program-error "Wrong number of arguments for DEFCLASS."))
  (check-declaration-type name)
  `(ensure-class ',name
                 :direct-superclasses
                 (canonicalize-direct-superclasses ',direct-superclasses)
                 :direct-slots
                 ,(canonicalize-direct-slots direct-slots)
                 ,@(canonicalize-defclass-options options)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defstruct method-combination
    name
    operator
    identity-with-one-argument
    documentation)

  (defun expand-short-defcombin (whole)
    (let* ((name (cadr whole))
           (documentation
            (getf (cddr whole) :documentation ""))
           (identity-with-one-arg
            (getf (cddr whole) :identity-with-one-argument nil))
           (operator
            (getf (cddr whole) :operator name)))
      `(progn
         (setf (get ',name 'method-combination-object)
               (make-method-combination :name ',name
                                        :operator ',operator
                                        :identity-with-one-argument ',identity-with-one-arg
                                        :documentation ',documentation))
         ',name)))

  (defun expand-long-defcombin (whole)
    (declare (ignore whole))
    (error "The long form of DEFINE-METHOD-COMBINATION is not implemented.")))

(defmacro define-method-combination (&whole form &rest args)
  (declare (ignore args))
  (if (and (cddr form)
           (listp (caddr form)))
      (expand-long-defcombin form)
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

(defstruct eql-specializer
  object)

(defparameter *eql-specializer-table* (make-hash-table :test 'eql))

(defun intern-eql-specializer (object)
  (or (gethash object *eql-specializer-table*)
      (setf (gethash object *eql-specializer-table*)
            (make-eql-specializer :object object))))

;; MOP (p. 216) specifies the following reader generic functions:
;;   generic-function-argument-precedence-order
;;   generic-function-declarations
;;   generic-function-lambda-list
;;   generic-function-method-class
;;   generic-function-method-combination
;;   generic-function-methods
;;   generic-function-name

(defun generic-function-lambda-list (gf)
  (%generic-function-lambda-list gf))
(defsetf generic-function-lambda-list %set-generic-function-lambda-list)

(defun (setf generic-function-documentation) (new-value gf)
  (set-generic-function-documentation gf new-value))

(defun (setf generic-function-initial-methods) (new-value gf)
  (set-generic-function-initial-methods gf new-value))

(defun (setf generic-function-methods) (new-value gf)
  (set-generic-function-methods gf new-value))

(defun (setf generic-function-method-class) (new-value gf)
  (set-generic-function-method-class gf new-value))

(defun (setf generic-function-method-combination) (new-value gf)
  (set-generic-function-method-combination gf new-value))

(defun (setf generic-function-argument-precedence-order) (new-value gf)
  (set-generic-function-argument-precedence-order gf new-value))

(declaim (ftype (function * t) classes-to-emf-table))
(defun classes-to-emf-table (gf)
  (generic-function-classes-to-emf-table gf))

(defun (setf classes-to-emf-table) (new-value gf)
  (set-generic-function-classes-to-emf-table gf new-value))

(defun (setf method-lambda-list) (new-value method)
  (set-method-lambda-list method new-value))

(defun (setf method-qualifiers) (new-value method)
  (set-method-qualifiers method new-value))

(defun (setf method-documentation) (new-value method)
  (set-method-documentation method new-value))

;;; defgeneric

(defmacro defgeneric (function-name lambda-list
                                    &rest options-and-method-descriptions)
  (let ((options ())
        (methods ())
        (documentation nil))
    (dolist (item options-and-method-descriptions)
      (case (car item)
        (declare) ; FIXME
        (:documentation
         (when documentation
           (error 'program-error
                  :format-control "Documentation option was specified twice for generic function ~S."
                  :format-arguments (list function-name)))
         (setf documentation t)
         (push item options))
        (:method
         (push
          `(push (defmethod ,function-name ,@(cdr item))
                 (generic-function-initial-methods (fdefinition ',function-name)))
          methods))
        (t
         (push item options))))
    (setf options (nreverse options)
          methods (nreverse methods))
    `(prog1
       (%defgeneric
        ',function-name
        :lambda-list ',lambda-list
        ,@(canonicalize-defgeneric-options options))
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

;; From OpenMCL.
(defun canonicalize-argument-precedence-order (apo req)
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
      (when (typep gf 'generic-function)
        ;; Remove methods defined by previous DEFGENERIC forms.
        (dolist (method (generic-function-initial-methods gf))
          (%remove-method gf method))
        (setf (generic-function-initial-methods gf) '()))))
  (apply 'ensure-generic-function function-name all-keys))

(defun ensure-generic-function (function-name
                                &rest all-keys
                                &key
                                lambda-list
                                (generic-function-class +the-standard-generic-function-class+)
                                (method-class +the-standard-method-class+)
                                (method-combination 'standard)
                                (argument-precedence-order nil apo-p)
                                documentation
                                &allow-other-keys)
  (when (autoloadp function-name)
    (resolve function-name))
  (let ((gf (find-generic-function function-name nil)))
    (if gf
        (progn
          (unless (or (null (generic-function-methods gf))
                      (lambda-lists-congruent-p lambda-list (generic-function-lambda-list gf)))
            (error 'simple-error
                   :format-control "The lambda list ~S is incompatible with the existing methods of ~S."
                   :format-arguments (list lambda-list gf)))
          (setf (generic-function-lambda-list gf) lambda-list)
          (setf (generic-function-documentation gf) documentation)
          (let* ((plist (analyze-lambda-list lambda-list))
                 (required-args (getf plist ':required-args)))
            (%set-gf-required-args gf required-args)
            (when apo-p
              (setf (generic-function-argument-precedence-order gf)
                    (if argument-precedence-order
                        (canonicalize-argument-precedence-order argument-precedence-order
                                                                required-args)
                        nil)))
            (finalize-generic-function gf))
          gf)
        (progn
          (when (and (null *clos-booting*)
                     (fboundp function-name))
            (error 'program-error
                   :format-control "~A already names an ordinary function, macro, or special operator."
                   :format-arguments (list function-name)))
          (setf gf (apply (if (eq generic-function-class +the-standard-generic-function-class+)
                              #'make-instance-standard-generic-function
                              #'make-instance)
                          generic-function-class
                          :name function-name
                          :method-class method-class
                          :method-combination method-combination
                          all-keys))
          gf))))

(defun initial-discriminating-function (gf args)
  (set-funcallable-instance-function
   gf
   (funcall (if (eq (class-of gf) +the-standard-generic-function-class+)
                #'std-compute-discriminating-function
                #'compute-discriminating-function)
            gf))
  (apply gf args))

(defun collect-eql-specializer-objects (generic-function)
  (let ((result nil))
    (dolist (method (generic-function-methods generic-function))
      (dolist (specializer (%method-specializers method))
        (when (typep specializer 'eql-specializer)
          (pushnew (eql-specializer-object specializer)
                   result
                   :test 'eql))))
    result))

(defun finalize-generic-function (gf)
  (%finalize-generic-function gf)
  (setf (classes-to-emf-table gf) (make-hash-table :test #'equal))
  (%init-eql-specializations gf (collect-eql-specializer-objects gf))
  (set-funcallable-instance-function
   gf #'(lambda (&rest args)
          (initial-discriminating-function gf args)))
  ;; FIXME Do we need to warn on redefinition somewhere else?
  (let ((*warn-on-redefinition* nil))
    (setf (fdefinition (%generic-function-name gf)) gf))
  (values))

(defun make-instance-standard-generic-function (generic-function-class
                                                &key name lambda-list
                                                method-class
                                                method-combination
                                                argument-precedence-order
                                                documentation)
  (declare (ignore generic-function-class))
  (let ((gf (std-allocate-instance +the-standard-generic-function-class+)))
    (%set-generic-function-name gf name)
    (setf (generic-function-lambda-list gf) lambda-list)
    (setf (generic-function-initial-methods gf) ())
    (setf (generic-function-methods gf) ())
    (setf (generic-function-method-class gf) method-class)
    (setf (generic-function-method-combination gf) method-combination)
    (setf (generic-function-documentation gf) documentation)
    (setf (classes-to-emf-table gf) nil)
    (let* ((plist (analyze-lambda-list (generic-function-lambda-list gf)))
           (required-args (getf plist ':required-args)))
      (%set-gf-required-args gf required-args)
      (setf (generic-function-argument-precedence-order gf)
            (if argument-precedence-order
                (canonicalize-argument-precedence-order argument-precedence-order
                                                        required-args)
                nil)))
    (finalize-generic-function gf)
    gf))

(defun canonicalize-specializers (specializers)
  (mapcar #'canonicalize-specializer specializers))

(defun canonicalize-specializer (specializer)
  (cond ((classp specializer)
         specializer)
        ((eql-specializer-p specializer)
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
              (extract-specializers specialized-lambda-list)
              documentation
              declarations
              (list* 'block
                     (fdefinition-block-name function-name)
                     real-body)))))

(defun required-portion (gf args)
  (let ((number-required (length (gf-required-args gf))))
    (when (< (length args) number-required)
      (error 'program-error
             :format-control "Not enough arguments for generic function ~S."
             :format-arguments (list (%generic-function-name gf))))
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
      ,@(if rv `(&rest ,rv) ())
      ,@(if (or ks keysp aok) `(&key ,@ks) ())
      ,@(if aok '(&allow-other-keys) ())
      ,@(if opts `(&optional ,@opts) ())
      ,@(if auxs `(&aux ,@auxs) ()))))

(defun extract-specializers (specialized-lambda-list)
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
        (state :parsing-required))
    (dolist (arg lambda-list)
      (if (member arg lambda-list-keywords)
          (ecase arg
            (&optional
             (setq state :parsing-optional))
            (&rest
             (setq state :parsing-rest))
            (&key
             (setq keysp t)
             (setq state :parsing-key))
            (&allow-other-keys
             (setq allow-other-keys 't))
            (&aux
             (setq state :parsing-aux)))
          (case state
            (:parsing-required
             (push-on-end arg required-args)
             (if (listp arg)
                 (progn (push-on-end (car arg) required-names)
                   (push-on-end (cadr arg) specializers))
                 (progn (push-on-end arg required-names)
                   (push-on-end 't specializers))))
            (:parsing-optional (push-on-end arg optionals))
            (:parsing-rest (setq rest-var arg))
            (:parsing-key
             (push-on-end (get-keyword-from-arg arg) keys)
             (push-on-end arg key-args))
            (:parsing-aux (push-on-end arg auxs)))))
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

(defun check-method-lambda-list (method-lambda-list gf-lambda-list)
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
      (error "The method has the wrong number of required arguments for the generic function."))
    (unless (= (length (getf gf-plist :optional-args))
               (length (getf method-plist :optional-args)))
      (error "The method has the wrong number of optional arguments for the generic function."))
    (unless (eq (or gf-restp gf-keysp) (or method-restp method-keysp))
      (error "The method and the generic function differ in whether they accept &REST or &KEY arguments."))
    (when (consp gf-keywords)
      (unless (or (and method-restp (not method-keysp))
                  method-allow-other-keys-p
                  (every (lambda (k) (memq k method-keywords)) gf-keywords))
        (error "The method does not accept all of the keyword arguments defined for the generic function.")))))

(declaim (ftype (function * method) ensure-method))
(defun ensure-method (name &rest all-keys)
  (let ((method-lambda-list (getf all-keys :lambda-list))
        (gf (find-generic-function name nil)))
    (if gf
        (check-method-lambda-list method-lambda-list (generic-function-lambda-list gf))
        (setf gf (ensure-generic-function name :lambda-list method-lambda-list)))
    (let ((method
           (if (eq (generic-function-method-class gf) +the-standard-method-class+)
               (apply #'make-instance-standard-method gf all-keys)
               (apply #'make-instance (generic-function-method-class gf) all-keys))))
      (%add-method gf method)
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
  (let ((method (std-allocate-instance +the-standard-method-class+)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (%set-method-specializers method (canonicalize-specializers specializers))
    (setf (method-documentation method) documentation)
    (%set-method-generic-function method nil)
    (%set-method-function method function)
    (%set-method-fast-function method fast-function)
    method))

(defun %add-method (gf method)
  (when (%method-generic-function method)
    (error 'simple-error
           :format-control "ADD-METHOD: ~S is a method of ~S."
           :format-arguments (list method (%method-generic-function method))))
  ;; Remove existing method with same qualifiers and specializers (if any).
  (let ((old-method (%find-method gf (method-qualifiers method)
                                 (%method-specializers method) nil)))
    (when old-method
      (%remove-method gf old-method)))
  (%set-method-generic-function method gf)
  (push method (generic-function-methods gf))
  (dolist (specializer (%method-specializers method))
    (when (typep specializer 'class) ;; FIXME What about EQL specializer objects?
      (pushnew method (class-direct-methods specializer))))
  (finalize-generic-function gf)
  gf)

(defun %remove-method (gf method)
  (setf (generic-function-methods gf)
        (remove method (generic-function-methods gf)))
  (%set-method-generic-function method nil)
  (dolist (specializer (%method-specializers method))
    (when (typep specializer 'class) ;; FIXME What about EQL specializer objects?
      (setf (class-direct-methods specializer)
            (remove method (class-direct-methods specializer)))))
  (finalize-generic-function gf)
  gf)

(defun %find-method (gf qualifiers specializers &optional (errorp t))
  ;; "If the specializers argument does not correspond in length to the number
  ;; of required arguments of the generic-function, an an error of type ERROR
  ;; is signaled."
  (unless (= (length specializers) (length (gf-required-args gf)))
    (error "The specializers argument has length ~S, but ~S has ~S required parameters."
           (length specializers)
           gf
           (length (gf-required-args gf))))
  (let* ((canonical-specializers (canonicalize-specializers specializers))
         (method
          (find-if #'(lambda (method)
                      (and (equal qualifiers
                                  (method-qualifiers method))
                           (equal canonical-specializers
                                  (%method-specializers method))))
                   (generic-function-methods gf))))
    (if (and (null method) errorp)
        (error "No such method for ~S." (%generic-function-name gf))
        method)))

(defun fast-callable-p (gf)
  (and (eq (generic-function-method-combination gf) 'standard)
       (null (intersection (%generic-function-lambda-list gf)
                           '(&rest &optional &key &allow-other-keys &aux)))))

(declaim (ftype (function * t) slow-method-lookup-1))

(declaim (ftype (function (t t t) t) slow-reader-lookup))
(defun slow-reader-lookup (gf layout slot-name)
  (let ((location (layout-slot-location layout slot-name)))
    (cache-slot-location gf layout location)
    location))

(defun std-compute-discriminating-function (gf)
  (let ((code
         (cond
           ((and (= (length (generic-function-methods gf)) 1)
                 (typep (car (generic-function-methods gf)) 'standard-reader-method))
            ;;                 (sys::%format t "standard reader function ~S~%" (generic-function-name gf))

            (let* ((method (%car (generic-function-methods gf)))
                   (class (car (%method-specializers method)))
                   (slot-name (reader-method-slot-name method)))
              #'(lambda (arg)
                  (declare (optimize speed))
                  (let* ((layout (std-instance-layout arg))
                         (location (get-cached-slot-location gf layout)))
                    (unless location
                      (unless (simple-typep arg class)
                        ;; FIXME no applicable method
                        (error 'simple-type-error
                               :datum arg
                               :expected-type class))
                      (setf location (slow-reader-lookup gf layout slot-name)))
                    (if (consp location)
                        ;; Shared slot.
                        (cdr location)
                        (standard-instance-access arg location))))))

           (t
            (let* ((emf-table (classes-to-emf-table gf))
                   (number-required (length (gf-required-args gf)))
                   (lambda-list (%generic-function-lambda-list gf))
                   (exact (null (intersection lambda-list
                                              '(&rest &optional &key
                                                &allow-other-keys &aux)))))
              (if exact
                  (cond
                    ((= number-required 1)
                     (cond
                       ((and (eq (generic-function-method-combination gf) 'standard)
                             (= (length (generic-function-methods gf)) 1))
                        (let* ((method (%car (generic-function-methods gf)))
                               (specializer (car (%method-specializers method)))
                               (function (or (%method-fast-function method)
                                             (%method-function method))))
                          (if (eql-specializer-p specializer)
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
                            (let* ((specialization
                                    (%get-arg-specialization gf arg))
                                   (emfun (or (gethash1 specialization
                                                        emf-table)
                                              (slow-method-lookup-1
                                               gf arg specialization))))
                              (if emfun
                                  (funcall emfun (list arg))
                                  (apply #'no-applicable-method gf (list arg))))))))
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
                                    :format-arguments (list (%generic-function-name gf)))))
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
                                 :format-arguments (list (%generic-function-name gf)))))
                      (let ((emfun (get-cached-emf gf args)))
                        (if emfun
                            (funcall emfun args)
                            (slow-method-lookup gf args))))))))))

    code))

(defun sort-methods (methods gf required-classes)
  (if (or (null methods) (null (%cdr methods)))
      methods
      (sort methods
	    (if (eq (class-of gf) +the-standard-generic-function-class+)
		#'(lambda (m1 m2)
		    (std-method-more-specific-p m1 m2 required-classes
						(generic-function-argument-precedence-order gf)))
		#'(lambda (m1 m2)
		    (method-more-specific-p gf m1 m2 required-classes))))))

(defun method-applicable-p (method args)
  (do* ((specializers (%method-specializers method) (cdr specializers))
        (args args (cdr args)))
       ((null specializers) t)
    (let ((specializer (car specializers)))
      (if (typep specializer 'eql-specializer)
          (unless (eql (car args) (eql-specializer-object specializer))
            (return nil))
          (unless (subclassp (class-of (car args)) specializer)
            (return nil))))))

(defun %compute-applicable-methods (gf args)
  (let ((required-classes (mapcar #'class-of (required-portion gf args)))
        (methods '()))
    (dolist (method (generic-function-methods gf))
      (when (method-applicable-p method args)
        (push method methods)))
    (sort-methods methods gf required-classes)))

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
  (do* ((specializers (%method-specializers method) (cdr specializers))
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

(defun slow-method-lookup (gf args)
  (let ((applicable-methods (%compute-applicable-methods gf args)))
    (if applicable-methods
        (let ((emfun (funcall (if (eq (class-of gf) +the-standard-generic-function-class+)
                                  #'std-compute-effective-method-function
                                  #'compute-effective-method-function)
                              gf applicable-methods)))
          (cache-emf gf args emfun)
          (funcall emfun args))
        (apply #'no-applicable-method gf args))))

(defun slow-method-lookup-1 (gf arg arg-specialization)
  (let ((applicable-methods (%compute-applicable-methods gf (list arg))))
    (if applicable-methods
        (let ((emfun (funcall (if (eq (class-of gf) +the-standard-generic-function-class+)
                                  #'std-compute-effective-method-function
                                  #'compute-effective-method-function)
                              gf applicable-methods)))
          (when emfun
            (setf (gethash arg-specialization (classes-to-emf-table gf)) emfun))
          emfun))))

(defun sub-specializer-p (c1 c2 c-arg)
  (find c2 (cdr (memq c1 (%class-precedence-list c-arg)))))

(defun std-method-more-specific-p (method1 method2 required-classes argument-precedence-order)
  (if argument-precedence-order
      (let ((specializers-1 (%method-specializers method1))
            (specializers-2 (%method-specializers method2)))
        (dolist (index argument-precedence-order)
          (let ((spec1 (nth index specializers-1))
                (spec2 (nth index specializers-2)))
            (unless (eq spec1 spec2)
              (cond ((eql-specializer-p spec1)
                     (return t))
                    ((eql-specializer-p spec2)
                     (return nil))
                    (t
                     (return (sub-specializer-p spec1 spec2
                                                (nth index required-classes)))))))))
      (do ((specializers-1 (%method-specializers method1) (cdr specializers-1))
           (specializers-2 (%method-specializers method2) (cdr specializers-2))
           (classes required-classes (cdr classes)))
          ((null specializers-1) nil)
        (let ((spec1 (car specializers-1))
              (spec2 (car specializers-2)))
          (unless (eq spec1 spec2)
            (cond ((eql-specializer-p spec1)
                   (return t))
                  ((eql-specializer-p spec2)
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

(defun std-compute-effective-method-function (gf methods)
  (let* ((mc (generic-function-method-combination gf))
         (mc-name (if (atom mc) mc (%car mc)))
         (options (if (atom mc) '() (%cdr mc)))
         (order (car options))
         (primaries '())
         (arounds '())
         around
         emf-form)
    (dolist (m methods)
      (let ((qualifiers (method-qualifiers m)))
        (cond ((null qualifiers)
               (if (eq mc-name 'standard)
                   (push m primaries)
                   (error "Method combination type mismatch.")))
              ((cdr qualifiers)
               (error "Invalid method qualifiers."))
              ((eq (car qualifiers) :around)
               (push m arounds))
              ((eq (car qualifiers) mc-name)
               (push m primaries))
              ((memq (car qualifiers) '(:before :after)))
              (t
               (error "Invalid method qualifiers.")))))
    (unless (eq order :most-specific-last)
      (setf primaries (nreverse primaries)))
    (setf arounds (nreverse arounds))
    (setf around (car arounds))
    (when (null primaries)
      (error "No primary methods for the generic function ~S." gf))
    (cond
      (around
       (let ((next-emfun
              (funcall
               (if (eq (class-of gf) +the-standard-generic-function-class+)
                   #'std-compute-effective-method-function
                   #'compute-effective-method-function)
               gf (remove around methods))))
         (setf emf-form
;;;           `(lambda (args)
;;;          (funcall ,(%method-function around) args ,next-emfun))
               (generate-emf-lambda (%method-function around) next-emfun)
               )))
      ((eq mc-name 'standard)
       (let* ((next-emfun (compute-primary-emfun (cdr primaries)))
              (befores (remove-if-not #'before-method-p methods))
              (reverse-afters
               (reverse (remove-if-not #'after-method-p methods))))
         (setf emf-form
               (cond
                 ((and (null befores) (null reverse-afters))
                  (let ((fast-function (%method-fast-function (car primaries))))

                    (if fast-function
                        (ecase (length (gf-required-args gf))
                          (1
                           #'(lambda (args)
                               (declare (optimize speed))
                               (funcall fast-function (car args))))
                          (2
                           #'(lambda (args)
                               (declare (optimize speed))
                               (funcall fast-function (car args) (cadr args)))))
                        ;;                               `(lambda (args)
                        ;;                                  (declare (optimize speed))
                        ;;                                  (funcall ,(%method-function (car primaries)) args ,next-emfun))
                        (generate-emf-lambda (%method-function (car primaries))
                                             next-emfun))))
                 (t
                  (let ((method-function (%method-function (car primaries))))

                    #'(lambda (args)
                        (declare (optimize speed))
                        (dolist (before befores)
                          (funcall (%method-function before) args nil))
                        (multiple-value-prog1
                            (funcall method-function args next-emfun)
                          (dolist (after reverse-afters)
                            (funcall (%method-function after) args nil))))))))))
          (t
           (let ((mc-obj (get mc-name 'method-combination-object)))
             (unless mc-obj
               (error "Unsupported method combination type ~A." mc-name))
             (let* ((operator (method-combination-operator mc-obj))
                    (ioa (method-combination-identity-with-one-argument mc-obj)))
               (setf emf-form
                     (if (and (null (cdr primaries))
                              (not (null ioa)))
;;                          `(lambda (args)
;;                             (funcall ,(%method-function (car primaries)) args nil))
                         (generate-emf-lambda (%method-function (car primaries)) nil)
                         `(lambda (args)
                            (,operator ,@(mapcar
                                          (lambda (primary)
                                            `(funcall ,(%method-function primary) args nil))
                                          primaries)))))))))
    (or (ignore-errors (autocompile emf-form))
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
           (funcall (%method-function (car methods)) args next-emfun)))))

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

(defmacro defmethod (&rest args)
  (multiple-value-bind
      (function-name qualifiers lambda-list specializers documentation declarations body)
      (parse-defmethod args)
    (let* ((specializers-form '())
           (lambda-expression `(lambda ,lambda-list ,@declarations ,body))
           (method-function (compute-method-function lambda-expression))
           (fast-function (compute-method-fast-function lambda-expression))
           )
      (dolist (specializer specializers)
        (cond ((and (consp specializer) (eq (car specializer) 'eql))
               (push `(list 'eql ,(cadr specializer)) specializers-form))
              (t
               (push `',specializer specializers-form))))
      (setf specializers-form `(list ,@(nreverse specializers-form)))
      `(progn
         (ensure-method ',function-name
                        :lambda-list ',lambda-list
                        :qualifiers ',qualifiers
                        :specializers ,specializers-form
                        ,@(if documentation `(:documentation ,documentation))
                        :function (function ,method-function)
                        ,@(if fast-function `(:fast-function (function ,fast-function)))
                        )))))

;;; Reader and writer methods

(defun make-instance-standard-reader-method (gf
                                             &key
                                             lambda-list
                                             qualifiers
                                             specializers
                                             documentation
                                             function
                                             fast-function
                                             slot-name)
  (declare (ignore gf))
  (let ((method (std-allocate-instance +the-standard-reader-method-class+)))
    (setf (method-lambda-list method) lambda-list)
    (setf (method-qualifiers method) qualifiers)
    (%set-method-specializers method (canonicalize-specializers specializers))
    (setf (method-documentation method) documentation)
    (%set-method-generic-function method nil)
    (%set-method-function method function)
    (%set-method-fast-function method fast-function)
    (set-reader-method-slot-name method slot-name)
    method))

(defun add-reader-method (class function-name slot-name)
  (let* ((lambda-expression
          (if (eq (class-of class) +the-standard-class+)
              `(lambda (object) (std-slot-value object ',slot-name))
              `(lambda (object) (slot-value object ',slot-name))))
         (method-function (compute-method-function lambda-expression))
         (fast-function (compute-method-fast-function lambda-expression)))
    (let ((method-lambda-list '(object))
          (gf (find-generic-function function-name nil)))
      (if gf
          (check-method-lambda-list method-lambda-list (generic-function-lambda-list gf))
          (setf gf (ensure-generic-function function-name :lambda-list method-lambda-list)))
      (let ((method
             (make-instance-standard-reader-method gf
                                                   :lambda-list '(object)
                                                   :qualifiers ()
                                                   :specializers (list class)
                                                   :function (if (autoloadp 'compile)
                                                                 method-function
                                                                 (autocompile method-function))
                                                   :fast-function (if (autoloadp 'compile)
                                                                      fast-function
                                                                      (autocompile fast-function))
                                                   :slot-name slot-name)))
        (%add-method gf method)
        method))))

(defun add-writer-method (class function-name slot-name)
  (let* ((lambda-expression
          (if (eq (class-of class) +the-standard-class+)
              `(lambda (new-value object)
                 (setf (std-slot-value object ',slot-name) new-value))
              `(lambda (new-value object)
                 (setf (slot-value object ',slot-name) new-value))))
         (method-function (compute-method-function lambda-expression))
         (fast-function (compute-method-fast-function lambda-expression))
         )
    (ensure-method function-name
                   :lambda-list '(new-value object)
                   :qualifiers ()
                   :specializers (list +the-T-class+ class)
;;                    :function `(function ,method-function)
                   :function (if (autoloadp 'compile)
                                 method-function
                                 (autocompile method-function))
                   :fast-function (if (autoloadp 'compile)
                                      fast-function
                                      (autocompile fast-function))
                   )))

(defmacro redefine-class-forwarder (name slot &optional alternative-name)
  (let* (($name (if (consp name) (cadr name) name))
         (%name (intern (concatenate 'string
                                     "%"
                                     (if (consp name)
                                         (symbol-name 'set-) "")
                                     (symbol-name $name))
                        (find-package "SYS"))))
    (unless alternative-name
      (setf alternative-name name))
    (if (consp name)
        `(progn ;; setter
           (defgeneric ,alternative-name (new-value class))
           (defmethod ,alternative-name (new-value (class built-in-class))
             (,%name new-value class))
           (defmethod ,alternative-name (new-value (class forward-referenced-class))
             (,%name new-value class))
           (defmethod ,alternative-name (new-value (class structure-class))
             (,%name new-value class))
           (defmethod ,alternative-name (new-value (class standard-class))
             (setf (slot-value class ',slot) new-value))
           ,@(unless (eq name alternative-name)
                     `((setf (get ',$name 'SETF-FUNCTION)
                             (symbol-function ',alternative-name))))
           )
        `(progn ;; getter
           (defgeneric ,alternative-name (class))
           (defmethod ,alternative-name ((class built-in-class))
             (,%name class))
           (defmethod ,alternative-name ((class forward-referenced-class))
             (,%name class))
           (defmethod ,alternative-name ((class structure-class))
             (,%name class))
           (defmethod ,alternative-name ((class standard-class))
             (slot-value class ',slot))
           ,@(unless (eq name alternative-name)
                     `((setf (symbol-function ',$name)
                             (symbol-function ',alternative-name))))
           ) )))

(redefine-class-forwarder class-name name)
(redefine-class-forwarder (setf class-name) name)
(redefine-class-forwarder class-slots slots)
(redefine-class-forwarder (setf class-slots) slots)
(redefine-class-forwarder class-direct-slots direct-slots)
(redefine-class-forwarder (setf class-direct-slots) direct-slots)
(redefine-class-forwarder class-layout layout)
(redefine-class-forwarder (setf class-layout) layout)
(redefine-class-forwarder class-direct-superclasses direct-superclasses)
(redefine-class-forwarder (setf class-direct-superclasses) direct-superclasses)
(redefine-class-forwarder class-direct-subclasses direct-subclasses)
(redefine-class-forwarder (setf class-direct-subclasses) direct-subclasses)
(redefine-class-forwarder class-direct-methods direct-methods !class-direct-methods)
(redefine-class-forwarder (setf class-direct-methods) direct-methods !!class-direct-methods)
(redefine-class-forwarder class-precedence-list precedence-list)
(redefine-class-forwarder (setf class-precedence-list) precedence-list)
(redefine-class-forwarder class-finalized-p finalized-p)
(redefine-class-forwarder (setf class-finalized-p) finalized-p)
(redefine-class-forwarder class-default-initargs default-initargs)
(redefine-class-forwarder (setf class-default-initargs) default-initargs)
(redefine-class-forwarder class-direct-default-initargs direct-default-initargs)
(redefine-class-forwarder (setf class-direct-default-initargs) direct-default-initargs)

(defgeneric direct-slot-definition-class (class &rest initargs))

(defmethod direct-slot-definition-class ((class class) &rest initargs)
  (declare (ignore initargs))
  +the-direct-slot-definition-class+)

(defgeneric effective-slot-definition-class (class &rest initargs))

(defmethod effective-slot-definition-class ((class class) &rest initargs)
  (declare (ignore initargs))
  +the-effective-slot-definition-class+)

(fmakunbound 'documentation)
(defgeneric documentation (x doc-type))

(defgeneric (setf documentation) (new-value x doc-type))

(defmethod documentation ((x symbol) doc-type)
  (%documentation x doc-type))

(defmethod (setf documentation) (new-value (x symbol) doc-type)
  (%set-documentation x doc-type new-value))

(defmethod documentation ((x function) doc-type)
  (%documentation x doc-type))

(defmethod (setf documentation) (new-value (x function) doc-type)
  (%set-documentation x doc-type new-value))

;; FIXME This should be a weak hashtable!
(defvar *list-documentation-hashtable* (make-hash-table :test #'equal))

(defmethod documentation ((x list) (doc-type (eql 'function)))
  (let ((alist (gethash x *list-documentation-hashtable*)))
    (and alist (cdr (assoc doc-type alist)))))

(defmethod documentation ((x list) (doc-type (eql 'compiler-macro)))
  (let ((alist (gethash x *list-documentation-hashtable*)))
    (and alist (cdr (assoc doc-type alist)))))

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'function)))
  (let* ((alist (gethash x *list-documentation-hashtable*))
         (entry (and alist (assoc doc-type alist))))
    (cond (entry
           (setf (cdr entry) new-value))
          (t
           (setf (gethash x *list-documentation-hashtable*)
                 (push (cons doc-type new-value) alist)))))
  new-value)

(defmethod (setf documentation) (new-value (x list) (doc-type (eql 'compiler-macro)))
  (let* ((alist (gethash x *list-documentation-hashtable*))
         (entry (and alist (assoc doc-type alist))))
    (cond (entry
           (setf (cdr entry) new-value))
          (t
           (setf (gethash x *list-documentation-hashtable*)
                 (push (cons doc-type new-value) alist)))))
  new-value)

(defmethod documentation ((x standard-class) (doc-type (eql 't)))
  (class-documentation x))

(defmethod documentation ((x standard-class) (doc-type (eql 'type)))
  (class-documentation x))

(defmethod (setf documentation) (new-value (x standard-class) (doc-type (eql 't)))
  (%set-class-documentation x new-value))

(defmethod (setf documentation) (new-value (x standard-class) (doc-type (eql 'type)))
  (%set-class-documentation x new-value))

(defmethod documentation ((x structure-class) (doc-type (eql 't)))
  (%documentation x doc-type))

(defmethod documentation ((x structure-class) (doc-type (eql 'type)))
  (%documentation x doc-type))

(defmethod (setf documentation) (new-value (x structure-class) (doc-type (eql 't)))
  (%set-documentation x doc-type new-value))

(defmethod (setf documentation) (new-value (x structure-class) (doc-type (eql 'type)))
  (%set-documentation x doc-type new-value))

(defmethod documentation ((x standard-generic-function) (doc-type (eql 't)))
  (generic-function-documentation x))

(defmethod (setf documentation) (new-value (x standard-generic-function) (doc-type (eql 't)))
  (setf (generic-function-documentation x) new-value))

(defmethod documentation ((x standard-generic-function) (doc-type (eql 'function)))
  (generic-function-documentation x))

(defmethod (setf documentation) (new-value (x standard-generic-function) (doc-type (eql 'function)))
  (setf (generic-function-documentation x) new-value))

(defmethod documentation ((x standard-method) (doc-type (eql 't)))
  (method-documentation x))

(defmethod (setf documentation) (new-value (x standard-method) (doc-type (eql 't)))
  (setf (method-documentation x) new-value))

(defmethod documentation ((x package) (doc-type (eql 't)))
  (%documentation x doc-type))

(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (%set-documentation x doc-type new-value))

;;; Applicable methods

(defgeneric compute-applicable-methods (gf args)
  (:method ((gf standard-generic-function) args)
    (%compute-applicable-methods gf args)))

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

(export '(compute-applicable-methods
	  compute-applicable-methods-using-classes))


;;; Slot access

(defun set-slot-value-using-class (new-value class instance slot-name)
  (declare (ignore class)) ; FIXME
  (setf (std-slot-value instance slot-name) new-value))

(defgeneric slot-value-using-class (class instance slot-name))

(defmethod slot-value-using-class ((class standard-class) instance slot-name)
  (std-slot-value instance slot-name))

(defgeneric (setf slot-value-using-class) (new-value class instance slot-name))
(defmethod (setf slot-value-using-class) (new-value
                                          (class standard-class)
                                          instance
                                          slot-name)
  (setf (std-slot-value instance slot-name) new-value))

(defgeneric slot-exists-p-using-class (class instance slot-name))

(defmethod slot-exists-p-using-class (class instance slot-name)
  nil)

(defmethod slot-exists-p-using-class ((class standard-class) instance slot-name)
  (std-slot-exists-p instance slot-name))

(defmethod slot-exists-p-using-class ((class structure-class) instance slot-name)
  (dolist (dsd (class-slots class))
    (when (eq (sys::dsd-name dsd) slot-name)
      (return-from slot-exists-p-using-class t)))
  nil)

(defgeneric slot-boundp-using-class (class instance slot-name))
(defmethod slot-boundp-using-class ((class standard-class) instance slot-name)
  (std-slot-boundp instance slot-name))

(defgeneric slot-makunbound-using-class (class instance slot-name))
(defmethod slot-makunbound-using-class ((class standard-class)
                                        instance
                                        slot-name)
  (std-slot-makunbound instance slot-name))

(defgeneric slot-missing (class instance slot-name operation &optional new-value))

(defmethod slot-missing ((class t) instance slot-name operation &optional new-value)
  (declare (ignore new-value))
  (error "The slot ~S is missing from the class ~S." slot-name class))

(defgeneric slot-unbound (class instance slot-name))

(defmethod slot-unbound ((class t) instance slot-name)
  (error 'unbound-slot :instance instance :name slot-name))

;;; Instance creation and initialization

(defgeneric allocate-instance (class &rest initargs &key &allow-other-keys))

(defmethod allocate-instance ((class standard-class) &rest initargs)
  (declare (ignore initargs))
  (std-allocate-instance class))

(defmethod allocate-instance ((class structure-class) &rest initargs)
  (declare (ignore initargs))
  (%make-structure (class-name class)
                   (make-list (length (class-slots class))
                              :initial-element +slot-unbound+)))

;; "The set of valid initialization arguments for a class is the set of valid
;; initialization arguments that either fill slots or supply arguments to
;; methods, along with the predefined initialization argument :ALLOW-OTHER-KEYS."
;; 7.1.2

(defun check-initargs (instance shared-initialize-param initargs)
  (when (oddp (length initargs))
    (error 'program-error
           :format-control "Odd number of keyword arguments."))
  (unless (getf initargs :allow-other-keys)
    (let ((methods 
	   (nconc 
	    (compute-applicable-methods 
	     #'shared-initialize
	     (if initargs
		 `(,instance ,shared-initialize-param ,@initargs)
	       (list instance shared-initialize-param)))
	    (compute-applicable-methods 
	     #'initialize-instance
	     (if initargs
		 `(,instance ,@initargs)
	       (list instance)))))
	  (slots (class-slots (class-of instance))))
      (do* ((tail initargs (cddr tail))
            (initarg (car tail) (car tail)))
           ((null tail))
        (unless (or (valid-initarg-p initarg slots)
		    (valid-methodarg-p initarg methods)
                    (eq initarg :allow-other-keys))
          (error 'program-error
                 :format-control "Invalid initarg ~S."
                 :format-arguments (list initarg)))))))

(defun valid-methodarg-p (initarg methods)
  (when (symbolp initarg)
    (dolist (method methods nil)
      (let ((valid-initargs (method-lambda-list method)))
	(when (find (symbol-value initarg) valid-initargs 
		     :test #'(lambda (a b)
			       (if (listp b)
				   (string= a (car b))
				 (or
				  (string= a b)
				  (string= b "&ALLOW-OTHER-KEYS")))))

	  (return t))))))

(defun valid-initarg-p (initarg slots)
  (dolist (slot slots nil)
    (let ((valid-initargs (slot-definition-initargs slot)))
      (when (memq initarg valid-initargs)
        (return t)))))

(defgeneric make-instance (class &rest initargs &key &allow-other-keys))

(defmethod make-instance ((class standard-class) &rest initargs)
  (when (oddp (length initargs))
    (error 'program-error :format-control "Odd number of keyword arguments."))
  (unless (class-finalized-p class)
    (std-finalize-inheritance class))
  (let ((class-default-initargs (class-default-initargs class)))
    (when class-default-initargs
      (let ((default-initargs '()))
        (do* ((list class-default-initargs (cddr list))
              (key (car list) (car list))
              (fn (cadr list) (cadr list)))
             ((null list))
          (when (eq (getf initargs key 'not-found) 'not-found)
            (setf default-initargs (append default-initargs (list key (funcall fn))))))
        (setf initargs (append initargs default-initargs)))))

  (let ((instance (std-allocate-instance class)))
    (check-initargs instance t initargs)
    (apply #'initialize-instance instance initargs)
    instance))

(defmethod make-instance ((class symbol) &rest initargs)
  (apply #'make-instance (find-class class) initargs))

(defgeneric initialize-instance (instance &key))

(defmethod initialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance t initargs))

(defgeneric reinitialize-instance (instance &key))

;; "The system-supplied primary method for REINITIALIZE-INSTANCE checks the
;; validity of initargs and signals an error if an initarg is supplied that is
;; not declared as valid. The method then calls the generic function SHARED-
;; INITIALIZE with the following arguments: the instance, nil (which means no
;; slots should be initialized according to their initforms), and the initargs
;; it received."
(defmethod reinitialize-instance ((instance standard-object) &rest initargs)
  (apply #'shared-initialize instance () initargs))

(defun std-shared-initialize (instance slot-names all-keys)
  (when (oddp (length all-keys))
    (error 'program-error :format-control "Odd number of keyword arguments."))
  (do* ((tail all-keys (cddr tail))
	(initarg (car tail) (car tail)))
      ((null tail))
    (when (and initarg (not (symbolp initarg)))
      (error 'program-error
	     :format-control "Invalid initarg ~S."
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

(defgeneric shared-initialize (instance slot-names &key))

(defmethod shared-initialize ((instance standard-object) slot-names &rest initargs)
  (std-shared-initialize instance slot-names initargs))

(defmethod shared-initialize ((slot slot-definition) slot-names
			      &rest initargs
			      &key name initargs initform initfunction
			      readers writers allocation
			      &allow-other-keys)
  ;;Keyword args are duplicated from init-slot-definition only to have
  ;;them checked.
  (declare (ignore slot-names)) ;;TODO?
  (declare (ignore name initargs initform initfunction readers writers allocation))
  (apply #'init-slot-definition slot initargs))

;;; change-class

(defgeneric change-class (instance new-class &key))

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
               (old-slot (find slot-name old-slots :key #'slot-definition-name)))
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

(defgeneric update-instance-for-different-class (old new &key))

(defmethod update-instance-for-different-class
  ((old standard-object) (new standard-object) &rest initargs)
  (let ((added-slots
         (remove-if #'(lambda (slot-name)
                       (slot-exists-p old slot-name))
                    (mapcar #'slot-definition-name
                            (class-slots (class-of new))))))
    (check-initargs new added-slots initargs)
    (apply #'shared-initialize new added-slots initargs)))

;;; make-instances-obsolete

(defgeneric make-instances-obsolete (class))

(defmethod make-instances-obsolete ((class standard-class))
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
  (check-initargs instance added-slots initargs)
  (apply #'shared-initialize instance added-slots initargs))

;;;  Methods having to do with class metaobjects.

(defmethod initialize-instance :after ((class standard-class) &rest args)
  (apply #'std-after-initialization-for-classes class args))

;;; Finalize inheritance

(defgeneric finalize-inheritance (class))

(defmethod finalize-inheritance ((class standard-class))
  (std-finalize-inheritance class))

;;; Class precedence lists

(defgeneric compute-class-precedence-list (class))
(defmethod compute-class-precedence-list ((class standard-class))
  (std-compute-class-precedence-list class))

;;; Slot inheritance

(defgeneric compute-slots (class))
(defmethod compute-slots ((class standard-class))
  (std-compute-slots class))

(defgeneric compute-effective-slot-definition (class direct-slots))
(defmethod compute-effective-slot-definition
  ((class standard-class) direct-slots)
  (std-compute-effective-slot-definition class direct-slots))

;;; Methods having to do with generic function metaobjects.

(defmethod initialize-instance :after ((gf standard-generic-function) &key)
  (finalize-generic-function gf))

;;; Methods having to do with generic function invocation.

(defgeneric compute-discriminating-function (gf))
(defmethod compute-discriminating-function ((gf standard-generic-function))
  (std-compute-discriminating-function gf))

(defgeneric method-more-specific-p (gf method1 method2 required-classes))

(defmethod method-more-specific-p ((gf standard-generic-function)
                                   method1 method2 required-classes)
  (std-method-more-specific-p method1 method2 required-classes
                              (generic-function-argument-precedence-order gf)))

(defgeneric compute-effective-method-function (gf methods))
(defmethod compute-effective-method-function ((gf standard-generic-function) methods)
  (std-compute-effective-method-function gf methods))

(defgeneric compute-applicable-methods (gf args))
(defmethod compute-applicable-methods ((gf standard-generic-function) args)
  (%compute-applicable-methods gf args))

;;; Slot definition accessors

(mapcar (lambda (sym)
	  (fmakunbound sym) ;;we need to redefine them as GFs
	  (export sym))
	'(slot-definition-allocation 
	  slot-definition-initargs
	  slot-definition-initform
	  slot-definition-initfunction
	  slot-definition-name))

(defgeneric slot-definition-allocation (slot-definition)
  (:method ((slot-definition slot-definition))
    (let ((cl (class-of slot-definition)))
      (case cl
	((+the-slot-definition-class+
	  +the-direct-slot-definition-class+
	  +the-effective-slot-definition-class+)
	 (%slot-definition-allocation slot-definition))
	(t (slot-value slot-definition 'sys::allocation))))))

(defgeneric slot-definition-initargs (slot-definition)
  (:method ((slot-definition slot-definition))
    (let ((cl (class-of slot-definition)))
      (case cl
	((+the-slot-definition-class+
	  +the-direct-slot-definition-class+
	  +the-effective-slot-definition-class+)
	 (%slot-definition-initargs slot-definition))
	(t (slot-value slot-definition 'sys::initargs))))))

(defgeneric slot-definition-initform (slot-definition)
  (:method ((slot-definition slot-definition))
    (let ((cl (class-of slot-definition)))
      (case cl
	((+the-slot-definition-class+
	  +the-direct-slot-definition-class+
	  +the-effective-slot-definition-class+)
	 (%slot-definition-initform slot-definition))
	(t (slot-value slot-definition 'sys::initform))))))

(defgeneric slot-definition-initfunction (slot-definition)
  (:method ((slot-definition slot-definition))
    (let ((cl (class-of slot-definition)))
      (case cl
	((+the-slot-definition-class+
	  +the-direct-slot-definition-class+
	  +the-effective-slot-definition-class+)
	 (%slot-definition-initfunction slot-definition))
	(t (slot-value slot-definition 'sys::initfunction))))))

(defgeneric slot-definition-name (slot-definition)
  (:method ((slot-definition slot-definition))
    (let ((cl (class-of slot-definition)))
      (case cl
	((+the-slot-definition-class+
	  +the-direct-slot-definition-class+
	  +the-effective-slot-definition-class+)
	 (%slot-definition-name slot-definition))
	(t (slot-value slot-definition 'sys::name))))))

;;; No %slot-definition-type.


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
          (defclass ,name ,parent-types ,slot-specs ,@options)
          ',name))
      (string
       `(progn
          (defclass ,name ,parent-types ,slot-specs ,@options)
          (defmethod print-object ((condition ,name) stream)
            (if *print-escape*
                (call-next-method)
                (progn (write-string ,report stream) condition)))
          ',name))
      (t
       `(progn
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

(fmakunbound 'no-applicable-method)
(defgeneric no-applicable-method (generic-function &rest args))

(defmethod no-applicable-method (generic-function &rest args)
  (error "There is no applicable method for the generic function ~S when called with arguments ~S."
         generic-function
         args))

(defgeneric find-method (generic-function
                         qualifiers
                         specializers
                         &optional errorp))

(defmethod find-method ((generic-function standard-generic-function)
			qualifiers specializers &optional (errorp t))
  (%find-method generic-function qualifiers specializers errorp))

(defgeneric add-method (generic-function method))

(defmethod add-method ((generic-function standard-generic-function) (method method))
  (let ((method-lambda-list (method-lambda-list method))
        (gf-lambda-list (generic-function-lambda-list generic-function)))
    (check-method-lambda-list method-lambda-list gf-lambda-list))
  (%add-method generic-function method))

(defgeneric remove-method (generic-function method))

(defmethod remove-method ((generic-function standard-generic-function) method)
  (%remove-method generic-function method))

;; See describe.lisp.
(defgeneric describe-object (object stream))

;; FIXME
(defgeneric no-next-method (generic-function method &rest args))

;; FIXME
(defgeneric function-keywords (method))

(setf *clos-booting* nil)

(defgeneric class-prototype (class))

(defmethod class-prototype :before (class)
  (unless (class-finalized-p class)
    (error "~@<~S is not finalized.~:@>" class)))

(defmethod class-prototype ((class standard-class))
  (allocate-instance class))

(defmethod class-prototype ((class structure-class))
  (allocate-instance class))

(provide 'clos)
