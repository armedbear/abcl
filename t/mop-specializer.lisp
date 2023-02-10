#| After ensuring <file:../abcl-prove.asd> is in ASDF:

(unless (asdf:make :abcl-prove/closer-mop)
(asdf:make :quicklisp-abcl)
(ql:quickload :abcl-prove/closer-mop))
(asdf:test-system :abcl-prove/closer-mop)

|# 

;;;; <https://github.com/armedbear/abcl/issues/539>
;;;;
;;;; Implement in SBCL (which should work) and ABCL (which is going to work)
;;;;
;;;; <https://research.gold.ac.uk/id/eprint/6828/1/jucs_14_20_3370_3388_newton.pdf>

(prove:plan 1)

(in-package :closer-common-lisp-user) ;; defined in CLOSER-MOP
(defclass fixnum>= (specializer)
  ((number :type fixnum :initarg :number :initform most-negative-fixnum)
   #+abcl
   (sys::direct-methods :initform nil :allocation :class)
   #+sbcl
   (sb-pcl::direct-methods :initform (cons nil nil) :allocation :class)))

(defmethod make-load-form ((spec fixnum>=) &optional env)
  (declare (ignore env))
  #+abcl
  (make-load-form-saving-slots spec :slot-names '(number))
  #+(or sbcl ecl)
  (make-load-form-saving-slots spec))

(defmethod class-name ((f fixnum>=))
  `(fixnum>= ,(slot-value f 'number)))

(defun fixnum>= (spec n)
  (and (typep n 'fixnum) (>= n (slot-value spec 'number))))

(defun fixnum>=-compare (spec-a spec-b)
  (> (slot-value spec-a 'number) (slot-value spec-b 'number)))

(defclass range-generic-function (standard-generic-function)
  ()
  (:metaclass closer-mop:funcallable-standard-class)
  (:default-initargs :method-class (find-class 'standard-method)))

(defmethod compute-applicable-methods-using-classes ((function range-generic-function) classes)
  (declare (ignore function classes))
  (values nil nil))

(defmethod compute-applicable-methods ((function range-generic-function) args)
  (let ((applicable-methods
          (remove-if-not (lambda (method)
                           (every #'fixnum>=
                                  (method-specializers method) args))
                         (generic-function-methods function))))
    (values (sort applicable-methods
                  (lambda (method-a method-b)
                    (fixnum>=-compare
                     ;; For simplicity, we only sort the applicable
                     ;; methods by their first arguments.
                     (first
                      (method-specializers method-a))
                     (first
                      (method-specializers method-b)))))
            t)))

   ;;; Unlike the default specializers provided by CL which are parsed
   ;;;  in `defmethod', a (reader) macro is required for custom
   ;;;  specializers to be created at compile time.
(defmacro define-range-method (name lambda-list &body body) 
  `(defmethod ,name ,(mapcar (lambda (spec)
                               (if (and (listp spec)
                                        (second spec)
                                        (listp (second spec))
                                        (eql (first (second spec)) 'fixnum>=))
                                   (list (first spec)
                                         (make-instance 'fixnum>= :number (second (second spec))))
                                   spec))
                      lambda-list)
     ,@body))

(defgeneric foo (number)
  (:generic-function-class range-generic-function))

(define-range-method foo ((number (fixnum>= 0)))
  (list 0))

(define-range-method foo ((number (fixnum>= 10)))
  (cons 10 (call-next-method)))

(define-range-method foo ((number (fixnum>= 100)))
  (cons 100 (call-next-method)))

(prove:ok
 (flet ((foo-handling-error (arg)
          (handler-case (foo arg)
            (sb-pcl::no-applicable-method-error ()
              'no-applicable-method))))
   (let ((result
           (list
            (foo-handling-error -1)
            (foo-handling-error 5.0)
            (foo-handling-error 5)
            (foo-handling-error 50)
            (foo-handling-error 500))))
     (prove:diag (format nil "result: ~a~%" result))
     (equalp result
             `(no-applicable-method
               no-applicable-method
               (0)
               (10 0)
               (100 10 0)))))
#|
(foo -1)                                ; NO-APPLICABLE-METHOD

(foo 5.0)                               ; NO-APPLICABLE-METHOD

(foo 5)                                 ; => (0)

(foo 50)                                ; => (10 0)

(foo 500)                               ; => (100 10 0)

|#
"Able to get CLOSER-MOP to work on specializer")

(in-package :cl-user)

(prove:finalize)

