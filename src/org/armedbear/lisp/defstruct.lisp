;;; defstruct.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves <peter@armedbear.org>
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

(in-package "SYSTEM")

(export 'compiler-defstruct)

;;; DEFSTRUCT-DESCRIPTION

(defmacro dd-name (x)                `(aref ,x  0))
(defmacro dd-conc-name (x)           `(aref ,x  1))
(defmacro dd-default-constructor (x) `(aref ,x  2))
(defmacro dd-constructors (x)        `(aref ,x  3))
(defmacro dd-copier (x)              `(aref ,x  4))
(defmacro dd-include (x)             `(aref ,x  5))
(defmacro dd-type (x)                `(aref ,x  6))
(defmacro dd-named (x)               `(aref ,x  7))
(defmacro dd-initial-offset (x)      `(aref ,x  8))
(defmacro dd-predicate (x)           `(aref ,x  9))
(defmacro dd-print-function (x)      `(aref ,x 10))
(defmacro dd-print-object (x)        `(aref ,x 11))
(defmacro dd-direct-slots (x)        `(aref ,x 12))
(defmacro dd-slots (x)               `(aref ,x 13))
(defmacro dd-inherited-accessors (x) `(aref ,x 14))

(defun make-defstruct-description (&key name
                                        conc-name
                                        default-constructor
                                        constructors
                                        copier
                                        include
                                        type
                                        named
                                        initial-offset
                                        predicate
                                        print-function
                                        print-object
                                        direct-slots
                                        slots
                                        inherited-accessors)
  (let ((dd (make-array 15)))
    (setf (dd-name dd) name
          (dd-conc-name dd) conc-name
          (dd-default-constructor dd) default-constructor
          (dd-constructors dd) constructors
          (dd-copier dd) copier
          (dd-include dd) include
          (dd-type dd) type
          (dd-named dd) named
          (dd-initial-offset dd) initial-offset
          (dd-predicate dd) predicate
          (dd-print-function dd) print-function
          (dd-print-object dd) print-object
          (dd-direct-slots dd) direct-slots
          (dd-slots dd) slots
          (dd-inherited-accessors dd) inherited-accessors)
    dd))

;;; DEFSTRUCT-SLOT-DESCRIPTION

(defmacro dsd-name (x)      `(aref ,x 1))
(defmacro dsd-index (x)     `(aref ,x 2))
(defmacro dsd-reader (x)    `(aref ,x 3))
(defmacro dsd-initform (x)  `(aref ,x 4))
(defmacro dsd-type (x)      `(aref ,x 5))
(defmacro dsd-read-only (x) `(aref ,x 6))

(defun make-defstruct-slot-description (&key name
                                             index
                                             reader
                                             initform
                                             (type t)
                                             read-only)
  (let ((dsd (make-array 7)))
    (setf (aref dsd 0) 'defstruct-slot-description
          (dsd-name dsd) name
          (dsd-index dsd) index
          (dsd-reader dsd) reader
          (dsd-initform dsd) initform
          (dsd-type dsd) type
          (dsd-read-only dsd) read-only)
    dsd))

(defvar *dd-name*)
(defvar *dd-conc-name*)
(defvar *dd-default-constructor*)
(defvar *dd-constructors*)
(defvar *dd-copier*)
(defvar *dd-include*)
(defvar *dd-type*)
(defvar *dd-default-slot-type* t)
(defvar *dd-named*)
(defvar *dd-initial-offset*)
(defvar *dd-predicate*)
(defvar *dd-print-function*)
(defvar *dd-print-object*)
(defvar *dd-direct-slots*)
(defvar *dd-slots*)
(defvar *dd-inherited-accessors*)

(defun keywordify (symbol)
  (intern (symbol-name symbol) +keyword-package+))

(defun define-keyword-constructor (constructor)
  (let* ((constructor-name (car constructor))
         (keys ())
         (values ()))
    (dolist (slot *dd-slots*)
      (let ((name (dsd-name slot))
            (initform (dsd-initform slot)))
        (if (or name (dsd-reader slot))
            (let ((dummy (gensym)))
              (push (list (list (keywordify name) dummy) initform) keys)
              (push dummy values))
            (push initform values))))
    (setf keys (cons '&key (nreverse keys))
          values (nreverse values))
    (cond ((eq *dd-type* 'list)
           `((defun ,constructor-name ,keys
               (list ,@values))))
          ((or (eq *dd-type* 'vector)
               (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
           (let ((element-type (if (consp *dd-type*) (cadr *dd-type*) t)))
             `((defun ,constructor-name ,keys
                 (make-array ,(length values)
                             :element-type ',element-type
                             :initial-contents (list ,@values))))))
          ((<= 1 (length values) 6)
           `((defun ,constructor-name ,keys
               (make-structure (truly-the symbol ',*dd-name*) ,@values))))
          (t
           `((defun ,constructor-name ,keys
               (%make-structure (truly-the symbol ',*dd-name*) (list ,@values))))))))

(defun find-dsd (name)
  (dolist (dsd *dd-slots*)
    (when (string= name (dsd-name dsd))
      (return dsd))))

(defun get-slot (name)
;;   (let ((res (find name (dd-slots defstruct) :test #'string= :key #'dsd-name)))
  (let ((res nil))
    (dolist (dsd *dd-slots*)
      (when (string= name (dsd-name dsd))
        (setf res dsd)
        (return)))
    (if res
        (values (dsd-type res) (dsd-initform res))
        (values t nil))))

(defun define-boa-constructor (constructor)
  (multiple-value-bind (req opt restp rest keyp keys allowp auxp aux)
    (parse-lambda-list (cadr constructor))
    (let ((arglist ())
          (vars ())
          (types ())
          (skipped-vars ()))
      (dolist (arg req)
        (push arg arglist)
        (push arg vars)
        (push (get-slot arg) types))
      (when opt
        (push '&optional arglist)
        (dolist (arg opt)
          (cond ((consp arg)
                 (destructuring-bind
                  (name
                   &optional
                   (def (nth-value 1 (get-slot name)))
                   (supplied-test nil supplied-test-p))
                  arg
                  (push `(,name ,def ,@(if supplied-test-p `(,supplied-test) nil)) arglist)
                  (push name vars)
                  (push (get-slot name) types)))
                (t
                 (multiple-value-bind (type default) (get-slot arg)
                   (push `(,arg ,default) arglist)
                   (push arg vars)
                   (push type types))))))
      (when restp
        (push '&rest arglist)
        (push rest arglist)
        (push rest vars)
        (push 'list types))
      (when keyp
        (push '&key arglist)
        (dolist (key keys)
          (if (consp key)
              (destructuring-bind (wot
                                   &optional
                                   (def nil def-p)
                                   (supplied-test nil supplied-test-p))
                                  key
                                  (let ((name (if (consp wot)
                                                  (destructuring-bind (key var) wot
                                                                      (declare (ignore key))
                                                                      var)
                                                  wot)))
                                    (multiple-value-bind (type slot-def)
                                      (get-slot name)
                                      (push `(,wot ,(if def-p def slot-def)
                                                   ,@(if supplied-test-p `(,supplied-test) nil))
                                            arglist)
                                      (push name vars)
                                      (push type types))))
              (multiple-value-bind (type default) (get-slot key)
                (push `(,key ,default) arglist)
                (push key vars)
                (push type types)))))
      (when allowp
        (push '&allow-other-keys arglist))
      (when auxp
        (push '&aux arglist)
        (dolist (arg aux)
          (push arg arglist)
          (if (and (consp arg) (eql (length arg) 2))
              (let ((var (first arg)))
                (push var vars)
                (push (get-slot var) types))
              (push (if (consp arg) (first arg) arg) skipped-vars))))
      (setq arglist (nreverse arglist))
      (setq vars (nreverse vars))
      (setq types (nreverse types))
      (setq skipped-vars (nreverse skipped-vars))
      (let ((values ()))
        (dolist (dsd *dd-slots*)
          (let ((name (dsd-name dsd))
                var)
            (cond ((find name skipped-vars :test #'string=)
                   (push nil values))
                  ((setf var (find name vars :test #'string=))
                   (push var values))
                  (t
                   (push (dsd-initform dsd) values)))))
        (setf values (nreverse values))
        (let* ((constructor-name (car constructor)))
          (cond ((eq *dd-type* 'list)
                 `((defun ,constructor-name ,arglist
                     (list ,@values))))
                ((or (eq *dd-type* 'vector)
                     (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
                 (let ((element-type (if (consp *dd-type*) (cadr *dd-type*) t)))
                   `((defun ,constructor-name ,arglist
                       (make-array ,(length values)
                                   :element-type ',element-type
                                   :initial-contents (list ,@values))))))
                ((<= 1 (length values) 6)
                 `((declaim (inline ,constructor-name))
                   (defun ,constructor-name ,arglist
                     (make-structure (truly-the symbol ',*dd-name*) ,@values))))
                (t
                 `((declaim (inline ,constructor-name))
                   (defun ,constructor-name ,arglist
                     (%make-structure (truly-the symbol ',*dd-name*) (list ,@values)))))))))))

(defun default-constructor-name ()
  (intern (concatenate 'string "MAKE-" (symbol-name *dd-name*))))

(defun define-constructors ()
  (if *dd-constructors*
      (let ((results ()))
        (dolist (constructor *dd-constructors*)
          (when (car constructor)
            (setf results (nconc results
                                 (if (cadr constructor)
                                     (define-boa-constructor constructor)
                                     (define-keyword-constructor constructor))))))
        results)
      (define-keyword-constructor (cons (default-constructor-name) nil))))

(defun name-index ()
  (dolist (dsd *dd-slots*)
    (let ((name (dsd-name dsd))
          (initform (dsd-initform dsd)))
      (when (and (null name)
                 (equal initform (list 'quote *dd-name*)))
        (return-from name-index (dsd-index dsd)))))
  ;; We shouldn't get here.
  nil)

(defun define-predicate ()
  (when (and *dd-predicate*
             (or *dd-named* (null *dd-type*)))
    (let ((pred (if (symbolp *dd-predicate*)
                    *dd-predicate*
                    (intern *dd-predicate*))))
      (cond ((eq *dd-type* 'list)
             (let ((index (name-index)))
               `((defun ,pred (object)
                   (and (consp object)
                        (> (length object) ,index)
                        (eq (nth ,index object) ',*dd-name*))))))
            ((or (eq *dd-type* 'vector)
                 (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
             (let ((index (name-index)))
               `((defun ,pred (object)
                   (and (vectorp object)
                        (> (length object) ,index)
                        (eq (aref object ,index) ',*dd-name*))))))
            (t
             `((defun ,pred (object)
                 (simple-typep object ',*dd-name*))))))))

(defun define-reader (slot)
  (let ((accessor-name (dsd-reader slot))
        (index (dsd-index slot))
        (type (dsd-type slot)))
    (cond ((eq *dd-type* 'list)
           `((declaim (ftype (function * ,type) ,accessor-name))
             (defun ,accessor-name (instance) (elt instance ,index))))
          ((or (eq *dd-type* 'vector)
               (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
           `((declaim (ftype (function * ,type) ,accessor-name))
             (defun ,accessor-name (instance) (aref instance ,index))
             (define-source-transform ,accessor-name (instance)
               `(aref (truly-the ,',*dd-type* ,instance) ,,index))))
          (t
           `((declaim (ftype (function * ,type) ,accessor-name))
             (defun ,accessor-name (instance)
               (structure-ref (the ,*dd-name* instance) ,index))
             (define-source-transform ,accessor-name (instance)
               ,(if (eq type 't)
                    ``(structure-ref (the ,',*dd-name* ,instance) ,,index)
                    ``(the ,',type
                        (structure-ref (the ,',*dd-name* ,instance) ,,index)))))))))

(defun define-writer (slot)
  (let ((accessor-name (dsd-reader slot))
        (index (dsd-index slot)))
    (cond ((eq *dd-type* 'list)
           `((defun (setf ,accessor-name) (value instance)
               (%set-elt instance ,index value))))
          ((or (eq *dd-type* 'vector)
               (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
           `((defun (setf ,accessor-name) (value instance)
               (aset instance ,index value))
             (define-source-transform (setf ,accessor-name) (value instance)
               `(aset (truly-the ,',*dd-type* ,instance) ,,index ,value))))
          (t
           `((defun (setf ,accessor-name) (value instance)
               (structure-set (the ,*dd-name* instance) ,index value))
             (define-source-transform (setf ,accessor-name) (value instance)
               `(structure-set (the ,',*dd-name* ,instance)
                               ,,index ,value)))))))

(defun define-access-functions ()
  (let ((result ()))
    (dolist (slot *dd-slots*)
      (let ((accessor-name (dsd-reader slot)))
        (unless (assoc accessor-name *dd-inherited-accessors*)
          (setf result (nconc result (define-reader slot)))
          (unless (dsd-read-only slot)
            (setf result (nconc result (define-writer slot)))))))
    result))

(defun define-copier ()
  (when *dd-copier*
    (cond ((eq *dd-type* 'list)
           `((setf (fdefinition ',*dd-copier*) #'copy-list)))
          ((or (eq *dd-type* 'vector)
               (and (consp *dd-type*) (eq (car *dd-type*) 'vector)))
           `((setf (fdefinition ',*dd-copier*) #'copy-seq)))
          (t
           `((setf (fdefinition ',*dd-copier*) #'copy-structure))))))

(defun define-print-function ()
  (cond (*dd-print-function*
         (if (cadr *dd-print-function*)
             `((defmethod print-object ((instance ,*dd-name*) stream)
                 (funcall (function ,(cadr *dd-print-function*))
                          instance stream *current-print-level*)))
             `((defmethod print-object ((instance ,*dd-name*) stream)
                 (write-string (%write-to-string instance) stream)))))
        (*dd-print-object*
         (if (cadr *dd-print-object*)
             `((defmethod print-object ((instance ,*dd-name*) stream)
                 (funcall (function ,(cadr *dd-print-object*))
                          instance stream)))
             `((defmethod print-object ((instance ,*dd-name*) stream)
                 (write-string (%write-to-string instance) stream)))))
        (t
         nil)))

(defun parse-1-option (option)
  (case (car option)
    (:conc-name
     (setf *dd-conc-name* (if (symbolp (cadr option))
                              (cadr option)
                              (make-symbol (string (cadr option))))))
    (:constructor
     (let* ((args (cdr option))
            (numargs (length args)))
       (case numargs
         (0 ; Use default name.
          (push (list (default-constructor-name) nil) *dd-constructors*))
         (1
          (push (list (car args) nil) *dd-constructors*))
         (2
          (push args *dd-constructors*)))))
    (:copier
     (when (eql (length option) 2)
       (setf *dd-copier* (cadr option))))
    (:include
     (setf *dd-include* (cdr option)))
    (:initial-offset
     (setf *dd-initial-offset* (cadr option)))
    (:predicate
     (when (eql (length option) 2)
       (setf *dd-predicate* (cadr option))))
    (:print-function
     (setf *dd-print-function* option))
    (:print-object
     (setf *dd-print-object* option))
    (:type
     (setf *dd-type* (cadr option))
     (when (and (consp *dd-type*) (eq (car *dd-type*) 'vector))
       (unless (eq (second *dd-type*) '*)
         (setf *dd-default-slot-type* (second *dd-type*)))))))

(defun parse-name-and-options (name-and-options)
  (setf *dd-name* (the symbol (car name-and-options)))
  (setf *dd-conc-name* (make-symbol (concatenate 'string (symbol-name *dd-name*) "-")))
  (setf *dd-copier* (intern (concatenate 'string "COPY-" (symbol-name *dd-name*))))
  (setf *dd-predicate* (concatenate 'string (symbol-name *dd-name*) "-P"))
  (let ((options (cdr name-and-options)))
    (dolist (option options)
      (cond ((consp option)
             (parse-1-option option))
            ((eq option :named)
             (setf *dd-named* t))
            ((member option '(:constructor :copier :predicate :named :conc-name))
             (parse-1-option (list option)))
            (t
             (error "Unrecognized DEFSTRUCT option: ~S." option))))))

(defun compiler-defstruct (name &key
                                conc-name
                                default-constructor
                                constructors
                                copier
                                include
                                type
                                named
                                initial-offset
                                predicate
                                print-function
                                print-object
                                direct-slots
                                slots
                                inherited-accessors)
  (setf (get name 'structure-definition)
        (make-defstruct-description :name name
                                    :conc-name conc-name
                                    :default-constructor default-constructor
                                    :constructors constructors
                                    :copier copier
                                    :include include
                                    :type type
                                    :named named
                                    :initial-offset initial-offset
                                    :predicate predicate
                                    :print-function print-function
                                    :print-object print-object
                                    :direct-slots direct-slots
                                    :slots slots
                                    :inherited-accessors inherited-accessors))
  (when (or (null type) named)
    (make-structure-class name direct-slots slots (car include)))
  (when default-constructor
    (proclaim `(ftype (function * t) ,default-constructor))))

(defmacro defstruct (name-and-options &rest slots)
  (let ((*dd-name* nil)
        (*dd-conc-name* nil)
        (*dd-default-constructor* nil)
        (*dd-constructors* nil)
        (*dd-copier* nil)
        (*dd-include* nil)
        (*dd-type* nil)
        (*dd-default-slot-type* t)
        (*dd-named* nil)
        (*dd-initial-offset* nil)
        (*dd-predicate* nil)
        (*dd-print-function* nil)
        (*dd-print-object* nil)
        (*dd-direct-slots* ())
        (*dd-slots* ())
        (*dd-inherited-accessors* ()))
    (parse-name-and-options (if (atom name-and-options)
                                (list name-and-options)
                                name-and-options))
    (check-declaration-type *dd-name*)
    (if *dd-constructors*
        (dolist (constructor *dd-constructors*)
          (unless (cadr constructor)
            (setf *dd-default-constructor* (car constructor))
            (return)))
        (setf *dd-default-constructor* (default-constructor-name)))
    (when (stringp (car slots))
      (%set-documentation *dd-name* 'structure (pop slots)))
    (dolist (slot slots)
      (let* ((name (if (atom slot) slot (car slot)))
             (reader (if *dd-conc-name*
                         (intern (concatenate 'string
                                              (symbol-name *dd-conc-name*)
                                              (symbol-name name)))
                         name))
             (initform (if (atom slot) nil (cadr slot)))
             (dsd (apply #'make-defstruct-slot-description
                         :name name
                         :reader reader
                         :initform initform
                         (cond
                           ((atom slot)
                            (list :type *dd-default-slot-type*))
                           ((getf (cddr slot) :type)
                            (cddr slot))
                           (t
                            (list* :type *dd-default-slot-type* (cddr slot)))))))
        (push dsd *dd-direct-slots*)))
    (setf *dd-direct-slots* (nreverse *dd-direct-slots*))
    (let ((index 0))
      (when *dd-include*
        (let ((dd (get (car *dd-include*) 'structure-definition)))
          (unless dd
            (error 'simple-error
                   :format-control "Class ~S is undefined."
                   :format-arguments (list (car *dd-include*))))
          (dolist (dsd (dd-slots dd))
            ;; MUST COPY SLOT DESCRIPTION!
            (setf dsd (copy-seq dsd))
            (setf (dsd-index dsd) index
                  (dsd-reader dsd)
                  (if *dd-conc-name*
                      (intern (concatenate 'string
                                           (symbol-name *dd-conc-name*)
                                           (symbol-name (dsd-name dsd))))
                      (dsd-name dsd)))
            (push dsd *dd-slots*)
            (incf index))
          (setf *dd-inherited-accessors* (dd-inherited-accessors dd))
          (dolist (dsd (dd-direct-slots dd))
            (push (cons (dsd-reader dsd) (dsd-name dsd))
                  *dd-inherited-accessors*)))
        (when (cdr *dd-include*)
          (dolist (slot (cdr *dd-include*))
            (let* ((name (if (atom slot) slot (car slot)))
                   (initform (if (atom slot) nil (cadr slot)))
                   (dsd (find-dsd name)))
              (when dsd
                (setf (dsd-initform dsd) initform))))))
      (when *dd-initial-offset*
        (dotimes (i *dd-initial-offset*)
          (push (make-defstruct-slot-description :name nil
                                                 :index index
                                                 :reader nil
                                                 :initform nil
                                                 :type t
                                                 :read-only t)
                *dd-slots*)
          (incf index)))
      (when *dd-named*
        (push (make-defstruct-slot-description :name nil
                                               :index index
                                               :reader nil
                                               :initform (list 'quote *dd-name*)
                                               :type t
                                               :read-only t)
              *dd-slots*)
        (incf index))
      (dolist (dsd *dd-direct-slots*)
        (setf (dsd-index dsd) index)
        (push dsd *dd-slots*)
        (incf index)))
    (setf *dd-slots* (nreverse *dd-slots*))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (compiler-defstruct ',*dd-name*
                             :conc-name ',*dd-conc-name*
                             :default-constructor ',*dd-default-constructor*
                             ,@(if *dd-constructors* `(:constructors ',*dd-constructors*))
                             :copier ',*dd-copier*
                             ,@(if *dd-include* `(:include ',*dd-include*))
                             ,@(if *dd-type* `(:type ',*dd-type*))
                             ,@(if *dd-named* `(:named ,*dd-named*))
                             ,@(if *dd-initial-offset* `(:initial-offset ,*dd-initial-offset*))
                             :predicate ',*dd-predicate*
                             ,@(if *dd-print-function* `(:print-function ',*dd-print-function*))
                             ,@(if *dd-print-object* `(:print-object ',*dd-print-object*))
                             :direct-slots ',*dd-direct-slots*
                             :slots ',*dd-slots*
                             :inherited-accessors ',*dd-inherited-accessors*))
       ,@(define-constructors)
       ,@(define-predicate)
       ,@(define-access-functions)
       ,@(define-copier)
       ,@(define-print-function)
       ',*dd-name*)))

(defun defstruct-default-constructor (arg)
  (let ((type (cond ((symbolp arg)
                     arg)
                    ((classp arg)
                     (class-name arg))
                    (t
                     (type-of arg)))))
    (when type
      (let ((dd (get type 'structure-definition)))
        (and dd (dd-default-constructor dd))))))
