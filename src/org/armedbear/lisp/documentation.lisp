;;; documentation.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves
;;; Copyright (C) 2010-2013 Mark Evenson
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


(in-package #:mop)

(require "CLOS")

(defgeneric documentation (x doc-type)
  (:method ((x symbol) doc-type)
    (%documentation x doc-type))
  (:method ((x function) doc-type)
    (%documentation x doc-type)))

(defgeneric (setf documentation) (new-value x doc-type)
  (:method (new-value (x symbol) doc-type)
    (%set-documentation x doc-type new-value))
  (:method (new-value (x function) doc-type)
    (%set-documentation x doc-type new-value)))


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
    (cond
      (entry (setf (cdr entry) new-value))
      (t (setf (gethash x *list-documentation-hashtable*)
               (push (cons doc-type new-value) alist)))))
  new-value)

(defmethod (setf documentation) (new-value (x list)
                                 (doc-type (eql 'compiler-macro)))
  (let* ((alist (gethash x *list-documentation-hashtable*))
         (entry (and alist (assoc doc-type alist))))
    (cond
      (entry (setf (cdr entry) new-value))
      (t (setf (gethash x *list-documentation-hashtable*)
               (push (cons doc-type new-value) alist)))))
  new-value)

(defmethod documentation ((x class) (doc-type (eql 't)))
  (class-documentation x))

(defmethod documentation ((x class) (doc-type (eql 'type)))
  (class-documentation x))

(defmethod (setf documentation) (new-value (x class) (doc-type (eql 't)))
  (%set-class-documentation x new-value))

(defmethod (setf documentation) (new-value (x class) (doc-type (eql 'type)))
  (%set-class-documentation x new-value))

(defmethod documentation ((x structure-class) (doc-type (eql 't)))
  (%documentation x t))

(defmethod documentation ((x structure-class) (doc-type (eql 'type)))
  (%documentation x t))

(defmethod (setf documentation) (new-value (x structure-class)
                                 (doc-type (eql 't)))
  (%set-documentation x t new-value))

(defmethod (setf documentation) (new-value (x structure-class)
                                 (doc-type (eql 'type)))
  (%set-documentation x t new-value))

(defmethod documentation ((x standard-generic-function) (doc-type (eql 't)))
  (std-slot-value x 'sys::%documentation))

(defmethod (setf documentation) (new-value (x standard-generic-function)
                                 (doc-type (eql 't)))
  (setf (std-slot-value x 'sys::%documentation) new-value))

(defmethod documentation ((x standard-generic-function)
                          (doc-type (eql 'function)))
  (std-slot-value x 'sys::%documentation))

(defmethod (setf documentation) (new-value (x standard-generic-function)
                                 (doc-type (eql 'function)))
  (setf (std-slot-value x 'sys::%documentation) new-value))

(defmethod documentation ((x standard-method) (doc-type (eql 't)))
  (method-documentation x))

(defmethod (setf documentation) (new-value (x standard-method)
                                 (doc-type (eql 't)))
  (setf (method-documentation x) new-value))

(defmethod documentation ((x standard-slot-definition) (doc-type (eql 't)))
  (slot-definition-documentation x))

(defmethod (setf documentation) (new-value (x standard-slot-definition)
                                 (doc-type (eql 't)))
  (setf (slot-definition-documentation x) new-value))

(defmethod documentation ((x package) (doc-type (eql 't)))
  (%documentation x doc-type))

(defmethod (setf documentation) (new-value (x package) (doc-type (eql 't)))
  (%set-documentation x doc-type new-value))

(defmethod documentation ((x symbol) (doc-type (eql 'function)))
  (if (and (fboundp x) (typep (fdefinition x) 'generic-function))
      (documentation (fdefinition x) doc-type)
      (%documentation x doc-type)))

(defmethod (setf documentation) (new-value (x symbol)
                                 (doc-type (eql 'function)))
  (if (and (fboundp x) (typep (fdefinition x) 'generic-function))
      (setf (documentation (fdefinition x) 'function) new-value)
      (%set-documentation x 'function new-value)))

(defmethod documentation ((x symbol) (doc-type (eql 'type)))
  (let ((class (find-class x nil)))
    (if class
        (documentation class t)
        (%documentation x 'type))))

(defmethod documentation ((x symbol) (doc-type (eql 'structure)))
  (%documentation x 'structure))

(defmethod (setf documentation) (new-value (x symbol) (doc-type (eql 'type)))
  (let ((class (find-class x nil)))
    (if class
        (setf (documentation class t) new-value)
        (%set-documentation x 'type new-value))))

(defmethod (setf documentation) (new-value (x symbol)
                                 (doc-type (eql 'structure)))
  (%set-documentation x 'structure new-value))
