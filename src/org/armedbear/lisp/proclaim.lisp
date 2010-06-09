;;; proclaim.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves
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
;;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
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

(in-package #:system)

(export '(check-declaration-type proclaimed-type proclaimed-ftype ftype-result-type))

(defmacro declaim (&rest decls)
`(eval-when (:compile-toplevel :load-toplevel :execute)
   ,@(mapcar (lambda (decl) `(proclaim ',decl))
             decls)))

(defun declaration-error (name)
  (error 'simple-error
         :format-control "The symbol ~S cannot be both the name of a type and the name of a declaration."
         :format-arguments (list name)))

(defvar *declaration-types* (make-hash-table :test 'eq))

;; "A symbol cannot be both the name of a type and the name of a declaration.
;; Defining a symbol as the name of a class, structure, condition, or type,
;; when the symbol has been declared as a declaration name, or vice versa,
;; signals an error."
(defun check-declaration-type (name)
  (when (gethash1 name (the hash-table *declaration-types*))
    (declaration-error name)))

(defun proclaim (declaration-specifier)
  (unless (symbolp (car declaration-specifier))
    (%type-error (car declaration-specifier) 'symbol))
  ;; (cdr declaration-specifier) must be a proper list.
  (unless (listp (cddr declaration-specifier))
    (%type-error (cddr declaration-specifier) 'list))
  (case (car declaration-specifier)
    (SPECIAL
     (dolist (name (cdr declaration-specifier))
       (%defvar name)))
    (OPTIMIZE
     (dolist (spec (cdr declaration-specifier))
       (let ((val 3)
             (quality spec))
         (when (consp spec)
           (setf quality (%car spec)
                 val (cadr spec)))
         (when (and (fixnump val)
                    (<= 0 val 3))
           (case quality
             (SPEED
              (setf *speed* val))
             (SPACE
              (setf *space* val))
             (SAFETY
              (setf *safety* val))
             (DEBUG
              (setf *debug* val)))))))
    (FTYPE
     (unless (cdr declaration-specifier)
       (error "No type specified in FTYPE declaration: ~S" declaration-specifier))
     (apply 'proclaim-ftype (cdr declaration-specifier)))
    (TYPE
     (unless (cdr declaration-specifier)
       (error "No type specified in TYPE declaration: ~S" declaration-specifier))
     (apply 'proclaim-type (cdr declaration-specifier)))
    ((INLINE NOTINLINE)
     (dolist (name (cdr declaration-specifier))
       (when (symbolp name) ; FIXME Need to support non-symbol function names.
         (setf (get name '%inline) (car declaration-specifier)))))
    (DECLARATION
     (dolist (name (cdr declaration-specifier))
       (when (or (get name 'deftype-definition)
                 (find-class name nil))
         (declaration-error name))
       (setf (gethash name (the hash-table *declaration-types*)) name)))
    (:explain
     (dolist (spec (cdr declaration-specifier))
       (let ((val t)
             (quality spec))
         (when (consp spec)
           (setf quality (%car spec))
           (when (= (length spec) 2)
             (setf val (%cadr spec))))
         (if val
             (pushnew quality *explain*)
             (setf *explain* (remove quality *explain*))))))))

(defun proclaim-type (type &rest names)
  (dolist (name names)
    (setf (get name 'proclaimed-type) type)))

(defun proclaimed-type (name)
  (get name 'proclaimed-type))

(declaim (type hash-table *proclaimed-ftypes*))
(defconst *proclaimed-ftypes* (make-hash-table :test 'equal))

(declaim (inline proclaim-ftype-1))
(defun proclaim-ftype-1 (ftype name)
  (declare (optimize speed))
  (if (symbolp name)
      (setf (get name 'proclaimed-ftype) ftype)
      (setf (gethash name *proclaimed-ftypes*) ftype)))
(declaim (notinline proclaim-ftype-1))

(defun proclaim-ftype (ftype &rest names)
  (declare (optimize speed))
  (declare (inline proclaim-ftype-1))
  (dolist (name names)
    (proclaim-ftype-1 ftype name)))

(defun proclaimed-ftype (name)
  (if (symbolp name)
      (get name 'proclaimed-ftype)
      (gethash1 name *proclaimed-ftypes*)))

(defun ftype-result-type (ftype)
  (if (atom ftype)
      '*
      (let ((result-type (third ftype)))
        (if result-type
            result-type
            '*))))
