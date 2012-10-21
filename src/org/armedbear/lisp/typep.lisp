;;; typep.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
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

(defun simple-array-p (object)
  (and (arrayp object)
       (not (array-has-fill-pointer-p object))
       (multiple-value-bind (displaced-to offset) (array-displacement object)
         (and (null displaced-to) (zerop offset)))))

(defun in-interval-p (x interval)
  (if (endp interval)
      t
      (let ((low (%car interval))
            (high (if (endp (%cdr interval)) '* (%cadr interval))))
        (cond ((eq low '*))
              ((consp low)
               (when (<= x (%car low))
                 (return-from in-interval-p nil)))
              ((when (< x low)
                 (return-from in-interval-p nil))))
        (cond ((eq high '*))
              ((consp high)
               (when (>= x (%car high))
                 (return-from in-interval-p nil)))
              ((when (> x high)
                 (return-from in-interval-p nil))))
        t)))

(defun match-dimensions (dim pat)
  (if (null dim)
      (null pat)
      (and (or (eq (car pat) '*)
	       (eql (car dim) (car pat)))
	   (match-dimensions (cdr dim) (cdr pat)))))

(defun %typep (object type)
  (when (atom type)
    (when (eq type 'values)
      (error 'simple-error
             :format-control "The symbol ~S is not valid as a type specifier."
             :format-arguments (list type)))
    (unless (and (symbolp type) (get type 'deftype-definition))
      (return-from %typep (simple-typep object type))))
  (setf type (normalize-type type))
  (when (atom type)
    (return-from %typep (simple-typep object type)))
  (let ((tp (%car type))
        (i (%cdr type)))
    (case tp
      (INTEGER
       (and (integerp object) (in-interval-p object i)))
      (RATIONAL
       (and (rationalp object) (in-interval-p object i)))
      ((FLOAT SINGLE-FLOAT DOUBLE-FLOAT SHORT-FLOAT LONG-FLOAT)
       (and (floatp object) (in-interval-p object i)))
      (REAL
       (and (realp object) (in-interval-p object i)))
      (COMPLEX
       (and (complexp object)
            (or (null i)
                (and (typep (realpart object) i)
                     (typep (imagpart object) i)))))
      (CONS
       (and (consp object)
            (or (null (car i)) (eq (car i) '*) (%typep (%car object) (car i)))
            (or (null (cadr i)) (eq (cadr i) '*) (%typep (%cdr object) (cadr i)))))
      (SIMPLE-BIT-VECTOR
       (and (simple-bit-vector-p object)
            (or (endp i)
                (eq (%car i) '*)
                (eql (%car i) (array-dimension object 0)))))
      (BIT-VECTOR
       (and (bit-vector-p object)
            (or (endp i)
                (eq (%car i) '*)
                (eql (%car i) (array-dimension object 0)))))
      (SIMPLE-STRING
       (and (simple-string-p object)
            (or (endp i)
                (eq (%car i) '*)
                (eql (%car i) (array-dimension object 0)))))
      (STRING
       (and (stringp object)
            (or (endp i)
                (eq (%car i) '*)
                (eql (%car i) (array-dimension object 0)))))
      (SIMPLE-VECTOR
       (and (simple-vector-p object)
            (or (endp i)
                (eq (%car i) '*)
                (eql (%car i) (array-dimension object 0)))))
      (VECTOR
       (and (vectorp object)
            (or (endp i)
                (eq (%car i) '*)
                (and (eq (%car i) t) (not (stringp object)) (not (bit-vector-p object)))
                (and (stringp object) (%subtypep (%car i) 'character))
                (equal (array-element-type object) (%car i)))
            (or (endp (cdr i))
                (eq (%cadr i) '*)
                (eql (%cadr i) (array-dimension object 0)))))
      (SIMPLE-ARRAY
       (and (simple-array-p object)
            (or (endp i)
                (eq (%car i) '*)
                (equal (array-element-type object) (upgraded-array-element-type (%car i))))
            (or (endp (cdr i))
                (eq (%cadr i) '*)
                (if (listp (%cadr i))
                    (match-dimensions (array-dimensions object) (%cadr i))
                    (eql (array-rank object) (%cadr i))))))
      (ARRAY
       (and (arrayp object)
            (or (endp i)
                (eq (%car i) '*)
                (equal (array-element-type object) (upgraded-array-element-type (%car i))))
            (or (endp (cdr i))
                (eq (%cadr i) '*)
                (if (listp (%cadr i))
                    (match-dimensions (array-dimensions object) (%cadr i))
                    (eql (array-rank object) (%cadr i))))))
      (AND
       (dolist (type i)
         (unless (%typep object type)
           (return-from %typep nil)))
       t)
      (OR
       (dolist (type i)
         (when (%typep object type)
           (return-from %typep t)))
       nil)
      (NOT
       (not (%typep object (car i))))
      (MEMBER
       (member object i))
      (EQL
       (eql object (car i)))
      (SATISFIES
       (unless (symbolp (car i))
         (error 'simple-type-error
                :datum (car i)
                :expected-type 'symbol
                :format-control "The SATISFIES predicate name is not a symbol: ~S"
                :format-arguments (list (car i))))
       (funcall (car i) object))
      (NIL-VECTOR
       (and (simple-typep object 'nil-vector)
            (or (endp i)
                (eql (%car i) (length object)))))
      (MOD
       (and (integerp object)
            (or (zerop object)
                (and (plusp object)
                     (< object (second type))))))
      ((FUNCTION VALUES)
       (error 'simple-error
              :format-control "~S types are not a legal argument to TYPEP: ~S"
              :format-arguments (list tp type)))
      (t
       nil))))

(defun typep (object type &optional environment)
  (declare (ignore environment))
  (%typep object type))
