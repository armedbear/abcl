;;; coerce.lisp
;;;
;;; Copyright (C) 2004-2005 Peter Graves
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

(declaim (ftype (function (t) t) coerce-list-to-vector))
(defun coerce-list-to-vector (list)
  (let* ((length (length list))
         (result (make-array length)))
    (dotimes (i length)
      (declare (type index i))
      (setf (aref result i) (pop list)))
    result))

(declaim (ftype (function (string) simple-string) copy-string))
(defun copy-string (string)
  (declare (optimize speed (safety 0)))
  (declare (type string string))
  (let* ((length (length string))
         (copy (make-string length)))
    (dotimes (i length copy)
      (declare (type fixnum i))
      (setf (schar copy i) (char string i)))))

(defun coerce-error (object result-type)
  (error 'simple-type-error
         :datum object
         :format-control "~S cannot be converted to type ~S."
         :format-arguments (list object result-type)))

;; FIXME This is a special case for LOOP code, which does things like
;; (AND SINGLE-FLOAT REAL) and (AND SINGLE-FLOAT (REAL (0))).
(declaim (ftype (function (t t) t) coerce-object-to-and-type))
(defun coerce-object-to-and-type (object result-type)
  (when (and (consp result-type)
             (eq (%car result-type) 'AND)
             (= (length result-type) 3))
    (let* ((type1 (%cadr result-type))
           (type2 (%caddr result-type))
           (result (coerce object type1)))
      (when (typep object type2)
        (return-from coerce-object-to-and-type result))))
  (coerce-error object result-type))

(defun coerce (object result-type)
  (cond ((eq result-type t)
         object)
        ((typep object result-type)
         object)
        ((and (listp object)
              (eq result-type 'vector))
         (coerce-list-to-vector object))
        ((and (stringp object) ; a string, but not a simple-string
              (eq result-type 'simple-string))
         (copy-string object))
        ((eq result-type 'character)
         (cond ((and (stringp object)
                     (= (length object) 1))
                (char object 0))
               ((and (symbolp object)
                     (= (length (symbol-name object)) 1))
                (char (symbol-name object) 0))
               (t
                (coerce-error object result-type))))
        ((memq result-type '(float single-float short-float))
         (coerce-to-single-float object))
        ((memq result-type '(double-float long-float))
         (coerce-to-double-float object))
        ((eq result-type 'complex)
         (cond ((floatp object)
                (complex object 0.0))
               ((numberp object)
                object)
               (t
                (coerce-error object result-type))))
        ((eq result-type 'function)
         (coerce-to-function object))
        ((and (consp result-type)
              (eq (%car result-type) 'complex))
         (when (complexp object)
           (return-from coerce
             (complex (coerce (realpart object) (cadr result-type))
                      (coerce (imagpart object) (cadr result-type)))))
         (if (memq (%cadr result-type)
                   '(float single-float double-float short-float long-float))
             (complex (coerce object (cadr result-type))
                      (coerce 0.0 (cadr result-type)))
             object))
        ((and (consp result-type)
              (eq (%car result-type) 'AND))
         (coerce-object-to-and-type object result-type))
        ((and (simple-typep object 'sequence)
              (%subtypep result-type 'sequence))
         (concatenate result-type object))
        (t
         (let ((expanded-type (expand-deftype result-type)))
           (unless (eq expanded-type result-type)
             (return-from coerce (coerce object expanded-type))))
         (coerce-error object result-type))))
