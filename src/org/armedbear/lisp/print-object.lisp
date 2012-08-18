;;; print-object.lisp
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

(require 'clos)

(when (autoloadp 'print-object)
  (fmakunbound 'print-object))

(defgeneric print-object (object stream))

(defmethod print-object ((object t) stream)
  (print-unreadable-object (object stream :type t :identity t)
     (write-string (%write-to-string object) stream)))

(defmethod print-object ((object standard-object) stream)
  (write-string (%write-to-string object) stream))

(defmethod print-object ((object structure-object) stream)
  (write-string (%write-to-string object) stream))

(defmethod print-object ((class class) stream)
  (print-unreadable-object (class stream :identity t)
    ;; Avoid recursive errors for uninitialized class objects, e.g. when
    ;; validate-superclass fails
    (format stream "~S ~S" (class-name (class-of class)) (ignore-errors (class-name class))))
  class)

(defmethod print-object ((gf generic-function) stream)
  (print-unreadable-object (gf stream :identity t)
    (format stream "~S ~S"
            (class-name (class-of gf))
            (ignore-errors (mop:generic-function-name gf))))
  gf)

(defmethod print-object ((method method) stream)
  (print-unreadable-object (method stream :identity t)
    (format stream "~S ~S~{ ~S~} ~S"
            (class-name (class-of method))
            (mop:generic-function-name
             (mop:method-generic-function method))
            (method-qualifiers method)
            (mapcar #'(lambda (c)
                        (if (typep c 'mop:eql-specializer)
                            `(eql ,(mop:eql-specializer-object c))
                          (class-name c)))
                    (mop:method-specializers method))))
  method)

(defmethod print-object ((method-combination method-combination) stream)
  (print-unreadable-object (method-combination stream :identity t)
    (format stream "~A ~S" (class-name (class-of method-combination))
            (ignore-errors (mop::method-combination-name method-combination))))
  method-combination)

(defmethod print-object ((restart restart) stream)
  (if *print-escape*
      (print-unreadable-object (restart stream :type t :identity t)
        (prin1 (restart-name restart) stream))
      (restart-report restart stream)))

(defmethod print-object ((c condition) stream)
  (if *print-escape*
      (call-next-method)
      (if (slot-boundp c 'format-control)
          (apply #'format stream
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c))
          (call-next-method))))

(defmethod print-object ((c type-error) stream)
  (if *print-escape*
      (call-next-method)
      (if (slot-boundp c 'format-control)
          (apply 'format stream
                 (simple-condition-format-control c)
                 (simple-condition-format-arguments c))
          (format stream "The value ~S is not of type ~S."
                  (type-error-datum c)
                  (type-error-expected-type c)))))

(defmethod print-object ((x undefined-function) stream)
  (if *print-escape*
      (call-next-method)
      (format stream "The function ~S is undefined." (cell-error-name x))))

(defmethod print-object ((x unbound-variable) stream)
  (if *print-escape*
      (print-unreadable-object (x stream :identity t)
        (format stream "~S ~S"
                (type-of x)
                (cell-error-name x)))
      (format stream "The variable ~S is unbound." (cell-error-name x))))

(provide "PRINT-OBJECT")
