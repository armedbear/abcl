;;; make-sequence.lisp
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

;;; Adapted from ECL.

(defun size-mismatch-error (type size)
  (error 'simple-type-error
         :format-control "The requested length (~D) does not match the specified type ~A."
         :format-arguments (list size type)))

(defun make-sequence (type size	&key (initial-element nil iesp))
  (let (element-type sequence class)
    (setf type (normalize-type type))
    (cond ((atom type)
	   (setf class (if (classp type) type (find-class type nil)))
           (when (classp type)
	     (let ((class-name (%class-name type)))
	       (when (member class-name '(LIST CONS STRING SIMPLE-STRING
					  BASE-STRING SIMPLE-BASE-STRING NULL
					  BIT-VECTOR SIMPLE-BIT-VECTOR VECTOR
					  SIMPLE-VECTOR))
		 (setf type class-name))))
	  ;;Else we suppose it's a user-defined sequence and move on
           (cond ((memq type '(LIST CONS))
                  (when (zerop size)
                    (if (eq type 'CONS)
                        (size-mismatch-error type size)
                        (return-from make-sequence nil)))
                  (return-from make-sequence
                               (if iesp
                                   (make-list size :initial-element initial-element)
                                   (make-list size))))
                 ((memq type '(STRING SIMPLE-STRING BASE-STRING SIMPLE-BASE-STRING))
                  (return-from make-sequence
                               (if iesp
                                   (make-string size :initial-element initial-element)
                                   (make-string size))))
                 ((eq type 'NULL)
                  (if (zerop size)
                      (return-from make-sequence nil)
                      (size-mismatch-error type size)))
                 (t
                  (setq element-type
                        (cond ((memq type '(BIT-VECTOR SIMPLE-BIT-VECTOR)) 'BIT)
                              ((memq type '(VECTOR SIMPLE-VECTOR)) t)
                              ((null class)
                               (error 'simple-type-error
                                      :format-control "~S is not a sequence type."
                                      :format-arguments (list type))))))))
	  (t
           (let ((name (%car type))
                 (args (%cdr type)))
             (when (eq name 'LIST)
               (return-from make-sequence
                            (if iesp
                                (make-list size :initial-element initial-element)
                                (make-list size))))
             (when (eq name 'CONS)
               (unless (plusp size)
                 (size-mismatch-error name size))
               (return-from make-sequence
                            (if iesp
                                (make-list size :initial-element initial-element)
                                (make-list size))))
             (unless (memq name '(ARRAY SIMPLE-ARRAY VECTOR SIMPLE-VECTOR
                                  BIT-VECTOR SIMPLE-BIT-VECTOR STRING SIMPLE-STRING
                                  BASE-STRING SIMPLE-BASE-STRING))
               (error 'simple-type-error
                      :format-control "~S is not a sequence type."
                      :format-arguments (list type)))
             (let ((len nil))
               (cond ((memq name '(STRING SIMPLE-STRING BASE-STRING SIMPLE-BASE-STRING))
                      (setf element-type 'character
                            len (car args)))
                     ((memq name '(ARRAY SIMPLE-ARRAY))
                      (setf element-type (or (car args) t)
                            len (if (consp (cadr args)) (caadr args) '*)))
                     ((memq name '(BIT-VECTOR SIMPLE-BIT-VECTOR))
                      (setf element-type 'bit
                            len (car args)))
                     (t
                      (setf element-type (or (car args) t)
                            len (cadr args))))
               (unless (or (null len) (eq len '*) (equal len '(*)))
                 (when (/= size len)
                   (size-mismatch-error type size)))))))
    (setq sequence
	  (cond ((or (not (atom type)) (subtypep type 'array))
		 (if iesp
		     (make-array size :element-type element-type :initial-element initial-element)
		     (make-array size :element-type element-type)))
		((and class (subtypep type 'sequence))
		 (if iesp
		     (sequence:make-sequence-like (mop::class-prototype class) size :initial-element initial-element)
		     (sequence:make-sequence-like (mop::class-prototype class) size)))
		(t (error 'simple-type-error
			  :format-control "~S is not a sequence type."
			  :format-arguments (list type)))))
    sequence))
