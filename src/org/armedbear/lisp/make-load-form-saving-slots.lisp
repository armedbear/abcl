;;; make-load-form-saving-slots.lisp
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

(resolve 'defstruct)

(defun make-load-form-saving-slots (object &key slot-names environment)
  (declare (ignore environment))
  (let ((class (class-of object))
        (inits ())
        (instance (gensym "INSTANCE-")))
    (cond ((typep object 'structure-object)
           (let ((index 0))
             (dolist (slot (%class-slots class))
               (let ((slot-name (dsd-name slot)))
                 (when (or (memq slot-name slot-names)
                           (null slot-names))
                   (let ((value (structure-ref object index)))
                     (push `(structure-set ,instance ,index ',value) inits))))
               (incf index))))
          ((typep object 'standard-object)
           (dolist (slot (%class-slots class))
             (let ((slot-name (%slot-definition-name slot)))
               (when (or (memq slot-name slot-names)
                         (null slot-names))
                 (when (slot-boundp object slot-name)
                   (let ((value (slot-value object slot-name)))
                     (push `(setf (slot-value ,instance ',slot-name) ',value) inits))))))))
    (values `(let ((,instance (allocate-instance (find-class ',(%class-name class)))))
               (progn ,@inits)
               ,instance)
            nil)))
