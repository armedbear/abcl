;;; deftype.lisp
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

(defmacro deftype (name lambda-list &rest body)
  (when (eq (symbol-package name) +cl-package+)
    (error :format-control "Attempt to define ~S, a symbol in the COMMON-LISP package, as a type specifier."
           :format-arguments (list name)))
  (check-declaration-type name)
  ;; Optional and keyword parameters default to * rather than NIL.
  (when (or (memq '&optional lambda-list)
            (memq '&key lambda-list))
    (let ((new-lambda-list ())
          (state nil))
      (dolist (thing lambda-list)
        (cond ((eq thing '&optional)
               (setf state '&optional))
              ((eq thing '&key)
               (setf state '&key))
              ((memq thing lambda-list-keywords)
               (setf state nil))
              ((eq state '&optional)
               (when (symbolp thing)
                 (setf thing (list thing ''*))))
              ((eq state '&key)
               (when (symbolp thing)
                 (setf thing (list thing ''*)))))
        (push thing new-lambda-list))
      (setf lambda-list (nreverse new-lambda-list))))
  `(progn
     (setf (get ',name 'deftype-definition)
           #'(lambda ,lambda-list (block ,name ,@body)))
     ',name))

(defun expand-deftype (type)
  (let (tp i)
    (loop
      (if (consp type)
          (setf tp (%car type) i (%cdr type))
          (setf tp type
                i nil))
      (if (and (symbolp tp) (get tp 'deftype-definition))
          (setf type (apply (get tp 'deftype-definition) i))
          (return))))
  type)
