;;; source-transform.lisp
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

(export '(source-transform define-source-transform expand-source-transform))

(defun source-transform (name)
  (get-function-info-value name :source-transform))

(defun set-source-transform (name transform)
  (set-function-info-value name :source-transform transform))

(defsetf source-transform set-source-transform)

(defmacro define-source-transform (name lambda-list &rest body)
  (let* ((form (gensym))
         (env (gensym))
         (body (parse-defmacro lambda-list form body name 'defmacro
                               :environment env))
         (expander
          (if (symbolp name)
              `(lambda (,form) (block ,name ,body))
              `(lambda (,form) (block ,(cadr name) ,body)))))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (setf (source-transform ',name) ,expander)
       ',name)))

(defun expand-source-transform-1 (form)
  (let ((expander nil)
        (newdef nil))
    (cond ((atom form)
           (values form nil))
          ((and (consp (%car form))
                (eq (caar form) 'SETF)
                (setf expander (source-transform (%car form))))
           (values (setf newdef (funcall expander form))
                   (not (eq newdef form))))
          ((and (symbolp (%car form))
                (setf expander (source-transform (%car form))))
           (values (setf newdef (funcall expander form))
                   (not (eq newdef form))))
          (t
           (values form nil)))))

(defun expand-source-transform (form)
  (let ((expanded-p nil))
    (loop
      (multiple-value-bind (expansion exp-p) (expand-source-transform-1 form)
        (if exp-p
            (setf form expansion
                  expanded-p t)
            (return))))
    (values form expanded-p)))
