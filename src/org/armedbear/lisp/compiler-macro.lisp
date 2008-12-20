;;; compiler-macro.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves
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

(export 'compiler-macroexpand)

(defvar *compiler-macros* (make-hash-table :test #'equal))

(defun compiler-macro-function (name &optional environment)
  (declare (ignore environment))
  (gethash1 name (the hash-table *compiler-macros*)))

(defun (setf compiler-macro-function) (new-function name &optional environment)
  (declare (ignore environment))
  (setf (gethash name (the hash-table *compiler-macros*)) new-function))

(defmacro define-compiler-macro (name lambda-list &rest body)
  (let* ((form (gensym))
         (env (gensym)))
    (multiple-value-bind (body decls)
        (parse-defmacro lambda-list form body name 'defmacro :environment env)
      (let ((expander `(lambda (,form ,env)
                         (declare (ignorable ,env))
                         (block ,(fdefinition-block-name name) ,body))))
        `(progn
           (setf (compiler-macro-function ',name) (function ,expander))
           ',name)))))

;;; Adapted from OpenMCL.
(defun compiler-macroexpand-1 (form &optional env)
  (let ((expander nil)
        (new-form nil))
    (if (and (consp form)
             (symbolp (%car form))
             (setq expander (compiler-macro-function (%car form) env)))
        (values (setq new-form (funcall expander form env))
                (neq new-form form))
        (values form
                nil))))

(defun compiler-macroexpand (form &optional env)
  (let ((expanded-p nil))
    (loop
      (multiple-value-bind (expansion exp-p)
          (compiler-macroexpand-1 form env)
        (if exp-p
            (setf form expansion
                  expanded-p t)
            (return))))
    (values form expanded-p)))

