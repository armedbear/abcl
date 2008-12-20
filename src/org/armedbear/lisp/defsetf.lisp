;;; defsetf.lisp
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

;;; Adapted from SBCL.

(in-package #:system)

(require '#:collect)

(defun %defsetf (orig-access-form num-store-vars expander)
  (collect ((subforms) (subform-vars) (subform-exprs) (store-vars))
           (dolist (subform (cdr orig-access-form))
             (if (constantp subform)
                 (subforms subform)
                 (let ((var (gensym)))
                   (subforms var)
                   (subform-vars var)
                   (subform-exprs subform))))
           (dotimes (i num-store-vars)
             (store-vars (gensym)))
           (values (subform-vars)
                   (subform-exprs)
                   (store-vars)
                   (funcall expander (cons (subforms) (store-vars)))
                   `(,(car orig-access-form) ,@(subforms)))))

(defmacro defsetf (access-fn &rest rest)
  (cond ((not (listp (car rest)))
	 `(eval-when (:load-toplevel :compile-toplevel :execute)
	    (%define-setf-macro ',access-fn
                                nil
                                ',(car rest)
				,(when (and (car rest) (stringp (cadr rest)))
				   `',(cadr rest)))))
	((and (cdr rest) (listp (cadr rest)))
	 (destructuring-bind
          (lambda-list (&rest store-variables) &body body)
          rest
          (let ((arglist-var (gensym "ARGS-"))
                (access-form-var (gensym "ACCESS-FORM-"))
                (env-var (gensym "ENVIRONMENT-")))
            (multiple-value-bind
              (body doc)
              (parse-defmacro `(,lambda-list ,@store-variables)
                              arglist-var body access-fn 'defsetf
                              :anonymousp t)
              `(eval-when (:load-toplevel :compile-toplevel :execute)
                 (%define-setf-macro
                  ',access-fn
                  #'(lambda (,access-form-var ,env-var)
                     (declare (ignore ,env-var))
                     (%defsetf ,access-form-var ,(length store-variables)
                               #'(lambda (,arglist-var)
                                  (block ,access-fn
                                    ,body))))
                  nil
                  ',doc))))))
	(t
	 (error "Ill-formed DEFSETF for ~S" access-fn))))
