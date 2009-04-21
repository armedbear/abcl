;;; late-setf.lisp
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

;;; From CMUCL/SBCL.

(in-package #:system)

(defmacro define-setf-expander (access-fn lambda-list &body body)
  (require-type access-fn 'symbol)
  (let ((whole (gensym "WHOLE-"))
	(environment (gensym "ENV-")))
    (multiple-value-bind (body local-decs doc)
			 (parse-defmacro lambda-list whole body access-fn
					 'define-setf-expander
					 :environment environment)
      `(progn
         ,@(when doc
             `((%set-documentation ',access-fn 'setf ,doc)))
         (setf (get ',access-fn 'setf-expander)
             #'(lambda (,whole ,environment)
                ,@local-decs
                (block ,access-fn ,body)))
         ',access-fn))))

(define-setf-expander values (&rest places &environment env)
  (let ((setters ())
        (getters ())
        (all-dummies ())
        (all-vals ())
        (newvals ()))
    (dolist (place places)
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (setf all-dummies (append all-dummies dummies (cdr newval))
              all-vals (append all-vals vals
                               (mapcar (constantly nil) (cdr newval)))
              newvals (append newvals (list (car newval))))
        (push setter setters)
        (push getter getters)))
    (values all-dummies all-vals newvals
            `(values ,@(reverse setters)) `(values ,@(reverse getters)))))

(defun make-gensym-list (n)
  (let ((list ()))
    (dotimes (i n list)
      (push (gensym) list))))

(define-setf-expander getf (place prop &optional default &environment env)
  (multiple-value-bind (temps values stores set get)
    (get-setf-expansion place env)
    (let ((newval (gensym))
          (ptemp (gensym))
          (def-temp (if default (gensym))))
      (values `(,@temps ,ptemp ,@(if default `(,def-temp)))
              `(,@values ,prop ,@(if default `(,default)))
              `(,newval)
              `(let ((,(car stores) (%putf ,get ,ptemp ,newval)))
                 ,set
                 ,newval)
              `(getf ,get ,ptemp ,@(if default `(,def-temp)))))))

(define-setf-expander apply (functionoid &rest args)
  (unless (and (listp functionoid)
               (= (length functionoid) 2)
               (eq (first functionoid) 'function)
               (memq (second functionoid) '(aref bit sbit)))
    (error "SETF of APPLY is only defined for #'AREF, #'BIT and #'SBIT."))
  (let ((function (second functionoid))
        (new-var (gensym))
        (vars (make-gensym-list (length args))))
    (values vars args (list new-var)
            `(apply #'(setf ,function) ,new-var ,@vars)
            `(apply #',function ,@vars))))

(define-setf-expander the (type place &environment env)
  (multiple-value-bind (temps subforms store-vars setter getter)
    (get-setf-expansion place env)
    (values temps subforms store-vars
            `(multiple-value-bind ,store-vars
               (the ,type (values ,@store-vars))
               ,setter)
            `(the ,type ,getter))))

(defun (setf macro-function) (new-function symbol &optional environment)
  (declare (ignore environment))
  (let ((macro (make-macro symbol (or (precompile nil new-function)
                                      new-function))))
    (fset symbol macro)
    macro))
