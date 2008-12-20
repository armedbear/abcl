;;; macros.lisp
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

(export 'defconst)

(defmacro in-package (name)
  `(%in-package ,(string name)))

(defmacro when (test-form &rest body)
  (if (cdr body)
      `(if ,test-form (progn ,@body))
      `(if ,test-form ,(car body))))

(defmacro unless (test-form &rest body)
  (if (cdr body)
      `(if (not ,test-form) (progn ,@body))
      `(if (not ,test-form) ,(car body))))

(defmacro return (&optional result)
  `(return-from nil ,result))

(defmacro defconstant (name initial-value &optional docstring)
  `(%defconstant ',name ,initial-value ,docstring))

(defmacro defparameter (name initial-value &optional docstring)
  `(%defparameter ',name ,initial-value ,docstring))

(defmacro %car (x)
  `(car (truly-the cons ,x)))

(defmacro %cdr (x)
  `(cdr (truly-the cons ,x)))

(defmacro %cadr (x)
  `(%car (%cdr ,x)))

(defmacro %caddr (x)
  `(%car (%cdr (%cdr ,x))))

(defmacro prog1 (first-form &rest forms)
  (let ((result (gensym)))
    `(let ((,result ,first-form))
       ,@forms
       ,result)))

(defmacro prog2 (first-form second-form &rest forms)
  `(prog1 (progn ,first-form ,second-form) ,@forms))

;; Adapted from SBCL.
(defmacro push (&environment env item place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (cons ,item ,place))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (cons ,g ,getter)))
             ,setter)))))

;; Adapted from SBCL.
(defmacro pushnew (&environment env item place &rest keys)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(setq ,place (adjoin ,item ,place ,@keys))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (let ((g (gensym)))
          `(let* ((,g ,item)
                  ,@(mapcar #'list dummies vals)
                  (,(car newval) (adjoin ,g ,getter ,@keys)))
             ,setter)))))

;; Adapted from SBCL.
(defmacro pop (&environment env place)
  (if (and (symbolp place)
	   (eq place (macroexpand place env)))
      `(prog1 (car ,place)
	      (setq ,place (cdr ,place)))
      (multiple-value-bind (dummies vals newval setter getter)
        (get-setf-expansion place env)
        (do* ((d dummies (cdr d))
              (v vals (cdr v))
              (let-list nil))
             ((null d)
              (push (list (car newval) getter) let-list)
              `(let* ,(nreverse let-list)
                 (prog1 (car ,(car newval))
                        (setq ,(car newval) (cdr ,(car newval)))
                        ,setter)))
          (push (list (car d) (car v)) let-list)))))

(defmacro psetq (&environment env &rest args)
  (do ((l args (cddr l))
       (forms nil)
       (bindings nil))
    ((endp l) (list* 'let* (reverse bindings) (reverse (cons nil forms))))
    (if (and (symbolp (car l))
             (eq (car l) (macroexpand-1 (car l) env)))
        (let ((sym (gensym)))
          (push (list sym (cadr l)) bindings)
          (push (list 'setq (car l) sym) forms))
        (multiple-value-bind
              (dummies vals newval setter getter)
            (get-setf-expansion (macroexpand-1 (car l) env) env)
          (declare (ignore getter))
          (do ((d dummies (cdr d))
               (v vals (cdr v)))
              ((null d))
            (push (list (car d) (car v)) bindings))
          (push (list (car newval) (cadr l)) bindings)
          (push setter forms)))))

(defmacro time (form)
  `(%time #'(lambda () ,form)))

(defmacro with-open-stream (&rest args)
  (let ((var (caar args))
        (stream (cadar args))
        (forms (cdr args))
        (abortp (gensym)))
    `(let ((,var ,stream)
	   (,abortp t))
       (unwind-protect
        (multiple-value-prog1
         (progn ,@forms)
         (setq ,abortp nil))
        (when ,var
          (close ,var :abort ,abortp))))))

(defun ansi-loop (exps)
  (let ((*warn-on-redefinition* nil))
    (require 'loop))
  (fmakunbound 'ansi-loop)
  `(loop ,@exps))

(defmacro loop (&rest exps)
  (dolist (exp exps)
    (when (atom exp)
      (return-from loop (ansi-loop exps))))
  (let ((tag (gensym)))
    `(block nil (tagbody ,tag ,@exps (go ,tag)))))

(defmacro defvar (var &optional (val nil valp) (doc nil docp))
  `(progn
     (%defvar ',var)
     ,@(when valp
         `((unless (boundp ',var)
             (setq ,var ,val))))
     ,@(when docp
         `((%set-documentation ',var 'variable ',doc)))
     ',var))

(defmacro defconst (name value)
  `(defconstant ,name
     (if (boundp ',name)
         (symbol-value ',name)
         ,value)))
