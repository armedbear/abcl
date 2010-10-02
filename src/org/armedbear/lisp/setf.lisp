;;; setf.lisp
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

(defun get-setf-method-inverse (form inverse setf-function)
  (let ((new-var (gensym))
        (vars nil)
        (vals nil))
    (dolist (x (cdr form))
      (push (gensym) vars)
      (push x vals))
    (setq vals (nreverse vals))
    (values vars vals (list new-var)
            (if setf-function
                `(,@inverse ,new-var ,@vars)
                (if (functionp (car inverse))
                    `(funcall ,@inverse ,@vars ,new-var)
                    `(,@inverse ,@vars ,new-var)))
            `(,(car form) ,@vars))))

;;; If a macro, expand one level and try again.  If not, go for the
;;; SETF function.
(defun expand-or-get-setf-inverse (form environment)
  (multiple-value-bind (expansion expanded)
      (macroexpand-1 form environment)
    (if expanded
        (get-setf-expansion expansion environment)
        (get-setf-method-inverse form `(funcall #'(setf ,(car form)))
                                 t))))

(defun get-setf-expansion (form &optional environment)
  (when (and (consp form)
             (autoloadp (%car form)))
    (resolve (%car form)))
  (let (temp)
    (cond ((symbolp form)
           (multiple-value-bind (expansion expanded)
               (macroexpand-1 form environment)
             (if expanded
                 (get-setf-expansion expansion environment)
                 (let ((new-var (gensym)))
                   (values nil nil (list new-var)
                           `(setq ,form ,new-var) form)))))
          ((setq temp (get (car form) 'setf-inverse))
           (get-setf-method-inverse form `(,temp) nil))
          ((setq temp (get (car form) 'setf-expander))
           (funcall temp form environment))
          (t
           (expand-or-get-setf-inverse form environment)))))

(defmacro setf (&rest args &environment environment)
  (let ((numargs (length args)))
    (cond
     ((= numargs 2)
      (let ((place (first args))
            (value-form (second args)))
        (if (atom place)
            `(setq ,place ,value-form)
            (progn
              (when (symbolp (%car place))
                (resolve (%car place)))
              (multiple-value-bind (dummies vals store-vars setter getter)
                  (get-setf-expansion place environment)
                (let ((inverse (get (car place) 'setf-inverse)))
                  (if (and inverse (eq inverse (car setter)))
                      (if (functionp inverse)
                          `(funcall ,inverse ,@(cdr place) ,value-form)
                          `(,inverse ,@(cdr place) ,value-form))
                      (if (or (null store-vars) (cdr store-vars))
                          `(let* (,@(mapcar #'list dummies vals))
                             (multiple-value-bind ,store-vars ,value-form
                               ,setter))
                          `(let* (,@(mapcar #'list dummies vals)
                                    ,(list (car store-vars) value-form))
                               ,setter)))))))))
     ((oddp numargs)
      (error "Odd number of arguments to SETF."))
     (t
      (do ((a args (cddr a)) (l nil))
          ((null a) `(progn ,@(nreverse l)))
        (setq l (cons (list 'setf (car a) (cadr a)) l)))))))

;;; Redefined in define-modify-macro.lisp.
(defmacro incf (place &optional (delta 1))
  `(setf ,place (+ ,place ,delta)))

;;; Redefined in define-modify-macro.lisp.
(defmacro decf (place &optional (delta 1))
  `(setf ,place (- ,place ,delta)))

;; (defsetf subseq (sequence start &optional (end nil)) (v)
;;   `(progn (replace ,sequence ,v :start1 ,start :end1 ,end)
;;      ,v))
(defun %set-subseq (sequence start &rest rest)
  (let ((end nil) v)
    (ecase (length rest)
      (1
       (setq v (car rest)))
      (2
       (setq end (car rest)
             v (cadr rest))))
    (progn
      (replace sequence v :start1 start :end1 end)
      v)))

(defun %define-setf-macro (name expander inverse doc)
  (declare (ignore doc)) ; FIXME
  (when inverse
    (put name 'setf-inverse inverse))
  (when expander
    (put name 'setf-expander expander))
  name)

(defmacro defsetf (access-function update-function)
  `(eval-when (:load-toplevel :compile-toplevel :execute)
     (put ',access-function 'setf-inverse ',update-function)))

(defun %set-caar (x v) (set-car (car x) v))
(defun %set-cadr (x v) (set-car (cdr x) v))
(defun %set-cdar (x v) (set-cdr (car x) v))
(defun %set-cddr (x v) (set-cdr (cdr x) v))
(defun %set-caaar (x v) (set-car (caar x) v))
(defun %set-cadar (x v) (set-car (cdar x) v))
(defun %set-cdaar (x v) (set-cdr (caar x) v))
(defun %set-cddar (x v) (set-cdr (cdar x) v))
(defun %set-caadr (x v) (set-car (cadr x) v))
(defun %set-caddr (x v) (set-car (cddr x) v))
(defun %set-cdadr (x v) (set-cdr (cadr x) v))
(defun %set-cdddr (x v) (set-cdr (cddr x) v))
(defun %set-caaaar (x v) (set-car (caaar x) v))
(defun %set-cadaar (x v) (set-car (cdaar x) v))
(defun %set-cdaaar (x v) (set-cdr (caaar x) v))
(defun %set-cddaar (x v) (set-cdr (cdaar x) v))
(defun %set-caadar (x v) (set-car (cadar x) v))
(defun %set-caddar (x v) (set-car (cddar x) v))
(defun %set-cdadar (x v) (set-cdr (cadar x) v))
(defun %set-cdddar (x v) (set-cdr (cddar x) v))
(defun %set-caaadr (x v) (set-car (caadr x) v))
(defun %set-cadadr (x v) (set-car (cdadr x) v))
(defun %set-cdaadr (x v) (set-cdr (caadr x) v))
(defun %set-cddadr (x v) (set-cdr (cdadr x) v))
(defun %set-caaddr (x v) (set-car (caddr x) v))
(defun %set-cadddr (x v) (set-car (cdddr x) v))
(defun %set-cdaddr (x v) (set-cdr (caddr x) v))
(defun %set-cddddr (x v) (set-cdr (cdddr x) v))

(defsetf car set-car)
(defsetf cdr set-cdr)
(defsetf caar %set-caar)
(defsetf cadr %set-cadr)
(defsetf cdar %set-cdar)
(defsetf cddr %set-cddr)
(defsetf caaar %set-caaar)
(defsetf cadar %set-cadar)
(defsetf cdaar %set-cdaar)
(defsetf cddar %set-cddar)
(defsetf caadr %set-caadr)
(defsetf caddr %set-caddr)
(defsetf cdadr %set-cdadr)
(defsetf cdddr %set-cdddr)
(defsetf caaaar %set-caaaar)
(defsetf cadaar %set-cadaar)
(defsetf cdaaar %set-cdaaar)
(defsetf cddaar %set-cddaar)
(defsetf caadar %set-caadar)
(defsetf caddar %set-caddar)
(defsetf cdadar %set-cdadar)
(defsetf cdddar %set-cdddar)
(defsetf caaadr %set-caaadr)
(defsetf cadadr %set-cadadr)
(defsetf cdaadr %set-cdaadr)
(defsetf cddadr %set-cddadr)
(defsetf caaddr %set-caaddr)
(defsetf cadddr %set-cadddr)
(defsetf cdaddr %set-cdaddr)
(defsetf cddddr %set-cddddr)

(defsetf first set-car)
(defsetf second %set-cadr)
(defsetf third %set-caddr)
(defsetf fourth %set-cadddr)
(defun %set-fifth (x v) (set-car (cddddr x) v))
(defsetf fifth %set-fifth)
(defun %set-sixth (x v) (set-car (cdr (cddddr x)) v))
(defsetf sixth %set-sixth)
(defun %set-seventh (x v) (set-car (cddr (cddddr x)) v))
(defsetf seventh %set-seventh)
(defun %set-eighth (x v) (set-car (cdddr (cddddr x)) v))
(defsetf eighth %set-eighth)
(defun %set-ninth (x v) (set-car (cddddr (cddddr x)) v))
(defsetf ninth %set-ninth)
(defun %set-tenth (x v) (set-car (cdr (cddddr (cddddr x))) v))
(defsetf tenth %set-tenth)

(defsetf rest set-cdr)
;;Redefined in extensible-sequences-base.lisp
(defsetf elt %set-elt)
(defsetf nth %set-nth)
(defsetf svref svset)
(defsetf fill-pointer %set-fill-pointer)
(defsetf subseq %set-subseq)
(defsetf symbol-value set)
(defsetf symbol-function %set-symbol-function)
(defsetf symbol-plist %set-symbol-plist)
(defsetf get put)
(defsetf gethash puthash)
(defsetf char set-char)
(defsetf schar set-schar)
(defsetf logical-pathname-translations %set-logical-pathname-translations)
(defsetf readtable-case %set-readtable-case)

(defsetf function-info %set-function-info)

(defsetf stream-external-format %set-stream-external-format)

(defsetf structure-ref structure-set)
