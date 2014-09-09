;;; define-modify-macro.lisp
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

;; FIXME See section 5.1.3.
(defmacro define-modify-macro (name lambda-list function &optional doc-string)
  "Creates a new read-modify-write macro like PUSH or INCF."
  (let ((other-args nil)
	(rest-arg nil)
	(env (gensym))
	(reference (gensym)))
    ;; Parse out the variable names and &REST arg from the lambda list.
    (do ((ll lambda-list (cdr ll))
	 (arg nil))
	((null ll))
      (setq arg (car ll))
      (cond ((eq arg '&optional))
	    ((eq arg '&rest)
	     (if (symbolp (cadr ll))
		 (setq rest-arg (cadr ll))
		 (error "Non-symbol &REST arg in definition of ~S." name))
	     (if (null (cddr ll))
		 (return nil)
		 (error "Illegal stuff after &REST argument in DEFINE-MODIFY-MACRO.")))
	    ((memq arg '(&key &allow-other-keys &aux))
	     (error "~S not allowed in DEFINE-MODIFY-MACRO lambda list." arg))
	    ((symbolp arg)
	     (push arg other-args))
	    ((and (listp arg) (symbolp (car arg)))
	     (push (car arg) other-args))
	    (t (error "Illegal stuff in DEFINE-MODIFY-MACRO lambda list."))))
    (setq other-args (nreverse other-args))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
      (defmacro ,name (,reference ,@lambda-list &environment ,env)
        ,doc-string
        (multiple-value-bind (dummies vals newval setter getter)
            (get-setf-expansion ,reference ,env)
          (do ((d dummies (cdr d))
               (v vals (cdr v))
               (let-list nil (cons (list (car d) (car v)) let-list)))
              ((null d)
               (push (list (car newval)
                           ,(if rest-arg
                                `(list* ',function getter ,@other-args ,rest-arg)
                                `(list ',function getter ,@other-args)))
                     let-list)
               `(let* ,(nreverse let-list)
                 ,setter))))))))

(define-modify-macro incf-complex (&optional (delta 1)) +
  "The first argument is some location holding a number.  This number is
   incremented by the second argument, DELTA, which defaults to 1.")

(define-modify-macro decf-complex (&optional (delta 1)) -
  "The first argument is some location holding a number.  This number is
   decremented by the second argument, DELTA, which defaults to 1.")

(defmacro incf (place &optional (delta 1))
  (cond ((symbolp place)
         (cond ((constantp delta)
                `(setq ,place (+ ,place ,delta)))
               (t
                ;; See section 5.1.3.
                (let ((temp (gensym)))
                  `(let ((,temp ,delta))
                     (setq ,place (+ ,place ,temp)))))))
        ((and (consp place) (eq (car place) 'THE))
         (let ((res (gensym)))
           `(let ((,res (the ,(second place) (+ ,place ,delta))))
              (setf ,(third place) ,res))))
        (t
         `(incf-complex ,place ,delta))))

(defmacro decf (place &optional (delta 1))
  (cond ((symbolp place)
         (cond ((constantp delta)
                `(setq ,place (- ,place ,delta)))
               (t
                ;; See section 5.1.3.
                (let ((temp (gensym)))
                  `(let ((,temp ,delta))
                     (setq ,place (- ,place ,temp)))))))
        ((and (consp place) (eq (car place) 'THE))
         (let ((res (gensym)))
           `(let ((,res (the ,(second place) (- ,place ,delta))))
              (setf ,(third place) ,res))))
        (t
         `(decf-complex ,place ,delta))))
