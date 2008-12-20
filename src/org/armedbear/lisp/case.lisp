;;; case.lisp
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

;;; Is X a (possibly-improper) list of at least N elements?
(defun list-of-length-at-least-p (x n)
  (or (zerop n) ; since anything can be considered an improper list of length 0
      (and (consp x)
	   (list-of-length-at-least-p (cdr x) (1- n)))))

(defun case-body-error (name keyform keyform-value expected-type keys)
  (declare (ignore name keys))
  (restart-case
   (error 'type-error
          :datum keyform-value
          :expected-type expected-type)
   (store-value (value)
                :report (lambda (stream)
                          (format stream "Supply a new value for ~S." keyform))
                :interactive read-evaluated-form
                value)))

;;; CASE-BODY-AUX provides the expansion once CASE-BODY has groveled
;;; all the cases. Note: it is not necessary that the resulting code
;;; signal case-failure conditions, but that's what KMP's prototype
;;; code did. We call CASE-BODY-ERROR, because of how closures are
;;; compiled. RESTART-CASE has forms with closures that the compiler
;;; causes to be generated at the top of any function using the case
;;; macros, regardless of whether they are needed.
;;;
;;; The CASE-BODY-ERROR function is defined later, when the
;;; RESTART-CASE macro has been defined.
(defun case-body-aux (name keyform keyform-value clauses keys
                           errorp proceedp expected-type)
  (if proceedp
      (let ((block (gensym))
	    (again (gensym)))
	`(let ((,keyform-value ,keyform))
	   (block ,block
	     (tagbody
	      ,again
	      (return-from
	       ,block
	       (cond ,@(nreverse clauses)
		     (t
		      (setf ,keyform-value
			    (setf ,keyform
				  (case-body-error
				   ',name ',keyform ,keyform-value
				   ',expected-type ',keys)))
		      (go ,again))))))))
      `(let ((,keyform-value ,keyform))
	 (cond
	  ,@(nreverse clauses)
	  ,@(if errorp
;; 		`((t (error 'case-failure
;; 			    :name ',name
;; 			    :datum ,keyform-value
;; 			    :expected-type ',expected-type
;; 			    :possibilities ',keys))))))))
		`((t (error 'type-error
			    :datum ,keyform-value
			    :expected-type ',expected-type))))))))

;;; CASE-BODY returns code for all the standard "case" macros. NAME is
;;; the macro name, and KEYFORM is the thing to case on. MULTI-P
;;; indicates whether a branch may fire off a list of keys; otherwise,
;;; a key that is a list is interpreted in some way as a single key.
;;; When MULTI-P, TEST is applied to the value of KEYFORM and each key
;;; for a given branch; otherwise, TEST is applied to the value of
;;; KEYFORM and the entire first element, instead of each part, of the
;;; case branch. When ERRORP, no T or OTHERWISE branch is permitted,
;;; and an ERROR form is generated. When PROCEEDP, it is an error to
;;; omit ERRORP, and the ERROR form generated is executed within a
;;; RESTART-CASE allowing KEYFORM to be set and retested.
(defun case-body (name keyform cases multi-p test errorp proceedp needcasesp)
  (unless (or cases (not needcasesp))
    (warn "no clauses in ~S" name))
  (let ((keyform-value (gensym))
	(clauses ())
	(keys ()))
    (do* ((cases cases (cdr cases))
	  (case (car cases) (car cases)))
	 ((null cases) nil)
      (unless (list-of-length-at-least-p case 1)
	(error "~S -- bad clause in ~S" case name))
      (destructuring-bind (keyoid &rest forms) case
	(cond ((and (memq keyoid '(t otherwise))
		    (null (cdr cases)))
	       (if errorp
		   (progn
		     (style-warn "~@<Treating bare ~A in ~A as introducing a ~
                                  normal-clause, not an otherwise-clause~@:>"
				 keyoid name)
		     (push keyoid keys)
		     (push `((,test ,keyform-value ',keyoid) nil ,@forms)
			   clauses))
		   (push `(t nil ,@forms) clauses)))
	      ((and multi-p (listp keyoid))
	       (setf keys (append keyoid keys))
	       (push `((or ,@(mapcar (lambda (key)
				       `(,test ,keyform-value ',key))
				     keyoid))
		       nil
		       ,@forms)
		     clauses))
	      (t
	       (push keyoid keys)
	       (push `((,test ,keyform-value ',keyoid)
		       nil
		       ,@forms)
		     clauses)))))
    (case-body-aux name keyform keyform-value clauses keys errorp proceedp
		   `(,(if multi-p 'member 'or) ,@keys))))

(defmacro case (keyform &body cases)
  "CASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If a singleton key is T then the clause is a default clause."
  (case-body 'case keyform cases t 'eql nil nil nil))

(defmacro ccase (keyform &body cases)
  "CCASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then a correctable error is
  signalled."
  (case-body 'ccase keyform cases t 'eql t t t))

(defmacro ecase (keyform &body cases)
  "ECASE Keyform {({(Key*) | Key} Form*)}*
  Evaluates the Forms in the first clause with a Key EQL to the value of
  Keyform. If none of the keys matches then an error is signalled."
  (case-body 'ecase keyform cases t 'eql t nil t))

(defmacro typecase (keyform &body cases)
  "TYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true."
  (case-body 'typecase keyform cases nil 'typep nil nil nil))

(defmacro ctypecase (keyform &body cases)
  "CTYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then a correctable error is signalled."
  (case-body 'ctypecase keyform cases nil 'typep t t t))

(defmacro etypecase (keyform &body cases)
  "ETYPECASE Keyform {(Type Form*)}*
  Evaluates the Forms in the first clause for which TYPEP of Keyform and Type
  is true. If no form is satisfied then an error is signalled."
  (case-body 'etypecase keyform cases nil 'typep t nil t))
