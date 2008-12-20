;;; assert.lisp
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

;;; Adapted from CMUCL.

(in-package #:system)

(defmacro assert (test-form &optional places datum &rest arguments)
  "Signals an error if the value of test-form is nil.  Continuing from this
   error using the CONTINUE restart will allow the user to alter the value of
   some locations known to SETF and start over with test-form.  Returns nil."
  `(loop
     (when ,test-form (return nil))
     (assert-error ',test-form ',places ,datum ,@arguments)
     ,@(mapcar #'(lambda (place)
                  `(setf ,place (assert-prompt ',place ,place)))
	       places)))

(defun assert-error (assertion places datum &rest arguments)
  (declare (ignore places))
  (let ((c (if datum
               (coerce-to-condition
                datum arguments
                'simple-error 'error)
               (make-condition 'simple-error
                               :format-control "The assertion ~S failed."
                               :format-arguments (list assertion)))))
    (restart-case (error c)
                  (continue ()
                            :report (lambda (stream) (format stream "Retry assertion."))
                            nil))))


(defun assert-prompt (name value)
  (cond ((y-or-n-p "The old value of ~S is ~S.~%Do you want to supply a new value? "
		   name value)
         (fresh-line *query-io*)
	 (format *query-io* "Type a form to be evaluated:~%")
	 (flet ((read-it () (eval (read *query-io*))))
	   (if (symbolp name) ;help user debug lexical variables
	       (progv (list name) (list value) (read-it))
	       (read-it))))
	(t value)))
