;;; do.lisp
;;;
;;; Copyright (C) 2004-2006 Peter Graves <peter@armedbear.org>
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

;;; Adapted from CMUCL.

(in-package "SYSTEM")

(defun do-do-body (varlist endlist decls-and-code bind step name block)
  (let* ((inits ())
	 (steps ())
	 (L1 (gensym))
	 (L2 (gensym)))
    ;; Check for illegal old-style do.
    (when (or (not (listp varlist)) (atom endlist))
      (error "Ill-formed ~S -- possibly illegal old style DO?" name))
    ;; Parse the varlist to get inits and steps.
    (dolist (v varlist)
      (cond ((symbolp v) (push v inits))
	    ((listp v)
	     (unless (symbolp (first v))
	       (error "~S step variable is not a symbol: ~S" name (first v)))
	     (case (length v)
	       (1 (push (first v) inits))
	       (2 (push v inits))
	       (3 (push (list (first v) (second v)) inits)
		  (setq steps (list* (third v) (first v) steps)))
	       (t (error "~S is an illegal form for a ~S varlist." v name))))
	    (t (error "~S is an illegal form for a ~S varlist." v name))))
    ;; Construct the new form.
    (multiple-value-bind (code decls) (parse-body decls-and-code nil)
      `(block ,block
         (,bind ,(nreverse inits)
          ,@decls
          (tagbody
           (go ,L2)
           ,L1
           ,@code
           (,step ,@(nreverse steps))
           ,L2
           (unless ,(car endlist) (go ,L1))
           (return-from ,block (progn ,@(cdr endlist)))))))))

(defmacro do (varlist endlist &rest body)
  (do-do-body varlist endlist body 'let 'psetq 'do nil))

(defmacro do* (varlist endlist &rest body)
  (do-do-body varlist endlist body 'let* 'setq 'do* nil))
