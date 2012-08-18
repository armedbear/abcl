;;; pprint-dispatch.lisp
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

;;; Adapted from the November, 26 1991 version of Richard C. Waters' XP pretty
;;; printer.

;------------------------------------------------------------------------

;Copyright Massachusetts Institute of Technology, Cambridge, Massachusetts.

;Permission to use, copy, modify, and distribute this software and its
;documentation for any purpose and without fee is hereby granted,
;provided that this copyright and permission notice appear in all
;copies and supporting documentation, and that the name of M.I.T. not
;be used in advertising or publicity pertaining to distribution of the
;software without specific, written prior permission. M.I.T. makes no
;representations about the suitability of this software for any
;purpose.  It is provided "as is" without express or implied warranty.

;    M.I.T. DISCLAIMS ALL WARRANTIES WITH REGARD TO THIS SOFTWARE, INCLUDING
;    ALL IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS, IN NO EVENT SHALL
;    M.I.T. BE LIABLE FOR ANY SPECIAL, INDIRECT OR CONSEQUENTIAL DAMAGES OR
;    ANY DAMAGES WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS,
;    WHETHER IN AN ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION,
;    ARISING OUT OF OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS
;    SOFTWARE.

;------------------------------------------------------------------------

(in-package #:xp)

(require "PPRINT")

(defvar *ipd* nil ;see initialization at end of file.
  "initial print dispatch table.")

(defstruct (pprint-dispatch-table (:conc-name nil) (:copier nil))
  (conses-with-cars (make-hash-table :test #'eq) :type hash-table)
  (structures (make-hash-table :test #'eq) :type hash-table)
  (others nil :type list))

;The list and the hash-tables contain entries of the
;following form.  When stored in the hash tables, the test entry is
;the number of entries in the OTHERS list that have a higher priority.

(defstruct (entry (:conc-name nil))
  (test nil)        ;predicate function or count of higher priority others.
  (fn nil)          ;pprint function
  (full-spec nil))  ;list of priority and type specifier

(defun copy-pprint-dispatch (&optional (table *print-pprint-dispatch*))
  (unless table
    (setf table *ipd*))
  (sys::require-type table 'pprint-dispatch-table)
  (let* ((new-conses-with-cars
          (make-hash-table :test #'eq
                           :size (max (hash-table-count (conses-with-cars table)) 32)))
	 (new-structures
          (make-hash-table :test #'eq
                           :size (max (hash-table-count (structures table)) 32))))
    (maphash #'(lambda (key value)
                (setf (gethash key new-conses-with-cars) (copy-entry value)))
	     (conses-with-cars table))
    (maphash #'(lambda (key value)
                (setf (gethash key new-structures) (copy-entry value)))
	     (structures table))
    (make-pprint-dispatch-table
     :conses-with-cars new-conses-with-cars
     :structures new-structures
     :others (copy-list (others table)))))

(defun set-pprint-dispatch (type-specifier function
                                           &optional (priority 0) (table *print-pprint-dispatch*))
  (when (or (not (numberp priority)) (complexp priority))
    (error "invalid PRIORITY argument ~A to SET-PPRINT-DISPATCH" priority))
  (set-pprint-dispatch+ type-specifier function priority table))

(defun set-pprint-dispatch+ (type-specifier function priority table)
  (let* ((category (specifier-category type-specifier))
	 (pred
          (if (not (eq category 'other)) nil
              (let ((pred (specifier-fn type-specifier)))
                (if (and (consp (caddr pred))
                         (symbolp (caaddr pred))
                         (equal (cdaddr pred) '(x)))
                    (symbol-function (caaddr pred))
                    ;;                      (compile nil pred)
                    pred
                    ))))
	 (entry (if function (make-entry :test pred
					 :fn function
					 :full-spec (list priority type-specifier)))))
    (case category
      (cons-with-car
       (cond ((null entry) (remhash (cadadr type-specifier) (conses-with-cars table)))
             (T (setf (test entry)
                      (count-if #'(lambda (e)
                                   (priority-> (car (full-spec e)) priority))
                                (others table)))
                (setf (gethash (cadadr type-specifier) (conses-with-cars table)) entry))))
      (structure-type
       (cond ((null entry) (remhash type-specifier (structures table)))
             (T (setf (test entry)
                      (count-if #'(lambda (e)
                                   (priority-> (car (full-spec e)) priority))
                                (others table)))
                (setf (gethash type-specifier (structures table)) entry))))
      (T ;other
       (let ((old (car (member type-specifier (others table) :test #'equal
                               :key #'(lambda (e) (cadr (full-spec e)))))))
         (when old
           (setf (others table) (delete old (others table)))
           (adjust-counts table (car (full-spec old)) -1)))
       (when entry
         (let ((others (cons nil (others table))))
           (do ((l others (cdr l)))
               ((null (cdr l)) (rplacd l (list entry)))
             (when (priority-> priority (car (full-spec (cadr l))))
               (rplacd l (cons entry (cdr l)))
               (return nil)))
           (setf (others table) (cdr others)))
         (adjust-counts table priority 1)))))
  nil)

(defun priority-> (x y)
  (if (consp x)
      (if (consp y) (> (car x) (car y)) nil)
      (if (consp y) T (> x y))))


(defun adjust-counts (table priority delta)
  (maphash #'(lambda (key value)
              (declare (ignore key))
              (if (priority-> priority (car (full-spec value)))
                  (incf (test value) delta)))
	   (conses-with-cars table))
  (maphash #'(lambda (key value)
              (declare (ignore key))
              (if (priority-> priority (car (full-spec value)))
                  (incf (test value) delta)))
	   (structures table)))

(defun pprint-dispatch (object &optional (table *print-pprint-dispatch*))
  (unless table
    (setf table *ipd*))
  (let ((fn (get-printer object table)))
    (values (or fn #'non-pretty-print) (not (null fn)))))

(defun get-printer (object table)
  (let* ((entry (if (consp object)
		    (gethash (car object) (conses-with-cars table))
		    (gethash (type-of object) (structures table)))))
    (if (not entry)
	(setq entry (find object (others table) :test #'fits))
	(do ((i (test entry) (1- i))
	     (l (others table) (cdr l)))
	    ((zerop i))
	  (when (fits object (car l)) (setq entry (car l)) (return nil))))
    (when entry (fn entry))))

(defun fits (obj entry) (funcall (test entry) obj))

(defun specifier-category (spec)
  (cond ((and (consp spec)
	      (eq (car spec) 'cons)
	      (consp (cdr spec))
	      (null (cddr spec))
	      (consp (cadr spec))
	      (eq (caadr spec) 'member)
	      (consp (cdadr spec))
	      (null (cddadr spec)))
	 'cons-with-car)
	((and (symbolp spec)
;;               (structure-type-p spec)
              (get spec 'structure-printer)
              )
         'structure-type)
	(T 'other)))

(defvar *preds-for-specs*
  '((T always-true) (cons consp) (simple-atom simple-atom-p) (other otherp)
    (null null) (symbol symbolp) (atom atom) (cons consp)
    (list listp) (number numberp) (integer integerp)
    (rational rationalp) (float floatp) (complex complexp)
    (character characterp) (string stringp) (bit-vector bit-vector-p)
    (vector vectorp) (simple-vector simple-vector-p)
    (simple-string simple-string-p) (simple-bit-vector simple-bit-vector-p)
    (array arrayp) (package packagep) (function functionp)
    (compiled-function compiled-function-p) (common commonp)))

(defun always-true (x) (declare (ignore x)) T)

(defun specifier-fn (spec)
  `(lambda (x) ,(convert-body spec)))

(defun convert-body (spec)
  (cond ((atom spec)
	 (let ((pred (cadr (assoc spec *preds-for-specs*))))
	   (if pred `(,pred x) `(typep x ',spec))))
	((member (car spec) '(and or not))
	 (cons (car spec) (mapcar #'convert-body (cdr spec))))
	((eq (car spec) 'member)
	 `(member x ',(copy-list (cdr spec))))
	((eq (car spec) 'cons)
	 `(and (consp x)
	       ,@(if (cdr spec) `((let ((x (car x)))
				    ,(convert-body (cadr spec)))))
	       ,@(if (cddr spec) `((let ((x (cdr x)))
				     ,(convert-body (caddr spec)))))))
	((eq (car spec) 'satisfies)
	 `(funcall (function ,(cadr spec)) x))
        ((eq (car spec) 'eql)
         `(eql x ',(cadr spec)))
	(t
         `(typep x ',(copy-tree spec)))))



(defun function-call-p (x)
  (and (consp x) (symbolp (car x)) (fboundp (car x))))



(setq *ipd* (make-pprint-dispatch-table))

(set-pprint-dispatch+ '(satisfies function-call-p) 'fn-call '(-5) *ipd*)
(set-pprint-dispatch+ 'cons 'pprint-fill '(-10) *ipd*)

(set-pprint-dispatch+ '(cons (member block)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member case)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member catch)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member ccase)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member compiler-let)) 'let-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member cond)) 'cond-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member ctypecase)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member defconstant)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member define-setf-method)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member defmacro)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member define-modify-macro)) 'dmm-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member defparameter)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member defsetf)) 'defsetf-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member define-setf-method)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member defstruct)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member deftype)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member defun)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member defvar)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member do)) 'do-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member do*)) 'do-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member do-all-symbols)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member do-external-symbols)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member do-symbols)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member dolist)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member dotimes)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member ecase)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member etypecase)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member eval-when)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member flet)) 'flet-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member function)) 'function-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member labels)) 'flet-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member lambda)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member let)) 'let-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member let*)) 'let-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member locally)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member loop)) 'pretty-loop '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member macrolet)) 'flet-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member multiple-value-bind)) 'mvb-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member multiple-value-setq)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member prog)) 'prog-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member prog*)) 'prog-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member progv)) 'defun-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member psetf)) 'setq-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member psetq)) 'setq-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member quote)) 'quote-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member return-from)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member setf)) 'setq-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member setq)) 'setq-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member tagbody)) 'tagbody-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member throw)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member typecase)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member unless)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member unwind-protect)) 'up-print '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member when)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member with-input-from-string)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member with-open-file)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member with-open-stream)) 'block-like '(0) *ipd*)
(set-pprint-dispatch+ '(cons (member with-output-to-string)) 'block-like '(0) *ipd*)

(defun pprint-dispatch-print (xp table)
  (let ((stuff (copy-list (others table))))
    (maphash #'(lambda (key val) (declare (ignore key))
                (push val stuff))
	     (conses-with-cars table))
    (maphash #'(lambda (key val) (declare (ignore key))
                (push val stuff))
	     (structures table))
    (setq stuff (sort stuff #'priority-> :key #'(lambda (x) (car (full-spec x)))))
    (pprint-logical-block (xp stuff :prefix "#<" :suffix ">")
                          (format xp (formatter "pprint dispatch table containing ~A entries: ")
                                  (length stuff))
                          (loop (pprint-exit-if-list-exhausted)
                            (let ((entry (pprint-pop)))
                              (format xp (formatter "~{~_P=~4D ~W~} F=~W ")
                                      (full-spec entry) (fn entry)))))))

(setf (get 'pprint-dispatch-table 'structure-printer) #'pprint-dispatch-print)

(set-pprint-dispatch+ 'pprint-dispatch-table #'pprint-dispatch-print '(0) *ipd*)

(setf *print-pprint-dispatch* (copy-pprint-dispatch nil))

(provide "PPRINT-DISPATCH")