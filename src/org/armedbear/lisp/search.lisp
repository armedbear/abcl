;;; search.lisp
;;;
;;; Copyright (C) 2003-2004 Peter Graves
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

(in-package "SYSTEM")

(require "EXTENSIBLE-SEQUENCES-BASE")

(export '(simple-search))


;; From CMUCL.

(eval-when (:compile-toplevel :execute)

  (defmacro compare-elements (elt1 elt2)
    `(if test-not
         (if (funcall test-not (apply-key key ,elt1) (apply-key key ,elt2))
             (return nil)
             t)
         (if (not (funcall test (apply-key key ,elt1) (apply-key key ,elt2)))
             (return nil)
             t)))


  (defmacro search-compare-list-list (main sub)
    `(do ((main ,main (cdr main))
          (jndex start1 (1+ jndex))
          (sub (nthcdr start1 ,sub) (cdr sub)))
         ((or (null main) (null sub) (= end1 jndex))
          t)
       (compare-elements (car sub) (car main))))


  (defmacro search-compare-list-vector (main sub)
    `(do ((main ,main (cdr main))
          (index start1 (1+ index)))
         ((or (null main) (= index end1)) t)
       (compare-elements (aref ,sub index) (car main))))


  (defmacro search-compare-vector-list (main sub index)
    `(do ((sub (nthcdr start1 ,sub) (cdr sub))
          (jndex start1 (1+ jndex))
          (index ,index (1+ index)))
         ((or (= end1 jndex) (null sub)) t)
       (compare-elements (car sub) (aref ,main index))))


  (defmacro search-compare-vector-vector (main sub index)
    `(do ((index ,index (1+ index))
          (sub-index start1 (1+ sub-index)))
         ((= sub-index end1) t)
       (compare-elements (aref ,sub sub-index) (aref ,main index))))


  (defmacro search-compare (main-type main sub index)
    (if (eq main-type 'list)
        `(if (listp ,sub)
             (search-compare-list-list ,main ,sub)
             (search-compare-list-vector ,main ,sub))
        `(if (listp ,sub)
             (search-compare-vector-list ,main ,sub ,index)
             (search-compare-vector-vector ,main ,sub ,index))))


  (defmacro list-search (main sub)
    `(do ((main (nthcdr start2 ,main) (cdr main))
          (index2 start2 (1+ index2))
          (terminus (- end2 (- end1 start1)))
          (last-match ()))
         ((> index2 terminus) last-match)
       (if (search-compare list main ,sub index2)
           (if from-end
               (setq last-match index2)
               (return index2)))))


  (defmacro vector-search (main sub)
    `(do ((index2 start2 (1+ index2))
          (terminus (- end2 (- end1 start1)))
          (last-match ()))
         ((> index2 terminus) last-match)
       (if (search-compare vector ,main ,sub index2)
           (if from-end
               (setq last-match index2)
               (return index2)))))

  ) ; eval-when

(defun search (sequence1 sequence2 &rest args &key from-end (test #'eql)
	       test-not (start1 0) end1 (start2 0) end2 key)
  (let ((end1 (or end1 (length sequence1)))
	(end2 (or end2 (length sequence2))))
    (when key
      (setq key (coerce-to-function key)))
    (sequence::seq-dispatch sequence2
      (list-search sequence2 sequence1)
      (vector-search sequence2 sequence1)
      (apply #'sequence:search sequence1 sequence2 args))))

(defun simple-search (sequence1 sequence2)
  (cond ((and (stringp sequence1) (stringp sequence2))
         (simple-string-search sequence1 sequence2))
        ((vectorp sequence2)
         (simple-vector-search sequence1 sequence2))
        (t
         (search sequence1 sequence2 :from-end nil))))
