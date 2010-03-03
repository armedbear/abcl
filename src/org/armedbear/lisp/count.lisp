;;; count.lisp
;;;
;;; Copyright (C) 2003 Peter Graves
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

(in-package "COMMON-LISP")

(require "EXTENSIBLE-SEQUENCES-BASE")

;;; From CMUCL.

(defmacro vector-count-if (not-p from-end-p predicate sequence)
  (let ((next-index (if from-end-p '(1- index) '(1+ index)))
        (pred `(funcall ,predicate (sys::apply-key key (aref ,sequence index)))))
    `(let ((%start ,(if from-end-p '(1- end) 'start))
           (%end ,(if from-end-p '(1- start) 'end)))
       (do ((index %start ,next-index)
            (count 0))
           ((= index %end) count)
         (,(if not-p 'unless 'when) ,pred
           (setq count (1+ count)))))))

(defmacro list-count-if (not-p from-end-p predicate sequence)
  (let ((pred `(funcall ,predicate (sys::apply-key key (pop sequence)))))
    `(let ((%start ,(if from-end-p '(- length end) 'start))
           (%end ,(if from-end-p '(- length start) 'end))
           (sequence ,(if from-end-p '(reverse sequence) 'sequence)))
       (do ((sequence (nthcdr %start ,sequence))
            (index %start (1+ index))
            (count 0))
           ((or (= index %end) (null sequence)) count)
         (,(if not-p 'unless 'when) ,pred
           (setq count (1+ count)))))))

(defun count (item sequence &rest args &key from-end (test #'eql test-p) (test-not nil test-not-p)
		   (start 0) end key)
  (when (and test-p test-not-p)
    (error "test and test-not both supplied"))
  (let* ((length (length sequence))
	 (end (or end length)))
    (let ((%test (if test-not-p
		     (lambda (x)
		       (not (funcall test-not item x)))
		     (lambda (x)
		       (funcall test item x)))))
      (sequence::seq-dispatch sequence
	(if from-end
	    (list-count-if nil t %test sequence)
	    (list-count-if nil nil %test sequence))
	(if from-end
	    (vector-count-if nil t %test sequence)
	    (vector-count-if nil nil %test sequence))
	(apply #'sequence:count item sequence args)))))

(defun count-if (test sequence &rest args &key from-end (start 0) end key)
  (let* ((length (length sequence))
	 (end (or end length)))
    (sequence::seq-dispatch sequence
        (if from-end
            (list-count-if nil t test sequence)
            (list-count-if nil nil test sequence))
        (if from-end
            (vector-count-if nil t test sequence)
            (vector-count-if nil nil test sequence))
	(apply #'sequence:count-if test sequence args))))

(defun count-if-not (test sequence &rest args &key from-end (start 0) end key)
  (let* ((length (length sequence))
	 (end (or end length)))
    (sequence::seq-dispatch sequence
        (if from-end
            (list-count-if t t test sequence)
            (list-count-if t nil test sequence))
        (if from-end
            (vector-count-if t t test sequence)
            (vector-count-if t nil test sequence))
	(apply #'sequence:count-if-not test sequence args))))
