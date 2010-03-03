;;; remove-duplicates.lisp
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

(in-package #:system)

(require "EXTENSIBLE-SEQUENCES-BASE")

;;; Adapted from CMUCL.

(defun list-remove-duplicates (list test test-not start end key from-end)
  (let* ((result (list ()))
	 (splice result)
	 (current list))
    (do ((index 0 (1+ index)))
        ((= index start))
      (setq splice (cdr (rplacd splice (list (car current)))))
      (setq current (cdr current)))
    (do ((index start (1+ index)))
        ((or (and end (= index end))
             (atom current)))
      (if (or (and from-end
		   (not (member (apply-key key (car current))
				(nthcdr (1+ start) result)
				:test test
				:test-not test-not
				:key key)))
	      (and (not from-end)
		   (not (do ((it (apply-key key (car current)))
			     (l (cdr current) (cdr l))
			     (i (1+ index) (1+ i)))
                          ((or (atom l) (and end (= i end)))
                           ())
			  (if (if test-not
				  (not (funcall test-not it (apply-key key (car l))))
				  (funcall test it (apply-key key (car l))))
			      (return t))))))
	  (setq splice (cdr (rplacd splice (list (car current))))))
      (setq current (cdr current)))
    (do ()
        ((atom current))
      (setq splice (cdr (rplacd splice (list (car current)))))
      (setq current (cdr current)))
    (cdr result)))

(defun vector-remove-duplicates (vector test test-not start end key from-end
                                        &optional (length (length vector)))
  (when (null end) (setf end (length vector)))
  (let ((result (make-sequence-like vector length))
	(index 0)
	(jndex start))
    (do ()
        ((= index start))
      (setf (aref result index) (aref vector index))
      (setq index (1+ index)))
    (do ((elt))
      ((= index end))
      (setq elt (aref vector index))
      (unless (or (and from-end
                       (position (apply-key key elt) result :start start
                                 :end jndex :test test :test-not test-not :key key))
		  (and (not from-end)
                       (position (apply-key key elt) vector :start (1+ index)
                                 :end end :test test :test-not test-not :key key)))
	(setf (aref result jndex) elt)
	(setq jndex (1+ jndex)))
      (setq index (1+ index)))
    (do ()
      ((= index length))
      (setf (aref result jndex) (aref vector index))
      (setq index (1+ index))
      (setq jndex (1+ jndex)))
    (shrink-vector result jndex)))

(defun remove-duplicates (sequence &rest args &key (test #'eql) test-not
			  (start 0) from-end end key)
  (sequence::seq-dispatch sequence
    (when sequence
      (if (and (eq test #'eql)
	       (null test-not)
	       (eql start 0)
	       (null from-end)
	       (null end)
	       (null key))
	  (simple-list-remove-duplicates sequence)
	  (list-remove-duplicates sequence test test-not start end key from-end)))
    (vector-remove-duplicates sequence test test-not start end key from-end)
    (apply #'sequence:remove-duplicates sequence args)))
