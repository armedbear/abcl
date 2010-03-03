;;; find.lisp
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

(in-package #:system)

(require "EXTENSIBLE-SEQUENCES-BASE")

;;; From CMUCL.

(defmacro vector-locater-macro (sequence body-form return-type)
  `(let ((incrementer (if from-end -1 1))
	 (start (if from-end (1- (the fixnum end)) start))
	 (end (if from-end (1- (the fixnum start)) end)))
     (declare (fixnum start end incrementer))
     (do ((index start (+ index incrementer))
	  ,@(case return-type (:position nil) (:element '(current))))
	 ((= index end) ())
       (declare (fixnum index))
       ,@(case return-type
	   (:position nil)
	   (:element `((setf current (aref ,sequence index)))))
       ,body-form)))

(defmacro locater-test-not (item sequence seq-type return-type)
  (let ((seq-ref (case return-type
		   (:position
		    (case seq-type
		      (:vector `(aref ,sequence index))
		      (:list `(pop ,sequence))))
		   (:element 'current)))
	(return (case return-type
		  (:position 'index)
		  (:element 'current))))
    `(if test-not
	 (if (not (funcall test-not ,item (sys::apply-key key ,seq-ref)))
	     (return ,return))
	 (if (funcall test ,item (sys::apply-key key ,seq-ref))
	     (return ,return)))))

(defmacro vector-locater (item sequence return-type)
  `(vector-locater-macro ,sequence
			 (locater-test-not ,item ,sequence :vector ,return-type)
			 ,return-type))

(defmacro locater-if-test (test sequence seq-type return-type sense)
  (let ((seq-ref (case return-type
		   (:position
		    (case seq-type
		      (:vector `(aref ,sequence index))
		      (:list `(pop ,sequence))))
		   (:element 'current)))
	(return (case return-type
		  (:position 'index)
		  (:element 'current))))
    (if sense
	`(if (funcall ,test (sys::apply-key key ,seq-ref))
	     (return ,return))
	`(if (not (funcall ,test (sys::apply-key key ,seq-ref)))
	     (return ,return)))))

(defmacro vector-locater-if-macro (test sequence return-type sense)
  `(vector-locater-macro ,sequence
			 (locater-if-test ,test ,sequence :vector ,return-type ,sense)
			 ,return-type))

(defmacro vector-locater-if (test sequence return-type)
  `(vector-locater-if-macro ,test ,sequence ,return-type t))

(defmacro vector-locater-if-not (test sequence return-type)
  `(vector-locater-if-macro ,test ,sequence ,return-type nil))

(defmacro list-locater-macro (sequence body-form return-type)
  `(if from-end
       (do ((sequence (nthcdr (- (the fixnum (length sequence))
				 (the fixnum end))
			      (reverse (the list ,sequence))))
	    (index (1- (the fixnum end)) (1- index))
	    (terminus (1- (the fixnum start)))
	    ,@(case return-type (:position nil) (:element '(current))))
	   ((or (= index terminus) (null sequence)) ())
	 (declare (fixnum index terminus))
	 ,@(case return-type
	     (:position nil)
	     (:element `((setf current (pop ,sequence)))))
	 ,body-form)
       (do ((sequence (nthcdr start ,sequence))
	    (index start (1+ index))
	    ,@(case return-type (:position nil) (:element '(current))))
	   ((or (= index (the fixnum end)) (null sequence)) ())
	 (declare (fixnum index))
	 ,@(case return-type
	     (:position nil)
	     (:element `((setf current (pop ,sequence)))))
	 ,body-form)))

(defmacro list-locater (item sequence return-type)
  `(list-locater-macro ,sequence
		       (locater-test-not ,item ,sequence :list ,return-type)
		       ,return-type))

(defmacro list-locater-if-macro (test sequence return-type sense)
  `(list-locater-macro ,sequence
		       (locater-if-test ,test ,sequence :list ,return-type ,sense)
		       ,return-type))

(defmacro list-locater-if (test sequence return-type)
  `(list-locater-if-macro ,test ,sequence ,return-type t))

(defmacro list-locater-if-not (test sequence return-type)
  `(list-locater-if-macro ,test ,sequence ,return-type nil))

(defmacro vector-position (item sequence)
  `(vector-locater ,item ,sequence :position))

(defmacro list-position (item sequence)
  `(list-locater ,item ,sequence :position))


(defun position (item sequence &rest args &key from-end (test #'eql) test-not
		 (start 0) end key)
  (sequence::seq-dispatch sequence
    (list-position* item sequence from-end test test-not start end key)
    (vector-position* item sequence from-end test test-not start end key)
    (apply #'sequence:position item sequence args)))

(defun list-position* (item sequence from-end test test-not start end key)
  (declare (type fixnum start))
  (let ((end (or end (length sequence))))
    (declare (type fixnum end))
    (list-position item sequence)))

(defun vector-position* (item sequence from-end test test-not start end key)
  (declare (type fixnum start))
  (let ((end (or end (length sequence))))
    (declare (type fixnum end))
    (vector-position item sequence)))

(defmacro vector-position-if (test sequence)
  `(vector-locater-if ,test ,sequence :position))

(defmacro list-position-if (test sequence)
  `(list-locater-if ,test ,sequence :position))

(defun position-if (test sequence &rest args &key from-end (start 0) key end)
  (declare (type fixnum start))
  (let ((end (or end (length sequence))))
    (declare (type fixnum end))
    (sequence::seq-dispatch sequence
      (list-position-if test sequence)
      (vector-position-if test sequence)
      (apply #'sequence:position-if test sequence args))))

(defmacro vector-position-if-not (test sequence)
  `(vector-locater-if-not ,test ,sequence :position))

(defmacro list-position-if-not (test sequence)
  `(list-locater-if-not ,test ,sequence :position))

(defun position-if-not (test sequence &rest args &key from-end (start 0) key end)
  (declare (type fixnum start))
  (let ((end (or end (length sequence))))
    (declare (type fixnum end))
    (sequence::seq-dispatch sequence
      (list-position-if-not test sequence)
      (vector-position-if-not test sequence)
      (apply #'sequence:position-if-not test sequence args))))

(defmacro vector-find (item sequence)
  `(vector-locater ,item ,sequence :element))

(defmacro list-find (item sequence)
  `(list-locater ,item ,sequence :element))

(defun list-find* (item sequence from-end test test-not start end key)
  (declare (type fixnum start end))
  (unless (or test test-not)
    (setf test 'eql))
  (list-find item sequence))

(defun vector-find* (item sequence from-end test test-not start end key)
  (declare (type fixnum start end))
  (unless (or test test-not)
    (setf test 'eql))
  (vector-find item sequence))

(defun find (item sequence &rest args &key from-end (test #'eql) test-not
	     (start 0) end key)
  (let ((end (check-sequence-bounds sequence start end)))
    (sequence::seq-dispatch sequence
      (list-find* item sequence from-end test test-not start end key)
      (vector-find* item sequence from-end test test-not start end key)
      (apply #'sequence:find item sequence args))))

(defmacro vector-find-if (test sequence)
  `(vector-locater-if ,test ,sequence :element))

(defmacro list-find-if (test sequence)
  `(list-locater-if ,test ,sequence :element))

(defun find-if (test sequence &rest args &key from-end (start 0) end key)
  (let ((end (or end (length sequence))))
    (declare (type fixnum end))
    (sequence::seq-dispatch sequence
      (list-find-if test sequence)
      (vector-find-if test sequence)
      (apply #'sequence:find-if test sequence args))))

(defmacro vector-find-if-not (test sequence)
  `(vector-locater-if-not ,test ,sequence :element))

(defmacro list-find-if-not (test sequence)
  `(list-locater-if-not ,test ,sequence :element))

(defun find-if-not (test sequence &rest args &key from-end (start 0) end key)
  (let ((end (or end (length sequence))))
    (declare (type fixnum end))
    (sequence::seq-dispatch sequence
      (list-find-if-not test sequence)
      (vector-find-if-not test sequence)
      (apply #'sequence:find-if-not test sequence args))))
