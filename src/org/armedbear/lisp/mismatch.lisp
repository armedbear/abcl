;;; mismatch.lisp
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
;;; MISMATCH (from ECL)

(in-package "COMMON-LISP")

(require "EXTENSIBLE-SEQUENCES-BASE")

(export 'mismatch)

;;; From ECL.

(defun bad-seq-limit (x &optional y)
  (error "bad sequence limit ~a" (if y (list x y) x)))

(defun the-end (x y)
  (cond ((sys::fixnump x)
	 (unless (<= x (length y))
	   (bad-seq-limit x))
	 x)
	((null x)
	 (length y))
	(t (bad-seq-limit x))))

(defun the-start (x)
  (cond ((sys::fixnump x)
	 (unless (>= x 0)
           (bad-seq-limit x))
	 x)
	((null x) 0)
	(t (bad-seq-limit x))))

(defmacro with-start-end (start end seq &body body)
  `(let* ((,start (if ,start (the-start ,start) 0))
          (,end (the-end ,end ,seq)))
     (unless (<= ,start ,end) (bad-seq-limit ,start ,end))
     ,@ body))

(defun call-test (test test-not item keyx)
  (cond (test (funcall test item keyx))
        (test-not (not (funcall test-not item keyx)))
        (t (eql item keyx))))

(defun test-error()
  (error "both test and test are supplied"))

(defun mismatch (sequence1 sequence2 &rest args &key from-end test test-not
		 (key #'identity) start1 start2 end1 end2)
  (and test test-not (test-error))
  (if (and (or (listp sequence1) (arrayp sequence1))
	   (or (listp sequence2) (arrayp sequence2)))
      (with-start-end start1 end1 sequence1
        (with-start-end start2 end2 sequence2
          (if (not from-end)
	      (do ((i1 start1 (1+ i1))
		   (i2 start2 (1+ i2)))
		  ((or (>= i1 end1) (>= i2 end2))
		   (if (and (>= i1 end1) (>= i2 end2)) nil i1))
		(unless (call-test test test-not
				   (funcall key (elt sequence1 i1))
				   (funcall key (elt sequence2 i2)))
		  (return i1)))
	      (do ((i1 (1- end1) (1- i1))
		   (i2 (1- end2)  (1- i2)))
		  ((or (< i1 start1) (< i2 start2))
		   (if (and (< i1 start1) (< i2 start2)) nil (1+ i1)))
		(unless (call-test test test-not
				   (funcall key (elt sequence1 i1))
				   (funcall key (elt sequence2 i2)))
		  (return (1+ i1)))))))
      (apply #'sequence:mismatch sequence1 sequence2 args)))
