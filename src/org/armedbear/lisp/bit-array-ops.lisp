;;; bit-array-ops.lisp
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

(defun bit-array-same-dimensions-p (array1 array2)
  (declare (type (array bit) array1 array2))
  (and (= (array-rank array1)
	  (array-rank array2))
       (dotimes (index (array-rank array1) t)
	 (when (/= (array-dimension array1 index)
		   (array-dimension array2 index))
	   (return nil)))))

(defun require-same-dimensions (array1 array2)
  (unless (bit-array-same-dimensions-p array1 array2)
    (error 'program-error
           "~S and ~S do not have the same dimensions."
           array1 array2)))

(defun pick-result-array (result-bit-array bit-array-1)
  (case result-bit-array
    ((t) bit-array-1)
    ((nil) (make-array (array-dimensions bit-array-1)
		       :element-type 'bit
		       :initial-element 0))
    (t
     (require-same-dimensions bit-array-1 result-bit-array)
     result-bit-array)))

(defun bit-and (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-and bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logand (row-major-aref bit-array-1 i)
                        (row-major-aref bit-array-2 i)))))))

(defun bit-ior (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-ior bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logior (row-major-aref bit-array-1 i)
                        (row-major-aref bit-array-2 i)))))))

(defun bit-xor (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-xor bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logxor (row-major-aref bit-array-1 i)
                        (row-major-aref bit-array-2 i)))))))

(defun bit-eqv (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-eqv bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logand (logeqv (row-major-aref bit-array-1 i)
                                (row-major-aref bit-array-2 i))
                        1))))))

(defun bit-nand (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-nand bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logand (lognand (row-major-aref bit-array-1 i)
                                 (row-major-aref bit-array-2 i))
                        1))))))

(defun bit-nor (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-nor bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logand (lognor (row-major-aref bit-array-1 i)
                                (row-major-aref bit-array-2 i))
                        1))))))

(defun bit-andc1 (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-andc1 bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logand (logandc1 (row-major-aref bit-array-1 i)
                                  (row-major-aref bit-array-2 i))
                        1))))))

(defun bit-andc2 (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-andc2 bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logand (logandc2 (row-major-aref bit-array-1 i)
                                  (row-major-aref bit-array-2 i))
                        1))))))

(defun bit-orc1 (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-orc1 bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logand (logorc1 (row-major-aref bit-array-1 i)
                                 (row-major-aref bit-array-2 i))
                        1))))))

(defun bit-orc2 (bit-array-1 bit-array-2 &optional result-bit-array)
  (require-same-dimensions bit-array-1 bit-array-2)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array-1)))
    (if (and (simple-bit-vector-p bit-array-1)
             (simple-bit-vector-p bit-array-2)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-orc2 bit-array-1 bit-array-2 result-bit-array)
        (dotimes (i (array-total-size result-bit-array) result-bit-array)
          (setf (row-major-aref result-bit-array i)
                (logand (logorc2 (row-major-aref bit-array-1 i)
                                 (row-major-aref bit-array-2 i))
                        1))))))

(defun bit-not (bit-array &optional result-bit-array)
  (let ((result-bit-array (pick-result-array result-bit-array bit-array)))
    (if (and (simple-bit-vector-p bit-array)
             (simple-bit-vector-p result-bit-array))
        (%simple-bit-vector-bit-not bit-array result-bit-array)
    (dotimes (i (array-total-size result-bit-array) result-bit-array)
      (setf (row-major-aref result-bit-array i)
            (logxor (row-major-aref bit-array i) 1))))))
