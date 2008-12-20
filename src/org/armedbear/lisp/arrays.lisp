;;; arrays.lisp
;;;
;;; Copyright (C) 2003-2007 Peter Graves <peter@armedbear.org>
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

(in-package "SYSTEM")

(defconstant array-total-size-limit most-positive-fixnum)
(defconstant array-rank-limit 8)

(defun make-array (dimensions &key
                              (element-type t)
                              (initial-element nil initial-element-p)
                              initial-contents adjustable fill-pointer
                              displaced-to displaced-index-offset)
  (setf element-type (normalize-type element-type))
  (%make-array dimensions element-type initial-element initial-element-p
               initial-contents adjustable fill-pointer displaced-to
               displaced-index-offset))

(defun adjust-array (array new-dimensions
                           &key
                           (element-type (array-element-type array))
                           (initial-element nil initial-element-p)
                           (initial-contents nil initial-contents-p)
                           fill-pointer
                           displaced-to displaced-index-offset)
  (%adjust-array array new-dimensions element-type
                 initial-element initial-element-p
                 initial-contents initial-contents-p
                 fill-pointer displaced-to displaced-index-offset))

(defun array-row-major-index (array &rest subscripts)
  (%array-row-major-index array subscripts))

(defun bit (bit-array &rest subscripts)
  (row-major-aref bit-array (%array-row-major-index bit-array subscripts)))

(defun sbit (simple-bit-array &rest subscripts)
  (row-major-aref simple-bit-array
		  (%array-row-major-index simple-bit-array subscripts)))

(defsetf row-major-aref aset)
(defsetf aref aset)
(defsetf bit aset)
(defsetf sbit aset)

;; (SETF (APPLY #'AREF ...
(defun (setf aref) (new-value array &rest subscripts)
  (aset array (%array-row-major-index array subscripts) new-value))

;; (SETF (APPLY #'BIT ...
(defun (setf bit) (new-value array &rest subscripts)
  (aset array (%array-row-major-index array subscripts) new-value))

;; (SETF (APPLY #'SBIT ...
(defun (setf sbit) (new-value array &rest subscripts)
  (aset array (%array-row-major-index array subscripts) new-value))
