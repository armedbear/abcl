;;; byte-io.lisp
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

(in-package #:system)

(defun write-byte (byte stream)
  (declare (type stream stream))
  (let ((element-type (expand-deftype (stream-element-type stream))))
    (require-type byte element-type)
    (let ((width (cadr element-type)))
      (if (= width 8)
          (write-8-bits (the (unsigned-byte 8) byte) stream)
          (let ((bytes ()))
            (dotimes (i (/ width 8))
              (push (logand byte #xff) bytes)
              (setf byte (ash byte -8)))
            (dolist (b bytes)
              (write-8-bits (the (unsigned-byte 8) b) stream)))))
    byte))

(defun read-byte (stream &optional (eof-error-p t) eof-value)
  (declare (type stream stream))
  (let* ((element-type (expand-deftype (stream-element-type stream))))
    (unless element-type
      (if eof-error-p
          (error 'end-of-file :stream stream)
          (return-from read-byte eof-value)))
    (unless (consp element-type)
      (error 'simple-type-error
             :format-control "READ-BYTE: unsupported element type ~S."
             :format-arguments (list element-type)))
    (let ((width (cadr element-type)))
      (if (= width 8)
          (read-8-bits stream eof-error-p eof-value)
          (let ((result 0))
            (dotimes (i (/ width 8))
              (let ((byte (read-8-bits stream eof-error-p eof-value)))
                (when (eq byte eof-value)
                  (return-from read-byte eof-value))
                (setf result (ash result 8))
                (setf result (+ result byte))))
            (if (and (eq (car element-type) 'signed-byte)
                     (not (zerop (logand result (expt 2 (1- width))))))
                (- result (expt 2 width))
                result))))))
