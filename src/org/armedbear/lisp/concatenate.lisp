;;; concatenate.lisp
;;;
;;; Copyright (C) 2003-2006 Peter Graves
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

(defun concatenate-to-string (sequences)
  (declare (optimize speed (safety 0)))
  (let ((length 0))
    (declare (type fixnum length))
    (dolist (seq sequences)
      (incf length (length seq)))
    (let ((result (make-string length))
          (i 0))
      (declare (type index i))
      (dolist (seq sequences result)
        (if (stringp seq)
            (dotimes (j (length seq))
              (declare (type index j))
              (setf (schar result i) (char (truly-the string seq) j))
              (incf i))
            (dotimes (j (length seq))
              (declare (type index j))
              (setf (schar result i) (elt seq j))
              (incf i)))))))

;;It uses make-sequence: it should already be user-extensible as-is
(defun concatenate (result-type &rest sequences)
  (case result-type
    (LIST
     (let ((result ()))
       (dolist (seq sequences (nreverse result))
         (dotimes (i (length seq))
           (push (elt seq i) result)))))
    ((STRING SIMPLE-STRING)
     (concatenate-to-string sequences))
    (t
     (let* ((length (apply '+ (mapcar 'length sequences)))
            (result (make-sequence result-type length))
            (i 0))
       (declare (type index i))
       (dolist (seq sequences result)
         (dotimes (j (length seq))
           (setf (elt result i) (elt seq j))
           (incf i)))))))
