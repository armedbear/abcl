;;; fill.lisp
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

(in-package "SYSTEM")

(require "EXTENSIBLE-SEQUENCES-BASE")

;;; Adapted from CMUCL.

(defun list-fill (sequence item start end)
  (do ((current (nthcdr start sequence) (cdr current))
       (index start (1+ index)))
      ((or (atom current) (and end (= index end)))
       sequence)
    (rplaca current item)))

(defun vector-fill (sequence item start end)
  (unless end
    (setf end (length sequence)))
  (do ((index start (1+ index)))
      ((= index end) sequence)
    (setf (aref sequence index) item)))

(defun fill (sequence item &key (start 0) end)
  "Replace the specified elements of SEQUENCE with ITEM."
  (sequence::seq-dispatch sequence
    (list-fill sequence item start end)
    (cond ((and (stringp sequence)
		(zerop start)
		(null end))
	   (simple-string-fill sequence item))
	  (t
	   (vector-fill sequence item start end)))
    (sequence:fill sequence item
		   :start start
		   :end (sequence::%check-generic-sequence-bounds
			 sequence start end))))