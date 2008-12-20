;;; parse-integer.lisp
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

;;; From OpenMCL.

(defun parse-integer-error (string)
  (error 'parse-error "not an integer string: ~S" string))

(defun parse-integer (string &key (start 0) end
                             (radix 10) junk-allowed)
  (when (null end)
    (setq end (length string)))
  (let ((index (do ((i start (1+ i)))
                   ((= i end)
                    (if junk-allowed
                        (return-from parse-integer (values nil end))
                        (parse-integer-error string)))
                 (unless (whitespacep (char string i)) (return i))))
        (minusp nil)
        (found-digit nil)
        (result 0))
    (let ((char (char string index)))
      (cond ((char= char #\-)
             (setq minusp t)
             (setq index (1+ index)))
            ((char= char #\+)
             (setq index (1+ index)))))
    (loop
      (when (= index end) (return nil))
      (let* ((char (char string index))
             (weight (digit-char-p char radix)))
        (cond (weight
               (setq result (+ weight (* result radix))
                     found-digit t))
              (junk-allowed (return nil))
              ((whitespacep char)
               (do () ((= (setq index (1+ index)) end))
                 (unless (whitespacep (char string index))
                   (parse-integer-error string)))
               (return nil))
              (t
               (parse-integer-error string))))
      (setq index (1+ index)))
    (values
     (if found-digit
         (if minusp (- result) result)
         (if junk-allowed
             nil
             (parse-integer-error string)))
     index)))
