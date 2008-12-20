;;; lcm.lisp
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

(defun two-arg-lcm (n m)
  (cond ((zerop n) 0)
        ((zerop m) 0)
        (t
         (/ (abs (* n m)) (gcd n m)))))

(defun lcm (&rest integers)
  (unless (every #'integerp integers)
    (error 'type-error :datum (find-if-not #'integerp integers) :expected-type 'integer))
  (case (length integers)
    (0 1)
    (1 (abs (car integers)))
    (2 (two-arg-lcm (car integers) (cadr integers)))
    (t
     (do ((result (car integers) (two-arg-lcm result (car rest)))
          (rest (cdr integers) (cdr rest)))
         ((null rest) result)))))
