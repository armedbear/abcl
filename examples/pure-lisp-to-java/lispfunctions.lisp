;;; lispfunctions.lisp
;;;
;;; Copyright (C) 2008 Ville Voutilainen
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

; When the java class is in CLASSPATH, we can invoke its functions with
; for example jstatic. In this example the Main class is to be found
; in CLASSPATH, so we can just use it without having to load it
; separately.
(defun void-function ()
  (let* ((result (jstatic "addTwoNumbers" "Main" 2 4)))
    (format t "in void-function, result of calling addTwoNumbers(2, 4): ~a~%" result)))

