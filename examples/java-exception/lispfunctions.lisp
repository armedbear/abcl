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

; we need to get the
; 1) class (Main)
; 2) classes of the parameters (int)
; 3) method reference (getting that requires the class
; of our object and the classes of the parameters

; After that we can invoke the function with jcall,
; giving the method reference, the object and the parameters.
; The function throws an exception, so we wrap the call in
; handler-case.
(defun void-function (param)
  (let* ((class (jclass "Main"))
	 (intclass (jclass "int"))
	 (method (jmethod class "addTwoNumbers" intclass intclass)))
    (handler-case
	(jcall method param 2 4)
      (java-exception (exception)
	(format t "Caught a java exception in void-function~%")))))

