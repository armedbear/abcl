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

; param comes from java, so accessing it require
; calling jobject-lisp-value on it
(defun void-function (param)
  (format t "in void-function, param: ~a~%" (jobject-lisp-value param)))

; params come from java, so accessing them require
; calling jobject-lisp-value on them
(defun int-function (jparam1 jparam2)
  (let* ((param1 (jobject-lisp-value jparam1))
	 (param2 (jobject-lisp-value jparam2))
	 (result (+ param1 param2)))
    (format t "in int-function, params: ~a ~a~%result: ~a~%" 
	    param1 param2 result) 
    result))