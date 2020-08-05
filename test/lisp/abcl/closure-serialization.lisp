;;; compiler-tests.lisp
;;;
;;; Copyright (C) 2010 Erik Huelsmann
;;;
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
(in-package #:abcl.test.lisp)
(require '#:java)

(defun f (x)
  (flet ((g (y) (cons x y)))
    (let* ((b (java:jnew "java.io.ByteArrayOutputStream"))
           (o (java:jnew "java.io.ObjectOutputStream" b)))
      (java:jcall "writeObject" o #'g)
      (java:jcall "flush" o)
      (java:jcall "toByteArray" b))))

(deftest serialization-of-closure
    (let* ((b (java:jnew "java.io.ByteArrayInputStream" (f 3)))
    	   (i (java:jnew "java.io.ObjectInputStream" b)))
      (fmakunbound 'f)
      (funcall (java:jcall "readObject" i) T))
  '(3 . T))