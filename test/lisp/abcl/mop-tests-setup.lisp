;;; mop-tests-setup.lisp
;;;
;;; Copyright (C) 2010 Matthias Hölzl
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

;;; Definitions used by mop-tests.lisp.  Split into a separate file to
;;; avoid problems with the functions not being available during test
;;; runs.

(in-package #:abcl.test.lisp)

(defun find-classes (&rest args)
  (mapcar #'find-class args))

(defgeneric mop-test.foo (x y)
  (:method (x y)
    (list :object x :object y))
  (:method ((x fixnum) y)
    (list :fixnum x :object y))
  (:method ((x fixnum) (y fixnum))
    (list :fixnum x :fixnum y)))

(defun find-foo (&rest specializers)
  (find-method #'mop-test.foo nil
	       (mapcar #'find-class specializers)))

(defgeneric mop-test.bar (x y)
  (:method (x y)
    (list :object x :object y))
  (:method ((x fixnum) y)
    (list :fixnum x :object y))
  (:method ((x fixnum) (y fixnum))
    (list :fixnum x :fixnum y))
  (:method ((x fixnum) (y string))
    (list :fixnum x :fixnum y))
  (:method ((x fixnum) (y (eql 123)))
    (list :fixnum x :123 y)))

(defun find-bar (&rest specializers)
  (find-method #'mop-test.bar nil
	 (mapcar #'find-class specializers)))

(defgeneric mop-test.baz (x y)
  (:method (x y)
    (list :object x :object y))
  (:method ((x fixnum) y)
    (list :fixnum x :object y))
  (:method ((x fixnum) (y fixnum))
    (list :fixnum x :fixnum y))
  (:method ((x (eql 234)) (y fixnum))
    (list :234 x :fixnum y)))

(defun find-baz (&rest specializers)
  (find-method #'mop-test.baz nil
	       (mapcar #'find-class specializers)))

(defgeneric mop-test.quux (x y)
  (:method (x y)
    (list :object x :object y))
  (:method ((x fixnum) y)
    (list :fixnum x :object y))
  (:method ((x fixnum) (y fixnum))
    (list :fixnum x :fixnum y))
  (:method ((x (eql :foo)) (y fixnum))
    (list :foo x :fixnum y)))

(defun find-quux (&rest specializers)
  (find-method #'mop-test.quux nil
	       (mapcar #'find-class specializers)))

(defclass foo-meta-class (standard-class)
  ())

(defclass foo-direct-slot-definition (mop:standard-direct-slot-definition)
  ())

(defclass foo-effective-slot-definition (mop:standard-effective-slot-definition)
  ())

(defmethod mop:direct-slot-definition-class ((class foo-meta-class) &rest initargs)
  (find-class 'foo-direct-slot-definition))

(defmethod mop:effective-slot-definition ((class foo-meta-class) &rest initargs)
  (find-class 'foo-effective-slot-definition))

(defmethod mop:compute-effective-slot-definition ((class foo-meta-class) name direct-slots)
  (car direct-slots))

(defclass bar-class ()
  ((x :initform T))
  (:metaclass foo-meta-class))

(defmethod mop:slot-boundp-using-class ((class foo-meta-class) object slot)
  (error "foo"))
