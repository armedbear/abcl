;;; metaclass.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
;;; $Id: misc-tests.lisp 12402 2010-01-26 11:15:48Z mevenson $
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

(defclass testclass1 () ()
  (:metaclass standard-class))
(defclass testclass2 () ()
  (:metaclass standard-class)
  (:documentation "test"))
(defclass metaclass1 (standard-class) ()
  (:metaclass standard-class))
(defclass metaclass2 (standard-class) ()
  (:metaclass standard-class)
  (:documentation "test"))

(defclass testclass3 () ()
  (:metaclass metaclass1)
  (:documentation "test"))

(deftest testclass1.instantiate
    (not (null (make-instance 'testclass1)))
  T)
(deftest testclass2.instantiate
    (not (null (make-instance 'testclass2)))
  T)
(deftest testclass3.instantiate
    (not (null (make-instance 'testclass3)))
  T)

(deftest testclass1.class-of
    (eq (class-of (make-instance 'testclass1)) (find-class 'testclass1))
  T)
(deftest testclass1.metaclass-of
    (eq (class-of (class-of (make-instance 'testclass1)))
        (find-class 'standard-class))
  T)

(deftest testclass3.metaclass-of
    (eq (class-of (class-of (make-instance 'testclass3)))
        (find-class 'metaclass1))
  T)

(deftest standard-class.typep.class
    (typep (class-of (find-class 'standard-class)) 'class)
  T)
(deftest standard-class.typep.standard-class
    (typep (class-of (class-of (find-class 'standard-class))) 'standard-class)
  T)
(deftest metaclass1.typep.class
    (typep (find-class 'metaclass1) 'class)
  T)
(deftest metaclass1.typep.standard-class
    (typep (find-class 'metaclass1) 'standard-class)
  T)
(deftest testclass3.class-of.typep
    (typep (class-of (make-instance 'testclass3)) 'metaclass1)
  T)
(deftest testclass3.metaclass-of.typep
    (typep (class-of (class-of (make-instance 'testclass3))) 'standard-class)
  T)

(defclass testclass4 ()
  ((a :initarg :a :initform 3)
   (b :initarg :b :initform 4))
  (:metaclass metaclass1)
  (:documentation "test"))

(deftest testclass4.init-noargs
    (slot-value (make-instance 'testclass4) 'a)
  3)

(deftest testclass4.initargs
    (slot-value (make-instance 'testclass4 :a 2) 'a)
  2)

(defclass testclass5 ()
  ((a :initarg :a)
   (b :initarg :b :initform 1))
  (:metaclass metaclass1)
  (:default-initargs :a 5))

(deftest testclass5.init-noargs
    (slot-value (make-instance 'testclass5) 'a)
  5)

(deftest testclass5.initargs
    (slot-value (make-instance 'testclass5 :a 3) 'a)
  3)

(defclass testclass6 ()
  ((a :initarg :a :allocation :class))
  (:metaclass metaclass1)
  (:documentation "test"))

(deftest testclass6.1
    (let ((instance1 (make-instance 'testclass6 :a 3))
          (instance2 (make-instance 'testclass6 :a 4)))
      (slot-value instance1 'a))
  4)


