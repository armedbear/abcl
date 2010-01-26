;;; mop-tests.lisp
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

(in-package #:abcl.test.lisp)

(deftest compute-applicable-methods.foo.1
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(111 222))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'fixnum 'fixnum)))
  t)

(deftest compute-applicable-methods.foo.2
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(x y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'symbol 'symbol)))
  t)

(deftest compute-applicable-methods.foo.3
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(111 y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'fixnum 'symbol)))
  t)

(deftest compute-applicable-methods.foo.4
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(x 111))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'symbol  'fixnum)))
  t)

(deftest compute-applicable-methods.foo.5
    (equalp
     (mop:compute-applicable-methods #'mop-test.foo '(111 "asdf"))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.foo (find-classes 'fixnum  'simple-base-string)))
  t)

(deftest compute-applicable-methods.foo.6
    (equalp (mop:compute-applicable-methods #'mop-test.foo '(111 222))
	    (list (find-foo 'fixnum 'fixnum)
		  (find-foo 'fixnum t)
		  (find-foo t t)))
  t)

(deftest compute-applicable-methods.foo.7
    (equalp (mop:compute-applicable-methods #'mop-test.foo '(111 x))
	    (list (find-foo 'fixnum t)
		  (find-foo t t)))
  t)

(deftest compute-applicable-methods.foo.8
    (equalp (mop:compute-applicable-methods #'mop-test.foo '(x 222))
	    (list (find-foo t t)))
  t)


(deftest compute-applicable-methods.bar.1
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 222))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'fixnum 'fixnum)))
  ;;; Bar with two fixnums might select EQL specializer for second
  ;;; argument.
  nil)

(deftest compute-applicable-methods.bar.1a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 222))
     (list (find-bar 'fixnum 'fixnum)
	   (find-bar 'fixnum t)
	   (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.1b
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 123))
     (list (find-method #'mop-test.bar nil (list (find-class 'fixnum) '(eql 123)))
	   (find-bar 'fixnum 'fixnum)
	   (find-bar 'fixnum t)
	   (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.1c
    (mop:compute-applicable-methods-using-classes
     #'mop-test.bar (find-classes 'fixnum 'fixnum))
  nil
  nil)

(deftest compute-applicable-methods.bar.2
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(x y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'symbol 'symbol)))
  t)

(deftest compute-applicable-methods.bar.2a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(x y))
     (list (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.3
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'fixnum 'symbol)))
  t)

(deftest compute-applicable-methods.bar.3a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 y))
     (list (find-bar 'fixnum t)
	   (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.4
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(x 111))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'symbol  'fixnum)))
  t)

(deftest compute-applicable-methods.bar.4a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(x 111))
     (list (find-bar t t)))
  t)

(deftest compute-applicable-methods.bar.5
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 "asdf"))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.bar (find-classes 'fixnum  'simple-base-string)))
  t)

(deftest compute-applicable-methods.bar.5a
    (equalp
     (mop:compute-applicable-methods #'mop-test.bar '(111 "asdf"))
     (list (find-bar 'fixnum 'string)
	   (find-bar 'fixnum t)
	   (find-bar t t)))
  t)


(deftest compute-applicable-methods.baz.1
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(111 222))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'fixnum 'fixnum)))
  ;; Two fixnum arguments might select EQL specializer for first
  ;; argument.
  nil)

(deftest compute-applicable-methods.baz.1a
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(111 222))
     (list (find-baz 'fixnum 'fixnum)
	   (find-baz 'fixnum t)
	   (find-baz t t)))
  t)

(deftest compute-applicable-methods.baz.1b
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(234 222))
     (list (find-method #'mop-test.baz nil (list '(eql 234) (find-class 'fixnum)))
	   (find-baz 'fixnum 'fixnum)
	   (find-baz 'fixnum t)
	   (find-baz t t)))
  t)

(deftest compute-applicable-methods.baz.1c
    (mop:compute-applicable-methods-using-classes
     #'mop-test.baz (find-classes 'fixnum 'fixnum))
  nil
  nil)

(deftest compute-applicable-methods.baz.2
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(x y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'symbol 'symbol)))
  t)

(deftest compute-applicable-methods.baz.3
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(111 y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'fixnum 'symbol)))
  t)

(deftest compute-applicable-methods.baz.4
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(x 111))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'symbol  'fixnum)))
  t)

(deftest compute-applicable-methods.baz.5
    (equalp
     (mop:compute-applicable-methods #'mop-test.baz '(111 "asdf"))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.baz (find-classes 'fixnum  'simple-base-string)))
  t)


(deftest compute-applicable-methods.quux.1
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 222))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'fixnum 'fixnum)))
  t)

(deftest compute-applicable-methods.quux.1a
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 222))
     (list (find-quux 'fixnum 'fixnum)
	   (find-quux 'fixnum t)
	   (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.2
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(x y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'symbol 'symbol)))
  t)

(deftest compute-applicable-methods.quux.2a
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(x y))
     (list (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.3
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 y))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'fixnum 'symbol)))
  t)

(deftest compute-applicable-methods.quux.3a
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 y))
     (list (find-quux 'fixnum t)
	   (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.4
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(x 111))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'symbol  'fixnum)))
  ;; Symbol/fixnum might trigger EQL spezializer
  nil)

(deftest compute-applicable-methods.quux.4a
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(x 111))
     (list (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.4b
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(:foo 111))
     (list (find-method #'mop-test.quux nil
			(list '(eql :foo) (find-class 'fixnum)))

	   (find-quux t t)))
  t)

(deftest compute-applicable-methods.quux.4c
    (mop:compute-applicable-methods-using-classes
     #'mop-test.quux (find-classes 'symbol 'fixnum))
  nil
  nil)

(deftest compute-applicable-methods.quux.5
    (equalp
     (mop:compute-applicable-methods #'mop-test.quux '(111 "asdf"))
     (mop:compute-applicable-methods-using-classes
      #'mop-test.quux (find-classes 'fixnum  'simple-base-string)))
  t)


