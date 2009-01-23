;;; misc-tests.lisp
;;;
;;; Copyright (C) 2005 Peter Graves
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

(load (merge-pathnames "test-utilities.lisp" *load-truename*))

(in-package #:test)

(deftest dotimes.1
  (progn
    (fmakunbound 'dotimes.1)
    (defun dotimes.1 ()
      (let ((sum 0)) (dotimes (i 10) (setq i 42) (incf sum i)) sum))
    (dotimes.1))
  420)

(deftest dotimes.1.compiled
  (progn
    (fmakunbound 'dotimes.1.compiled)
    (defun dotimes.1.compiled ()
      (let ((sum 0)) (dotimes (i 10) (setq i 42) (incf sum i)) sum))
    (compile 'dotimes.1.compiled)
    (dotimes.1.compiled))
  420)

(deftest dotimes.2
  (progn
    (fmakunbound 'dotimes.2)
    (defun dotimes.2 (count)
      (let ((sum 0)) (dotimes (i count) (setq i 42) (incf sum i)) sum))
    (dotimes.2 10))
  420)

(deftest dotimes.2.compiled
  (progn
    (fmakunbound 'dotimes.2.compiled)
    (defun dotimes.2.compiled (count)
      (let ((sum 0)) (dotimes (i count) (setq i 42) (incf sum i)) sum))
    (compile 'dotimes.2.compiled)
    (dotimes.2.compiled 10))
  420)

(deftest funcall.1
  (funcall
   (compile nil (lambda (a b c d e f) (list a b c d e f)))
   1 2 3 4 5 6)
  (1 2 3 4 5 6))

(deftest funcall.2
  (funcall
   (compile nil (lambda (a b c d e f g) (list a b c d e f g )))
   1 2 3 4 5 6 7)
  (1 2 3 4 5 6 7))

(deftest funcall.3
  (funcall
   (compile nil (lambda (a b c d e f g h) (list a b c d e f g h)))
   1 2 3 4 5 6 7 8)
  (1 2 3 4 5 6 7 8))

(deftest funcall.4
  (funcall
   (compile nil (lambda (a b c d e f g h i) (list a b c d e f g h i)))
   1 2 3 4 5 6 7 8 9)
  (1 2 3 4 5 6 7 8 9))

(deftest funcall.5
  (funcall
   (compile nil (lambda (a b c d e f g h i j) (list a b c d e f g h i j)))
   1 2 3 4 5 6 7 8 9 10)
  (1 2 3 4 5 6 7 8 9 10))

(deftest copy-list.1
  (eq (copy-list nil) nil)
  t)

(deftest read-from-string.1
  (read-from-string "(1 2 #-abcl #k(3 4))")
  (1 2)
  20)

(deftest read-from-string.2
  (read-from-string "(1 2 #+nil #k(3 4))")
  (1 2)
  19)

(do-tests)
