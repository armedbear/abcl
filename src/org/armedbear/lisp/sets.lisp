;;; sets.lisp
;;;
;;; Copyright (C) 2003-2005 Peter Graves
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

(in-package #:system)

;;; From CMUCL.

(defmacro with-set-keys (funcall)
  `(cond (notp ,(append funcall '(:key key :test-not test-not)))
	 (t ,(append funcall '(:key key :test test)))))

(defun union (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (require-type list2 'list)
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res list2))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall-key key elt) list2))
	(push elt res)))
    res))

(defmacro steve-splice (source destination)
  `(let ((temp ,source))
     (setf ,source (cdr ,source)
	   (cdr temp) ,destination
	   ,destination temp)))

(defun nunion (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res list2)
	(list1 list1))
    (do ()
        ((endp list1))
      (if (not (with-set-keys (member (funcall-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setf list1 (cdr list1))))
    res))


(defun intersection (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res nil))
    (dolist (elt list1)
      (if (with-set-keys (member (funcall-key key elt) list2))
	  (push elt res)))
    res))

(defun nintersection (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (with-set-keys (member (funcall-key key (car list1)) list2))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))

(defun set-difference (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (if (null list2)
      list1
      (let ((res nil))
	(dolist (elt list1)
	  (if (not (with-set-keys (member (funcall-key key elt) list2)))
	      (push elt res)))
	res)))


(defun nset-difference (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((res nil)
	(list1 list1))
    (do () ((endp list1))
      (if (not (with-set-keys (member (funcall-key key (car list1)) list2)))
	  (steve-splice list1 res)
	  (setq list1 (cdr list1))))
    res))


(defun set-exclusive-or (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (when key
    (setq key (coerce-to-function key)))
  (let ((result nil)
        (key (when key (coerce key 'function)))
        (test (coerce test 'function))
        (test-not (if test-not (coerce test-not 'function) #'eql)))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall-key key elt) list2))
	(setq result (cons elt result))))
    (let ((test (if testp
                    (lambda (x y) (funcall test y x))
                    test))
          (test-not (if notp
                        (lambda (x y) (funcall test-not y x))
                        test-not)))
      (dolist (elt list2)
        (unless (with-set-keys (member (funcall-key key elt) list1))
          (setq result (cons elt result)))))
    result))

;;; Adapted from SBCL.
(defun nset-exclusive-or (list1 list2 &key key (test #'eql testp) (test-not #'eql notp))
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (let ((key (and key (coerce-to-function key)))
        (test (if testp (coerce-to-function test) test))
        (test-not (if notp (coerce-to-function test-not) test-not)))
    ;; The outer loop examines LIST1 while the inner loop examines
    ;; LIST2. If an element is found in LIST2 "equal" to the element
    ;; in LIST1, both are spliced out. When the end of LIST1 is
    ;; reached, what is left of LIST2 is tacked onto what is left of
    ;; LIST1. The splicing operation ensures that the correct
    ;; operation is performed depending on whether splice is at the
    ;; top of the list or not.
    (do ((list1 list1)
         (list2 list2)
         (x list1 (cdr x))
         (splicex ())
         (deleted-y ())
         ;; elements of LIST2, which are "equal" to some processed
         ;; earlier elements of LIST1
         )
        ((endp x)
         (if (null splicex)
             (setq list1 list2)
             (rplacd splicex list2))
         list1)
      (let ((key-val-x (apply-key key (car x)))
            (found-duplicate nil))

        ;; Move all elements from LIST2, which are "equal" to (CAR X),
        ;; to DELETED-Y.
        (do* ((y list2 next-y)
              (next-y (cdr y) (cdr y))
              (splicey ()))
             ((endp y))
          (cond ((let ((key-val-y (apply-key key (car y))))
                   (if notp
                       (not (funcall test-not key-val-x key-val-y))
                       (funcall test key-val-x key-val-y)))
                 (if (null splicey)
                     (setq list2 (cdr y))
                     (rplacd splicey (cdr y)))
                 (setq deleted-y (rplacd y deleted-y))
                 (setq found-duplicate t))
                (t (setq splicey y))))

        (unless found-duplicate
          (setq found-duplicate (with-set-keys (member key-val-x deleted-y))))

        (if found-duplicate
            (if (null splicex)
                (setq list1 (cdr x))
                (rplacd splicex (cdr x)))
            (setq splicex x))))))

;;; Adapted from SBCL.
(defun subsetp (list1 list2 &key key (test #'eql testp) (test-not nil notp))
  (require-type list2 'list)
  (when (and testp notp)
    (error "Both :TEST and :TEST-NOT were supplied."))
  (let ((key (and key (coerce-to-function key))))
    (dolist (elt list1)
      (unless (with-set-keys (member (funcall-key key elt) list2))
        (return-from subsetp nil)))
    t))
