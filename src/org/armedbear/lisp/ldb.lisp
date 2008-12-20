;;; ldb.lisp
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

(defun byte (size position)
  (cons size position))

(defun byte-size (bytespec)
  (car bytespec))

(defun byte-position (bytespec)
  (cdr bytespec))

(defun ldb (bytespec integer)
  (logand (ash integer (- (byte-position bytespec)))
          (1- (ash 1 (byte-size bytespec)))))

(defun ldb-test (bytespec integer)
  (not (zerop (ldb bytespec integer))))

(defun dpb (newbyte bytespec integer)
  (let* ((size (byte-size bytespec))
         (position (byte-position bytespec))
         (mask (1- (ash 1 size))))
    (logior (logand integer (lognot (ash mask position)))
	    (ash (logand newbyte mask) position))))

;; From SBCL.
(define-setf-expander ldb (bytespec place &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
    (get-setf-expansion place env)
    (if (and (consp bytespec) (eq (car bytespec) 'byte))
	(let ((n-size (gensym))
	      (n-pos (gensym))
	      (n-new (gensym)))
	  (values (list* n-size n-pos dummies)
		  (list* (second bytespec) (third bytespec) vals)
		  (list n-new)
		  `(let ((,(car newval) (dpb ,n-new (byte ,n-size ,n-pos)
					     ,getter)))
		     ,setter
		     ,n-new)
		  `(ldb (byte ,n-size ,n-pos) ,getter)))
	(let ((btemp (gensym))
	      (gnuval (gensym)))
	  (values (cons btemp dummies)
		  (cons bytespec vals)
		  (list gnuval)
		  `(let ((,(car newval) (dpb ,gnuval ,btemp ,getter)))
		     ,setter
		     ,gnuval)
		  `(ldb ,btemp ,getter))))))

;; Used by the LDB source transform.
(defun %ldb (size position integer)
  (logand (ash integer (- position))
	  (1- (ash 1 size))))

(define-setf-expander %ldb (size position place &environment env)
  (multiple-value-bind (dummies vals newval setter getter)
      (get-setf-expansion place env)
    (let ((n-size (gensym))
          (n-pos (gensym))
          (n-new (gensym)))
      (values (list* n-size n-pos dummies)
              (list* size position vals)
              (list n-new)
              `(let ((,(car newval) (dpb ,n-new (byte ,n-size ,n-pos)
                                         ,getter)))
                 ,setter
                 ,n-new)
              `(ldb (byte ,n-size ,n-pos) ,getter)))))
