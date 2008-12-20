;;; map1.lisp
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

;;; Adapted from CMUCL.

(in-package #:system)

(defun map1 (function original-arglists accumulate take-car)
  (let* ((arglists (copy-list original-arglists))
	 (ret-list (list nil))
	 (temp ret-list))
    (do ((res nil)
	 (args '() '()))
        ((dolist (x arglists nil) (if (null x) (return t)))
         (if accumulate
             (cdr ret-list)
             (car original-arglists)))
      (do ((l arglists (cdr l)))
          ((null l))
	(push (if take-car (caar l) (car l)) args)
	(setf (car l) (cdar l)))
      (setq res (apply function (nreverse args)))
      (case accumulate
	(:nconc (setq temp (last (nconc temp res))))
	(:list (rplacd temp (list res))
	       (setq temp (cdr temp)))))))

(defun mapcan (function list &rest more-lists)
  (map1 function (cons list more-lists) :nconc t))

(defun mapl (function list &rest more-lists)
  (map1 function (cons list more-lists) nil nil))

(defun maplist (function list &rest more-lists)
  (map1 function (cons list more-lists) :list nil))

(defun mapcon (function list &rest more-lists)
  (map1 function (cons list more-lists) :nconc nil))
