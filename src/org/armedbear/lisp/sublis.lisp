;;; sublis.lisp
;;;
;;; Copyright (C) 2003 Peter Graves
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

(in-package "COMMON-LISP")

;;; From CMUCL.

(defun sublis (alist tree &key key (test #'eql) (test-not nil notp))
  (labels ((s (subtree)
              (let* ((key-val (sys::apply-key key subtree))
                     (assoc (if notp
                                (assoc key-val alist :test-not test-not)
                                (assoc key-val alist :test test))))
                (cond (assoc (cdr assoc))
                      ((atom subtree) subtree)
                      (t (let ((car (s (car subtree)))
                               (cdr (s (cdr subtree))))
                           (if (and (eq car (car subtree))
                                    (eq cdr (cdr subtree)))
                               subtree
                               (cons car cdr))))))))
          (s tree)))

(defmacro nsublis-macro ()
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (sys::apply-key key subtree)))
       (if notp
           (assoc ,key-tmp alist :test-not test-not)
           (assoc ,key-tmp alist :test test)))))

(defun nsublis (alist tree &key key (test #'eql) (test-not nil notp))
  (let (temp)
    (labels ((s (subtree)
		(cond ((setq temp (nsublis-macro))
		       (cdr temp))
		      ((atom subtree) subtree)
		      (t (do* ((last nil subtree)
			       (subtree subtree (cdr subtree)))
                              ((atom subtree)
                               (if (setq temp (nsublis-macro))
                                   (setf (cdr last) (cdr temp))))
			   (if (setq temp (nsublis-macro))
			       (return (setf (cdr last) (cdr temp)))
			       (setf (car subtree) (s (car subtree)))))
			 subtree))))
            (s tree))))
