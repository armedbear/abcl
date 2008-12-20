;;; subst.lisp
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

(in-package "SYSTEM")

;;; From CMUCL.

(defmacro satisfies-the-test (item elt)
  (let ((key-tmp (gensym)))
    `(let ((,key-tmp (apply-key key ,elt)))
       (cond (testp (funcall test ,item ,key-tmp))
             (notp (not (funcall test-not ,item ,key-tmp)))
             (t (funcall test ,item ,key-tmp))))))

(defun %subst (new old tree key test testp test-not notp)
  (cond ((satisfies-the-test old tree) new)
        ((atom tree) tree)
        (t (let ((car (%subst new old (car tree) key test testp test-not notp))
                 (cdr (%subst new old (cdr tree) key test testp test-not notp)))
             (if (and (eq car (car tree))
                      (eq cdr (cdr tree)))
                 tree
                 (cons car cdr))))))

(defun subst (new old tree &key key (test #'eql testp) (test-not nil notp))
  (%subst new old tree key test testp test-not notp))

(defun %subst-if (new test tree key)
  (cond ((funcall test (apply-key key tree)) new)
        ((atom tree) tree)
        (t (let ((car (%subst-if new test (car tree) key))
                 (cdr (%subst-if new test (cdr tree) key)))
             (if (and (eq car (car tree))
                      (eq cdr (cdr tree)))
                 tree
                 (cons car cdr))))))

(defun subst-if (new test tree &key key)
  (%subst-if new test tree key))

(defun %subst-if-not (new test tree key)
  (cond ((not (funcall test (apply-key key tree))) new)
        ((atom tree) tree)
        (t (let ((car (%subst-if-not new test (car tree) key))
                 (cdr (%subst-if-not new test (cdr tree) key)))
             (if (and (eq car (car tree))
                      (eq cdr (cdr tree)))
                 tree
                 (cons car cdr))))))

(defun subst-if-not (new test tree &key key)
  (%subst-if-not new test tree key))

(defun nsubst (new old tree &key key (test #'eql testp) (test-not nil notp))
  (labels ((s (subtree)
	      (cond ((satisfies-the-test old subtree) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (cdr subtree)))
                            ((atom subtree)
                             (if (satisfies-the-test old subtree)
                                 (setf (cdr last) new)))
			 (if (satisfies-the-test old subtree)
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
          (s tree)))

(defun nsubst-if (new test tree &key key)
  (labels ((s (subtree)
	      (cond ((funcall test (apply-key key subtree)) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (cdr subtree)))
                            ((atom subtree)
                             (if (funcall test (apply-key key subtree))
                                 (setf (cdr last) new)))
			 (if (funcall test (apply-key key subtree))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
          (s tree)))

(defun nsubst-if-not (new test tree &key key)
  (labels ((s (subtree)
	      (cond ((not (funcall test (apply-key key subtree))) new)
		    ((atom subtree) subtree)
		    (t (do* ((last nil subtree)
			     (subtree subtree (cdr subtree)))
                            ((atom subtree)
                             (if (not (funcall test (apply-key key subtree)))
                                 (setf (cdr last) new)))
			 (if (not (funcall test (apply-key key subtree)))
			     (return (setf (cdr last) new))
			     (setf (car subtree) (s (car subtree)))))
		       subtree))))
          (s tree)))
