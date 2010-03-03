;;; remove.lisp
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

(require "DELETE") ; MUMBLE-DELETE-FROM-END
(require "EXTENSIBLE-SEQUENCES-BASE")

;;; From CMUCL.

(defmacro real-count (count)
  `(cond ((null ,count) most-positive-fixnum)
         ((fixnump ,count) (if (minusp ,count) 0 ,count))
         ((integerp ,count) (if (minusp ,count) 0 most-positive-fixnum))
         (t ,count)))

(defmacro mumble-remove-macro (bump left begin finish right pred)
  `(do ((index ,begin (,bump index))
        (result
         (do ((index ,left (,bump index))
              (result (make-sequence-like sequence length)))
           ((= index ,begin) result)
           (aset result index (aref sequence index))))
        (new-index ,begin)
        (number-zapped 0)
        (this-element))
     ((or (= index ,finish) (= number-zapped count))
      (do ((index index (,bump index))
           (new-index new-index (,bump new-index)))
        ((= index ,right) (shrink-vector result new-index))
        (aset result new-index (aref sequence index))))
     (setq this-element (aref sequence index))
     (cond (,pred (setq number-zapped (1+ number-zapped)))
           (t (aset result new-index this-element)
              (setq new-index (,bump new-index))))))

(defmacro mumble-remove (pred)
  `(mumble-remove-macro 1+ 0 start end length ,pred))

(defmacro mumble-remove-from-end (pred)
  `(let ((sequence (copy-seq sequence)))
     (mumble-delete-from-end ,pred)))

(defmacro normal-mumble-remove ()
  `(mumble-remove
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(defmacro normal-mumble-remove-from-end ()
  `(mumble-remove-from-end
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(defmacro if-mumble-remove ()
  `(mumble-remove (funcall predicate (apply-key key this-element))))

(defmacro if-mumble-remove-from-end ()
  `(mumble-remove-from-end (funcall predicate (apply-key key this-element))))

(defmacro if-not-mumble-remove ()
  `(mumble-remove (not (funcall predicate (apply-key key this-element)))))

(defmacro if-not-mumble-remove-from-end ()
  `(mumble-remove-from-end
    (not (funcall predicate (apply-key key this-element)))))

(defmacro list-remove-macro (pred reverse-p)
  `(let* ((sequence ,(if reverse-p
                         '(reverse sequence)
                         'sequence))
          (%start ,(if reverse-p '(- length end) 'start))
          (%end ,(if reverse-p '(- length start) 'end))
          (splice (list nil))
          (results (do ((index 0 (1+ index))
                        (before-start splice))
                     ((= index %start) before-start)
                     (setq splice
                           (cdr (rplacd splice (list (pop sequence))))))))
     (do ((index %start (1+ index))
          (this-element)
          (number-zapped 0))
       ((or (= index %end) (= number-zapped count))
        (do ((index index (1+ index)))
          ((null sequence)
           ,(if reverse-p
                '(nreverse (cdr results))
                '(cdr results)))
          (setq splice (cdr (rplacd splice (list (pop sequence)))))))
       (setq this-element (pop sequence))
       (if ,pred
           (setq number-zapped (1+ number-zapped))
           (setq splice (cdr (rplacd splice (list this-element))))))))


(defmacro list-remove (pred)
  `(list-remove-macro ,pred nil))

(defmacro list-remove-from-end (pred)
  `(list-remove-macro ,pred t))

(defmacro normal-list-remove ()
  `(list-remove
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(defmacro normal-list-remove-from-end ()
  `(list-remove-from-end
    (if test-not
        (not (funcall test-not item (apply-key key this-element)))
        (funcall test item (apply-key key this-element)))))

(defmacro if-list-remove ()
  `(list-remove
    (funcall predicate (apply-key key this-element))))

(defmacro if-list-remove-from-end ()
  `(list-remove-from-end
    (funcall predicate (apply-key key this-element))))

(defmacro if-not-list-remove ()
  `(list-remove
    (not (funcall predicate (apply-key key this-element)))))

(defmacro if-not-list-remove-from-end ()
  `(list-remove-from-end
    (not (funcall predicate (apply-key key this-element)))))

(defun remove (item sequence &rest args &key from-end (test #'eql) test-not
	       (start 0) end count key)
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (sequence::seq-dispatch sequence
      (if from-end
	  (normal-list-remove-from-end)
	  (normal-list-remove))
      (if from-end
	  (normal-mumble-remove-from-end)
	  (normal-mumble-remove))
      (apply #'sequence:remove item sequence args))))

(defun remove-if (predicate sequence &rest args &key from-end (start 0)
		  end count key)
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (sequence::seq-dispatch sequence
      (if from-end
	  (if-list-remove-from-end)
	  (if-list-remove))
      (if from-end
	  (if-mumble-remove-from-end)
	  (if-mumble-remove))
      (apply #'sequence:remove-if predicate sequence args))))

(defun remove-if-not (predicate sequence &rest args &key from-end (start 0)
		      end count key)
  (let* ((length (length sequence))
	 (end (or end length))
	 (count (real-count count)))
    (sequence::seq-dispatch sequence
      (if from-end
	  (if-not-list-remove-from-end)
	  (if-not-list-remove))
      (if from-end
	  (if-not-mumble-remove-from-end)
	  (if-not-mumble-remove))
      (apply #'sequence:remove-if-not predicate sequence args))))
