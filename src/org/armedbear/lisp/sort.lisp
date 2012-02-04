;;; sort.lisp
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

(require "EXTENSIBLE-SEQUENCES-BASE")

(defun sort (sequence predicate &rest args &key key)
  (sequence::seq-dispatch sequence
    (sort-list sequence predicate key)
    (quicksort sequence 0 (1- (length sequence)) predicate (or key #'identity))
    (apply #'sequence:sort sequence predicate args)))

(defun stable-sort (sequence predicate &rest args &key key)
  (sequence::seq-dispatch sequence
    (sort-list sequence predicate key)
;;; Jorge Tavares: 
;;; As a quick fix, I send in attach a patch that uses in stable-sort merge
;;; sort for all sequences. This is done by coercing the sequence to list,
;;; calling merge sort and coercing it back to the original sequence type.
;;; However, as a long term improvement, the best solution would be to
;;; implement a merge sort for non-list sequences.
    (coerce (sort-list (coerce sequence 'list) 
		       predicate
		       key)
	    (type-of sequence))
    (apply #'sequence:stable-sort sequence predicate args)))

;; Adapted from SBCL.
(declaim (ftype (function (list) cons) last-cons-of))
(defun last-cons-of (list)
  (loop
    (let ((rest (rest list)))
      (if rest
          (setf list rest)
          (return list)))))

;; Adapted from OpenMCL.
(defun merge-lists (list1 list2 pred key)
  (declare (optimize (speed 3) (safety 0)))
  (if (null key)
      (merge-lists-no-key list1 list2 pred)
      (cond ((null list1)
             (values list2 (last-cons-of list2)))
            ((null list2)
             (values list1 (last-cons-of list1)))
            (t
             (let* ((result (cons nil nil))
                    (p result)               ; p points to last cell of result
                    (key1 (funcall key (car list1)))
                    (key2 (funcall key (car list2))))
               (declare (type list p))
               (loop
                 (cond ((funcall pred key2 key1)
                        (rplacd p list2)     ; append the lesser list to last cell of
                        (setf p (cdr p))     ;   result.  Note: test must bo done for
                        (pop list2)          ;   list2 < list1 so merge will be
                        (unless list2        ;   stable for list1
                          (rplacd p list1)
                          (return (values (cdr result) (last-cons-of p))))
                        (setf key2 (funcall key (car list2))))
                       (t
                        (rplacd p list1)
                        (setf p (cdr p))
                        (pop list1)
                        (unless list1
                          (rplacd p list2)
                          (return (values (cdr result) (last-cons-of p))))
                        (setf key1 (funcall key (car list1)))))))))))

(defun merge-lists-no-key (list1 list2 pred)
  (declare (optimize (speed 3) (safety 0)))
  (cond ((null list1)
         (values list2 (last-cons-of list2)))
        ((null list2)
         (values list1 (last-cons-of list1)))
        (t
         (let* ((result (cons nil nil))
                (p result)                   ; p points to last cell of result
                (key1 (car list1))
                (key2 (car list2)))
           (declare (type list p))
           (loop
             (cond ((funcall pred key2 key1)
                    (rplacd p list2)         ; append the lesser list to last cell of
                    (setf p (cdr p))         ;   result.  Note: test must bo done for
                    (pop list2)              ;   list2 < list1 so merge will be
                    (unless list2            ;   stable for list1
                      (rplacd p list1)
                      (return (values (cdr result) (last-cons-of p))))
                    (setf key2 (car list2)))
                   (t
                    (rplacd p list1)
                    (setf p (cdr p))
                    (pop list1)
                    (unless list1
                      (rplacd p list2)
                      (return (values (cdr result) (last-cons-of p))))
                    (setf key1 (car list1)))))))))

;;; SORT-LIST uses a bottom up merge sort.  First a pass is made over
;;; the list grabbing one element at a time and merging it with the next one
;;; form pairs of sorted elements.  Then n is doubled, and elements are taken
;;; in runs of two, merging one run with the next to form quadruples of sorted
;;; elements.  This continues until n is large enough that the inner loop only
;;; runs for one iteration; that is, there are only two runs that can be merged,
;;; the first run starting at the beginning of the list, and the second being
;;; the remaining elements.

(defun sort-list (list pred key)
  (when (or (eq key #'identity) (eq key 'identity))
    (setf key nil))
  (let ((head (cons nil list)) ; head holds on to everything
        (n 1)                  ; bottom-up size of lists to be merged
        unsorted               ; unsorted is the remaining list to be
                               ;   broken into n size lists and merged
        list-1                 ; list-1 is one length n list to be merged
        last                   ; last points to the last visited cell
        )
    (declare (type fixnum n))
    (loop
      ;; start collecting runs of n at the first element
      (setf unsorted (cdr head))
      ;; tack on the first merge of two n-runs to the head holder
      (setf last head)
      (let ((n-1 (1- n)))
        (declare (type fixnum n-1))
        (loop
          (setf list-1 unsorted)
          (let ((temp (nthcdr n-1 list-1))
                list-2)
            (cond (temp
                   ;; there are enough elements for a second run
                   (setf list-2 (cdr temp))
                   (setf (cdr temp) nil)
                   (setf temp (nthcdr n-1 list-2))
                   (cond (temp
                          (setf unsorted (cdr temp))
                          (setf (cdr temp) nil))
                         ;; the second run goes off the end of the list
                         (t (setf unsorted nil)))
                   (multiple-value-bind (merged-head merged-last)
                       (merge-lists list-1 list-2 pred key)
                     (setf (cdr last) merged-head)
                     (setf last merged-last))
                   (if (null unsorted) (return)))
                  ;; if there is only one run, then tack it on to the end
                  (t (setf (cdr last) list-1)
                     (return)))))
        (setf n (+ n n))
        ;; If the inner loop only executed once, then there were only enough
        ;; elements for two runs given n, so all the elements have been merged
        ;; into one list.  This may waste one outer iteration to realize.
        (if (eq list-1 (cdr head))
            (return list-1))))))

#|
<> dc:author "Jorge Tavares" ;
    dc:description 
""" 
The quicksort function picks the pivot by selecting a midpoint and
also sorts the smaller partition first. These are enough to avoid the
stack overflow problem as reported. I've performed some tests and it
looks it is correct 
"""" .
|#
;;;
;;; QUICKSORT
;;; - the pivot is a middle point
;;; - sorts the smaller partition first
;;;
(defun quicksort (vector start end predicate key)
  (declare (type fixnum start end)
	   (type function predicate key))
  (if (< start end)
      (let* ((i start)
	     (j (1+ end))
	     (p (+ start (ash (- end start) -1)))
	     (d (aref vector p))
	     (kd (funcall key d)))
	(rotatef (aref vector p) (aref vector start))
	(block outer-loop
	  (loop
	    (loop 
	      (unless (> (decf j) i) (return-from outer-loop))
	      (when (funcall predicate 
			     (funcall key (aref vector j)) kd)
		(return)))
	    (loop 
	      (unless (< (incf i) j) (return-from outer-loop))
	      (unless (funcall predicate
			       (funcall key (aref vector i)) kd)
		(return)))
	    (rotatef (aref vector i) (aref vector j))))
	(setf (aref vector start) (aref vector j)
	      (aref vector j) d)
	(if (< (- j start) (- end j))
	    (progn
	      (quicksort vector start (1- j) predicate key)
	      (quicksort vector (1+ j) end predicate key))
	    (progn
	      (quicksort vector (1+ j) end predicate key)
	      (quicksort vector start (1- j) predicate key))))
      vector))

;;; DEPRECATED -- to be removed in abcl-1.4
;;; From ECL.
(defun quick-sort (seq start end pred key)
  (unless key (setq key #'identity))
  (if (<= end (1+ start))
      seq
      (let* ((j start) (k end) (d (elt seq start)) (kd (funcall key d)))
        (block outer-loop
          (loop (loop (decf k)
                  (unless (< j k) (return-from outer-loop))
                  (when (funcall pred (funcall key (elt seq k)) kd)
                    (return)))
            (loop (incf j)
              (unless (< j k) (return-from outer-loop))
              (unless (funcall pred (funcall key (elt seq j)) kd)
                (return)))
            (let ((temp (elt seq j)))
              (setf (elt seq j) (elt seq k)
                    (elt seq k) temp))))
        (setf (elt seq start) (elt seq j)
              (elt seq j) d)
        (quick-sort seq start j pred key)
        (quick-sort seq (1+ j) end pred key))))

;;; From ECL. Should already be user-extensible as it does no type dispatch
;;; and uses only user-extensible functions.
(defun merge (result-type sequence1 sequence2 predicate
                          &key key
                          &aux (l1 (length sequence1)) (l2 (length sequence2)))
  (unless key (setq key #'identity))
  (do ((newseq (make-sequence result-type (+ l1 l2)))
       (j 0 (1+ j))
       (i1 0)
       (i2 0))
    ((and (= i1 l1) (= i2 l2)) newseq)
    (cond ((and (< i1 l1) (< i2 l2))
           (cond ((funcall predicate
                           (funcall key (elt sequence1 i1))
                           (funcall key (elt sequence2 i2)))
                  (setf (elt newseq j) (elt sequence1 i1))
                  (incf i1))
                 ((funcall predicate
                           (funcall key (elt sequence2 i2))
                           (funcall key (elt sequence1 i1)))
                  (setf (elt newseq j) (elt sequence2 i2))
                  (incf i2))
                 (t
                  (setf (elt newseq j) (elt sequence1 i1))
                  (incf i1))))
          ((< i1 l1)
           (setf (elt newseq j) (elt sequence1 i1))
           (incf i1))
          (t
           (setf (elt newseq j) (elt sequence2 i2))
           (incf i2)))))
