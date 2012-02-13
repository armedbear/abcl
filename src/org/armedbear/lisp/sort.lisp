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

;;;
;;; STABLE SORT
;;;

;;;
;;; MERGE SORT for vectors (and sequences in general)
;;;
;;; - top-down stable merge sort
;;; - it is defined with 2 macros to allow a single algorithm 
;;;   and multiple sequence types: merge-vectors-body and merge-sort-body
;;; - merge-vectors-body merges two given sequences
;;; - merge-sort-body contains the top-down algorithm
;;; - the body macro is called by the merge-sort-vectors functions, 
;;;   which typecases the type of sequence and expands the apropriate body
;;; - more types of sequences/vectors can be added
;;; - the macros generate the merge sort body with or without funcall to key
;;; - the merge-vectors algorithm is inspired from the CCL base code 
;;;

(defmacro merge-vectors-body (type ref a start-a end-a b start-b end-b aux start-aux predicate &optional key)
  (let ((i-a (gensym)) 
	(i-b (gensym))
	(i-aux (gensym))
	(v-a (gensym))
	(v-b (gensym))
	(k-a (gensym))
	(k-b (gensym))
	(merge-block (gensym))) 
    `(locally
	 (declare (type fixnum ,start-a ,end-a ,start-b ,end-b ,start-aux)
		  (type ,type ,a ,b)
		  (type simple-vector ,aux)
		  (type function ,predicate ,@(if key `(,key)))
		  (optimize (speed 3) (safety 0)))
       (block ,merge-block
	  (let ((,i-a ,start-a)
		(,i-b ,start-b)
		(,i-aux ,start-aux)
		,v-a ,v-b ,k-a ,k-b)
	    (declare (type fixnum ,i-a ,i-b ,i-aux))
	    (cond ((= ,start-a ,end-a)
		   (when (= ,start-b ,end-b)
		     (return-from ,merge-block))
		   (setf ,i-a ,start-b
			 ,end-a ,end-b
			 ,a ,b
			 ,v-a (,ref ,a ,i-a)))
		  ((= ,start-b ,end-b)
		   (setf ,i-a ,start-a
			 ,v-a (,ref ,a ,i-a)))
		  (t
		   (setf ,v-a (,ref ,a ,i-a)
			 ,v-b (,ref ,b ,i-b)
			 ,@(if key 
			       `(,k-a (funcall ,key ,v-a))
			       `(,k-a ,v-a))
			 ,@(if key 
			       `(,k-b (funcall ,key ,v-b))
			       `(,k-b ,v-b)))
		   (loop 
		     (if (funcall ,predicate ,k-b ,k-a)
			 (progn 
			   (setf (svref ,aux ,i-aux) ,v-b
				 ,i-aux (+ ,i-aux 1)
				 ,i-b (+ ,i-b 1))
			   (when (= ,i-b ,end-b) (return))
			   (setf ,v-b (,ref ,b ,i-b)
				 ,@(if key 
				       `(,k-b (funcall ,key ,v-b))
				       `(,k-b ,v-b))))
			 (progn 
			   (setf (svref ,aux ,i-aux) ,v-a
				 ,i-aux (+ ,i-aux 1)
				 ,i-a (+ ,i-a 1))
			   (when (= ,i-a ,end-a)
			     (setf ,a ,b 
				   ,i-a ,i-b 
				   ,end-a ,end-b 
				   ,v-a ,v-b)
			     (return))
			   (setf ,v-a (,ref ,a ,i-a)
				 ,@(if key 
				       `(,k-a (funcall ,key ,v-a))
				       `(,k-a ,v-a))))))))
	    (loop
	      (setf (svref ,aux ,i-aux) ,v-a
		    ,i-a (+ ,i-a 1))
	      (when (= ,i-a ,end-a) (return))
	      (setf ,v-a (,ref ,a ,i-a)
		    ,i-aux (+ ,i-aux 1))))))))

(defmacro merge-sort-body (type ref mpredicate mkey msequence mstart mend)
  (let ((merge-sort-call (gensym))
	(maux (gensym))
	(aux (gensym))
	(sequence (gensym))
	(start (gensym))
	(end (gensym))
	(predicate (gensym))
	(key (gensym))
	(mid (gensym))
	(direction (gensym)))
    `(locally
	 (declare (optimize (speed 3) (safety 0)))
       (labels ((,merge-sort-call (,sequence ,start ,end ,predicate ,key ,aux ,direction)
		  (declare (type function ,predicate ,@(if mkey `(,key)))
			   (type fixnum ,start ,end)
			   (type ,type ,sequence))
		  (let ((,mid (+ ,start (ash (- ,end ,start) -1))))
		    (declare (type fixnum ,mid))
		    (if (<= (- ,mid 1) ,start)
			(unless ,direction (setf (,ref ,aux ,start) (,ref ,sequence ,start)))
			(,merge-sort-call ,sequence ,start ,mid ,predicate ,key ,aux (not ,direction)))
		    (if (>= (+ ,mid 1) ,end)
			(unless ,direction (setf (,ref ,aux ,mid) (,ref ,sequence ,mid)))
			(,merge-sort-call ,sequence ,mid ,end ,predicate ,key ,aux (not ,direction)))
		    (unless ,direction (psetq ,sequence ,aux ,aux ,sequence))
		    ,(if mkey
			 `(merge-vectors-body ,type ,ref ,sequence ,start ,mid ,sequence 
					      ,mid ,end ,aux ,start ,predicate ,key)
			 `(merge-vectors-body ,type ,ref ,sequence ,start ,mid ,sequence 
					      ,mid ,end ,aux ,start ,predicate)))))
	 (let ((,maux (make-array ,mend)))
	   (declare (type simple-vector ,maux))
	   (,merge-sort-call ,msequence ,mstart ,mend ,mpredicate ,mkey ,maux nil))))))

(defun merge-sort-vectors (sequence predicate key)
  (let ((end (length sequence)))
    (typecase sequence
      (simple-vector 
       (if key
	   (merge-sort-body simple-vector svref predicate key sequence 0 end)
	   (merge-sort-body simple-vector svref predicate nil sequence 0 end)))
      (vector 
       (if key
	   (merge-sort-body vector aref predicate key sequence 0 end)
	   (merge-sort-body vector aref predicate nil sequence 0 end))))
    sequence))


;;;
;;;  MERGE SORT for lists
;;;

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
;;;
;;; MERGE
;;;

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

;;;
;;; SORT 
;;;

;;;
;;; QUICKSORT
;;;
;;; - algorithm is in the quicksort-body macro, so that it allows
;;;   the use of different types (e.g., simple-vector, vector)
;;; - the pivot is picked by selecting middle point
;;; - sorts the smaller partition first
;;; - the macro generates the quicksort body with or without funcall to key
;;;

(defmacro quicksort-body (type ref mpredicate mkey sequence mstart mend)
  (let ((quicksort-call (gensym))
	(predicate (gensym))
	(key (gensym))
	(vector (gensym))
	(start (gensym))
	(end (gensym))
	(i (gensym))
	(j (gensym))
	(p (gensym))
	(d (gensym))
	(kd (gensym)))
    `(locally 
	 (declare (speed 3) (safety 0))
       (labels ((,quicksort-call (,vector ,start ,end ,predicate ,key)
		   (declare (type function ,predicate ,@(if mkey `(,key)))
			    (type fixnum ,start ,end)
			    (type ,type ,sequence))
		   (if (< ,start ,end)
		       (let* ((,i ,start)
			      (,j (1+ ,end))
			      (,p (the fixnum (+ ,start (ash (- ,end ,start) -1))))
			      (,d (,ref ,vector ,p))
			      ,@(if mkey
				    `((,kd (funcall ,key ,d)))
				    `((,kd ,d))))
			 (rotatef (,ref ,vector ,p) (,ref ,vector ,start))
			 (block outer-loop
			   (loop
			     (loop 
			       (unless (> (decf ,j) ,i) (return-from outer-loop))
			       (when (funcall ,predicate 
					      ,@(if mkey 
						    `((funcall ,key (,ref ,vector ,j)))
						    `((,ref ,vector ,j)))
					      ,kd) (return)))
			     (loop 
			       (unless (< (incf ,i) ,j) (return-from outer-loop))
			       (unless (funcall ,predicate
						,@(if mkey 
						    `((funcall ,key (,ref ,vector ,i)))
						    `((,ref ,vector ,i)))
						,kd) (return)))
			     (rotatef (,ref ,vector ,i) (,ref ,vector ,j))))
			 (setf (,ref ,vector ,start) (,ref ,vector ,j)
			       (,ref ,vector ,j) ,d)
			 (if (< (- ,j ,start) (- ,end ,j))
			     (progn
			       (,quicksort-call ,vector ,start (1- ,j) ,predicate ,key)
			       (,quicksort-call ,vector (1+ ,j) ,end ,predicate ,key))
			     (progn
			       (,quicksort-call ,vector (1+ ,j) ,end ,predicate ,key)
			       (,quicksort-call ,vector ,start (1- ,j) ,predicate ,key)))))))
	 (,quicksort-call ,sequence ,mstart ,mend ,mpredicate ,mkey)))))

(defun quicksort (sequence predicate key)
  (handler-case 
      (let ((end (1- (length sequence))))
        (typecase sequence
          (simple-vector 
           (if key
               (quicksort-body simple-vector svref predicate key sequence 0 end)
               (quicksort-body simple-vector svref predicate nil sequence 0 end)))
          (vector 
           (if key
               (quicksort-body vector aref predicate key sequence 0 end)
               (quicksort-body vector aref predicate nil sequence 0 end))))
        sequence)
    (t (e) 
      (warn "~&New quicksort implementation failed with~&'~A'.~&Trying stable implementation...~&" e)
      (quick-sort sequence 0 (length sequence) predicate key))))

;;; DEPRECATED -- to be removed in abcl-1.4
;;; From ECL.
;;; Alternative implementation for quick-sort SORT
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

;;;
;;; main SORT and STABLE-SORT function calls
;;;
;;; - sort: quicksort and merge sort (only for lists)
;;; - stable-sort: merge sort (all types)
;;;

(defun sort (sequence predicate &rest args &key key)
  (sequence::seq-dispatch sequence
    (sort-list sequence predicate key)
    (quicksort sequence predicate key)
    (apply #'sequence:sort sequence predicate args)))

(defun stable-sort (sequence predicate &rest args &key key)
  (sequence::seq-dispatch sequence
    (sort-list sequence predicate key)
    (merge-sort-vectors sequence predicate key)
    (apply #'sequence:stable-sort sequence predicate args)))
