;;;This file only defines the minimum set of symbols and operators
;;;that is needed to make standard CL sequence functions refer to generic
;;;functions in the SEQUENCE package, without actually definining those
;;;generic functions and supporting code, which is in extensible-sequences.lisp.
;;;
;;;The rationale for splitting the code this way is that CLOS depends on
;;;some sequence functions, and if those in turn depend on CLOS we have
;;;a circular dependency.

(in-package :sequence)

(shadow '(ELT LENGTH COUNT "COUNT-IF" "COUNT-IF-NOT"
	  "FIND" "FIND-IF" "FIND-IF-NOT"
	  "POSITION" "POSITION-IF" "POSITION-IF-NOT"
	  "SUBSEQ" "COPY-SEQ" "FILL"
	  "NSUBSTITUTE" "NSUBSTITUTE-IF" "NSUBSTITUTE-IF-NOT"
	  "SUBSTITUTE" "SUBSTITUTE-IF" "SUBSTITUTE-IF-NOT"
	  "REPLACE" "REVERSE" "NREVERSE" "REDUCE"
	  "MISMATCH" "SEARCH"
	  "DELETE" "DELETE-IF" "DELETE-IF-NOT"
	  "REMOVE" "REMOVE-IF" "REMOVE-IF-NOT"
	  "DELETE-DUPLICATES" "REMOVE-DUPLICATES" "SORT" "STABLE-SORT"))

(export '(DOSEQUENCE
	  
	  MAKE-SEQUENCE-ITERATOR MAKE-SIMPLE-SEQUENCE-ITERATOR
	  
	  ITERATOR-STEP ITERATOR-ENDP ITERATOR-ELEMENT
	  ITERATOR-INDEX ITERATOR-COPY
	  
	  WITH-SEQUENCE-ITERATOR WITH-SEQUENCE-ITERATOR-FUNCTIONS
	  
	  CANONIZE-TEST CANONIZE-KEY
	  
	  LENGTH ELT
	  MAKE-SEQUENCE-LIKE ADJUST-SEQUENCE
	   
	  COUNT COUNT-IF COUNT-IF-NOT
	  FIND FIND-IF FIND-IF-NOT
	  POSITION POSITION-IF POSITION-IF-NOT
	  SUBSEQ COPY-SEQ FILL
	  NSUBSTITUTE NSUBSTITUTE-IF NSUBSTITUTE-IF-NOT
	  SUBSTITUTE SUBSTITUTE-IF SUBSTITUTE-IF-NOT
	  REPLACE REVERSE NREVERSE REDUCE
	  MISMATCH SEARCH
	  DELETE DELETE-IF DELETE-IF-NOT
	  REMOVE REMOVE-IF REMOVE-IF-NOT
	  DELETE-DUPLICATES REMOVE-DUPLICATES SORT STABLE-SORT))

;;; Adapted from SBCL
;;; SEQ-DISPATCH does an efficient type-dispatch on the given SEQUENCE.
;;;
;;; FIXME: It might be worth making three cases here, LIST,
;;; SIMPLE-VECTOR, and VECTOR, instead of the current LIST and VECTOR.
;;; It tends to make code run faster but be bigger; some benchmarking
;;; is needed to decide.
(defmacro seq-dispatch
    (sequence list-form array-form &optional other-form)
  `(if (listp ,sequence)
       (let ((,sequence (ext:truly-the list ,sequence)))
         (declare (ignorable ,sequence))
         ,list-form)
       ,@(if other-form
             `((if (arrayp ,sequence)
                   (let ((,sequence (ext:truly-the vector ,sequence)))
                     (declare (ignorable ,sequence))
                     ,array-form)
                   (if (typep ,sequence 'sequence)
		       ,other-form
		       (error 'type-error
			      :datum ,sequence :expected-type 'sequence))))
             `((let ((,sequence (ext:truly-the vector ,sequence)))
                 (declare (ignorable ,sequence))
                 ,array-form)))))

(defun %check-generic-sequence-bounds (seq start end)
  (let ((length (sequence:length seq)))
    (if (<= 0 start (or end length) length)
        (or end length)
        (sequence-bounding-indices-bad-error seq start end))))

(defun sequence-bounding-indices-bad-error (sequence start end)
  (let ((size (length sequence)))
    (error "The bounding indices ~S and ~S are bad for a sequence of length ~S"
	   start end size)))

(defun %set-elt (sequence index value)
  (seq-dispatch sequence
     (sys::%set-elt sequence index value)
     (sys::%set-elt sequence index value)
     (setf (sequence:elt sequence index) value)))

(defsetf cl:elt %set-elt)

#|
    (error 'bounding-indices-bad-error
           :datum (cons start end)
           :expected-type `(cons (integer 0 ,size)
                                 (integer ,start ,size))
           :object sequence)))|#

(provide "EXTENSIBLE-SEQUENCES-BASE")