(in-package :cl-user)


(let ((element-bits '(8 16 32))
      (length 16))
  (prove:plan (* (length element-bits) 3))
  (dolist (bits element-bits)
    (let* ((type
             `(unsigned-byte ,bits))
           (exclusive-max
             (expt 2 bits))
           (max
             (1- exclusive-max)))
      (prove:ok
       (let ((array (make-array length :element-type type :initial-element max)))
         (typep (aref array 0) type))
       (format nil "Able to make (SIMPLE-ARRAY ~a (~a)) filled with maximum value ~a"
               type length max))
      (prove:is-condition 
       (let ((array (make-array length :element-type type :initial-element -1)))
         (typep (aref array 0) type))
       'type-error
       (format nil "Making a (SIMPLE-ARRAY ~a (~a)) filled with -1 signals a type-error" 
               type length))
      (prove:is-condition 
       (let ((array (make-array 16 :element-type type :initial-element exclusive-max)))
         (typep (aref array 0) type))
       'type-error
       (format nil "Making a (SIMPLE-ARRAY ~a (~a)) filled with ~a signals a type-error" 
               type length exclusive-max)))))

;; pfdietz
(prove:plan 1)
(prove:ok
 (handler-case
     (stable-sort (make-array '(0)) '<)
   (t (e) nil))
 "Able to STABLE-SORT an empty vector.")

(prove:finalize)

