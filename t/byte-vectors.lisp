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

;; nibbles failures


(let* ((unspecialized
         #(2025373960 3099658457 3238582529 148439321
           3099658456 3238582528 3000000000 1000000000
           2000000000 2900000000 2400000000 2800000000
           0 1))
       (array 
         (make-array (length unspecialized)
                     :element-type '(unsigned-byte 32) 
                     :initial-contents unspecialized)))
  (prove:plan (length array))
  (loop :for i :below (length array)
        :doing
           (let ((x0
                   (elt unspecialized i))
                 (x1
                   (elt array i)))
           (prove:ok
            (equal x0 x1)
            (format nil "~a: ~a equals ~a" i x0 x1)))))

(prove:plan 1)
(let*
    ((java-array
       (jnew-array-from-array "byte"
                              #(0 244 2 3)))
     (nio-buffer
       (#"allocate" 'java.nio.ByteBuffer
                    (jarray-length java-array))))
  (#"put" nio-buffer java-array)
  (let ((result
          (make-array 4 :element-type '(unsigned-byte 8)
                        :nio-buffer nio-buffer)))
    (prove:ok
     (equalp result java-array)
     (format nil "~a EQUALP ~a" result java-array))
    (values result java-array)))

  
(prove:finalize)

