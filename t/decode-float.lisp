(in-package :cl-user)

;;; <https://github.com/armedbear/abcl/issues/93>
(let ((floats `(1f0
                1f6
                1f-6
                ,least-positive-normalized-single-float
                ,least-positive-single-float)))
  (prove:plan (length floats))
  (dolist (float floats)
    (multiple-value-bind (quotient exponent sign)
        (decode-float float)
      (let ((radix (float-radix float)))
        (let ((lower (/ 1 radix)))
          (prove:ok
           (and (< quotient 1)
                (>= quotient lower))
           (format nil "Whether ~a lies within (1 ~a]" quotient lower)))))))

;;; <https://github.com/armedbear/abcl/issues/94>
(let ((floats `(,least-positive-normalized-double-float
                ,(/ least-positive-normalized-double-float 2)
                ,(/ least-positive-normalized-double-float 4)
                ,(/ least-positive-normalized-double-float 8)
                ,(/ least-positive-normalized-double-float 16)
                ,(/ least-positive-normalized-double-float 1024))))
  (prove:plan (length floats))
  (dolist (float floats)
    (multiple-value-bind (quotient exponent sign)
        (decode-float float)
      (let ((radix (float-radix float)))
        (let ((lower (/ 1 radix)))
          (prove:ok
           (and (< quotient 1)
                (>= quotient lower))
           (format nil "Whether ~a lies within  (1 ~a]" quotient lower)))))))

;;; <https://github.com/armedbear/abcl/issues/95>
(let ((floats `(1d0 ,most-positive-double-float ,least-positive-double-float)))
  (prove:plan (* 2 (length floats)))
  (dolist (float floats)
    (prove:is-type float 'double-float)
    (let ((result (decode-float float)))
      (prove:is-type result 'double-float))))

;;; additional things along the way…

#|
Should fail, as you shouldn't be able to 
(decode-float single-float-positive-infinity)
|# 

(prove:finalize)
