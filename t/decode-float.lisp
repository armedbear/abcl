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

;; From the REPL, I don't get a signaled condition, but these tests succeed…
(let ((infinities '(single-float-positive-infinity
                    single-float-negative-infinity
                    double-float-positive-infinity
                    double-float-negative-infinity)))
  (prove:plan (length infinities))
  (dolist (infinity infinities)
    (prove:is-error
     (decode-float infinity)
     'error
     (format nil "Attempting to DECODE-FLOAT ~a should signal error" infinity))))


(let ((floats `(1f0
                1d0
                1f6
                1d6
                1f-6
                1d-6
                ,least-positive-normalized-single-float
                ,least-positive-single-float
                ,least-positive-normalized-double-float
                ,least-positive-double-float)))
  (prove:plan (length floats))
  (dolist (float floats)
    (multiple-value-bind (significand exponent sign)
        (integer-decode-float float)
      (prove:is
       (coerce (* significand (expt 2 exponent)) (type-of float))
       float
       (format nil "INTEGER-DECODE-FLOAT roundtrips for ~s" float)))))


(prove:finalize)
