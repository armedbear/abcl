(in-package #:abcl.test.lisp)

;;; FLOAT-PRECISION is less than FLOAT-DIGITS for a denormalized float
;;; (CLHS FLOAT-DIGITS); it returned 24 or 53 for every nonzero float.

(deftest float-precision.normalized
    (mapcar #'float-precision
            (list 1.0 -2.5 least-positive-normalized-single-float
                  most-positive-single-float 1d0 least-positive-normalized-double-float))
  (24 24 24 24 53 53))

(deftest float-precision.denormalized
    (mapcar #'float-precision
            (list least-positive-single-float (* 6 least-positive-single-float)
                  (* 3 least-positive-single-float) 5.77921e-40
                  least-positive-double-float 1.870987d-318
                  (- least-positive-double-float)))
  (1 3 2 19 1 19 1))

(deftest float-precision.zero
    (mapcar #'float-precision (list 0.0 -0.0 0d0))
  (0 0 0))

(deftest float-precision.decode-float-denormalized
    ;; DECODE-FLOAT's significand lies in [1/2, 1) for a denormalized
    ;; float as for any other, and the three values rebuild the float.
    (flet ((check (x)
             (multiple-value-bind (m e s) (decode-float x)
               (list (and (<= 1/2 m) (< m 1)) (= x (* s (scale-float m e)))))))
      (mapcar #'check
              (list least-positive-double-float (* 7 least-positive-double-float)
                    (- least-positive-normalized-double-float least-positive-double-float)
                    (- least-positive-double-float)
                    least-positive-single-float (* 7 least-positive-single-float)
                    (- least-positive-normalized-single-float least-positive-single-float)
                    (- least-positive-single-float)
                    1d0 -2.5 least-positive-normalized-double-float)))
  ((t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t) (t t)))

(deftest float-precision.decode-float-agrees-with-integer-decode
    ;; random finite floats of each format, normalized (an exponent
    ;; field from 1 to the one below all ones) and denormalized (zero
    ;; exponent field): the two decodings name the same number
    (flet ((agree (x)
             (multiple-value-bind (m e s) (decode-float x)
               (multiple-value-bind (im ie is) (integer-decode-float x)
                 ;; in rationals: a float factor would round the product
                 (= (* (rational s) (rational m) (expt 2 e)) (* is im (expt 2 ie))))))
           (double (sign exponent fraction)
             (sys::make-double-float (logior (ash sign 63) (ash exponent 52) fraction)))
           (single (sign exponent fraction)
             (sys::make-single-float (logior (ash sign 31) (ash exponent 23) fraction))))
      (list (loop repeat 300 always (agree (double (random 2) (1+ (random 2046)) (random (ash 1 52)))))
            (loop repeat 300 always (agree (single (random 2) (1+ (random 254)) (random (ash 1 23)))))
            (loop repeat 300 always (agree (double (random 2) 0 (1+ (random (1- (ash 1 52)))))))
            (loop repeat 300 always (agree (single (random 2) 0 (1+ (random (1- (ash 1 23)))))))))
  (t t t t))

(deftest float-precision.denormalized-below-digits
    ;; every denormal power of two, single and double
    (list (loop for i from 0 below 23
                for x = (* least-positive-single-float (expt 2 i))
                unless (= (float-precision x) (1+ i)) collect i)
          (loop for i from 0 below 52
                for x = (* least-positive-double-float (expt 2 i))
                unless (= (float-precision x) (1+ i)) collect i))
  (nil nil))
