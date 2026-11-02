(in-package #:abcl.test.lisp)

;;; SCALE-FLOAT multiplied by (expt 2 n) computed as a float, which is
;;; infinity or zero when N alone lies outside the format's exponent
;;; range, although the result is representable.

(deftest scale-float.denormal-up
    ;; the least positive floats scaled by their full exponent are one
    (list (scale-float least-positive-double-float 1074)
          (scale-float least-positive-single-float 149)
          (scale-float (- least-positive-double-float) 1074)
          (scale-float (* 3 least-positive-double-float) 1073))
  (1d0 1.0 -1d0 1.5d0))

(deftest scale-float.large-down
    ;; the most positive floats scaled below the exponent range of 2^n
    (list (= (scale-float most-positive-double-float -1100)
             (* most-positive-double-float (expt 2d0 -550) (expt 2d0 -550)))
          (= (scale-float most-positive-single-float -200)
             (* most-positive-single-float (expt 2.0 -100) (expt 2.0 -100)))
          (scale-float 1d0 -1074)
          (scale-float 1.0 -149))
  (t t 4.9d-324 1.4e-45))

(deftest scale-float.round-trip
    ;; up and down again across both ranges, every power of two
    (list (loop for n from -1074 to 1023
                for x = (scale-float 1d0 n)
                unless (= (scale-float x (- n)) 1d0) collect n)
          (loop for n from -149 to 127
                for x = (scale-float 1.0 n)
                unless (= (scale-float x (- n)) 1.0) collect n))
  (nil nil))

(deftest scale-float.exact
    ;; scaling by 2^n is exact where the result is a normalized float
    (loop repeat 500
          for x = (- (random 2d0) 1d0)
          for n = (- (random 2000) 1000)
          for y = (scale-float x n)
          unless (or (zerop x) (and (< (abs y) most-positive-double-float)
                                    (= (rational y) (* (rational x) (expt 2 n)))))
            collect (list x n y))
  nil)

(deftest scale-float.zero-and-sign
    (list (scale-float 0d0 2000) (scale-float -0d0 -2000) (scale-float 0.0 300)
          (float-sign (scale-float -0d0 -2000)))
  (0d0 -0d0 0.0 -1d0))
