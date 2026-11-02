(in-package #:abcl.test.lisp)

;;; An integer in fixnum range is a fixnum, whichever way it was made.
;;; Bignum.getInstance(BigInteger) used to return a Bignum for every
;;; value, so an integer that came from Java, or from SYS::%FLOAT-BITS,
;;; was neither EQL nor = to the same fixnum; and Bignum.multiplyBy built
;;; its product without normalizing, so (* 2147483648 -1) was a bignum
;;; equal to MOST-NEGATIVE-FIXNUM but not EQL to it.

(defun java-big-integer (string)
  (java:jobject-lisp-value (java:jnew "java.math.BigInteger" string)))

(defun normalized-p (x n)
  "X is EQL to N and a fixnum exactly when N is."
  (and (eql x n) (eq (typep x 'fixnum) (typep n 'fixnum))))

(deftest bignum-normalize.arithmetic
    ;; Every operation on a bignum operand whose result lands in fixnum
    ;; range; B is 2^31 and NB is -2^31-1, the bignums nearest the range.
    (let* ((b (1+ most-positive-fixnum))
           (nb (1- most-negative-fixnum))
           (mnf most-negative-fixnum))
      (remove t
              (list (normalized-p (* b -1) mnf)
                    (normalized-p (* -1 b) mnf)
                    (normalized-p (- b) mnf)
                    (normalized-p (- 0 b) mnf)
                    (normalized-p (/ b -1) mnf)
                    (normalized-p (truncate b -1) mnf)
                    (normalized-p (floor b -1) mnf)
                    (normalized-p (- b 1) most-positive-fixnum)
                    (normalized-p (+ nb 1) mnf)
                    (normalized-p (1+ nb) mnf)
                    (normalized-p (ash b -1) (expt 2 30))
                    (normalized-p (logand b (1- b)) 0)
                    (normalized-p (gcd b 6) 2)
                    (normalized-p (isqrt (* b b)) b)
                    (normalized-p (mod (* b 3) b) 0)
                    (normalized-p (round b 2) (expt 2 30))
                    (normalized-p (/ (* b b) b) b)
                    (normalized-p (numerator (/ b 2)) (expt 2 30)))))
  nil)

(deftest bignum-normalize.arithmetic-compiled
    ;; The compiler emits different calls for literal operands.
    (funcall (compile nil '(lambda ()
                            (let ((mnf most-negative-fixnum))
                              (remove t
                                      (list (normalized-p (* 2147483648 -1) mnf)
                                            (normalized-p (* -1 2147483648) mnf)
                                            (normalized-p (* (1+ most-positive-fixnum) -1) mnf)
                                            (normalized-p (* -1 (1+ most-positive-fixnum)) mnf)))))))
  nil)

(deftest bignum-normalize.from-java
    (loop for n in (list 0 5 -7 most-positive-fixnum most-negative-fixnum
                         (1+ most-positive-fixnum) (1- most-negative-fixnum)
                         (expt 10 30))
          for x = (java-big-integer (princ-to-string n))
          collect (list (eql x n) (= x n)
                        (eq (typep x 'fixnum) (typep n 'fixnum))))
  ((t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t) (t t t)))

(deftest bignum-normalize.lookups
    (let ((x (java-big-integer "5"))
          (table (make-hash-table)))
      (setf (gethash 5 table) :five)
      (list (gethash x table)
            (case x (5 :five) (t :other))
            (member x '(4 5 6))))
  (:five :five (5 6)))

(deftest bignum-normalize.float-bits
    (list (eql (sys::%float-bits 1.0f0) 1065353216)
          (eql (sys::%float-bits -1.0f0) -1082130432)
          (typep (sys::%float-bits 1.0f0) 'fixnum)
          (eql (sys::%float-bits 1d0) 4607182418800017408))
  (t t t t))
