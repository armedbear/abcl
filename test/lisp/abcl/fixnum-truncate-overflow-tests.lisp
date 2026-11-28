(in-package #:abcl.test.lisp)

;;; MOST-NEGATIVE-FIXNUM divided by -1 is the one quotient of two fixnums
;;; that is not a fixnum.  Fixnum.truncate divided in int arithmetic,
;;; which wraps, so TRUNCATE, FLOOR, CEILING and ROUND of it returned
;;; MOST-NEGATIVE-FIXNUM again.

(deftest fixnum-truncate-overflow.all-ways
    (let* ((n most-negative-fixnum)
           (want (list (- n) 0))
           (compiled (compile nil '(lambda (n d)
                                    (list (multiple-value-list (truncate n d))
                                          (multiple-value-list (floor n d))
                                          (multiple-value-list (ceiling n d))
                                          (multiple-value-list (round n d))))))
           (declared (compile nil '(lambda (n d)
                                    (declare (type (signed-byte 32) n d))
                                    (list (multiple-value-list (truncate n d))
                                          (multiple-value-list (floor n d))
                                          (multiple-value-list (ceiling n d))
                                          (multiple-value-list (round n d))))))
           (literal (compile nil '(lambda (n)
                                   (list (multiple-value-list (truncate n -1))
                                         (multiple-value-list (floor n -1))
                                         (multiple-value-list (ceiling n -1))
                                         (multiple-value-list (round n -1)))))))
      (list (list (multiple-value-list (funcall #'truncate n -1))
                  (multiple-value-list (funcall #'floor n -1))
                  (multiple-value-list (funcall #'ceiling n -1))
                  (multiple-value-list (funcall #'round n -1)))
            (funcall compiled n -1)
            (funcall declared n -1)
            (funcall literal n)
            want))
  (((2147483648 0) (2147483648 0) (2147483648 0) (2147483648 0))
   ((2147483648 0) (2147483648 0) (2147483648 0) (2147483648 0))
   ((2147483648 0) (2147483648 0) (2147483648 0) (2147483648 0))
   ((2147483648 0) (2147483648 0) (2147483648 0) (2147483648 0))
   (2147483648 0)))

(deftest fixnum-truncate-overflow.neighbors
    ;; the pairs beside it are unchanged
    (list (multiple-value-list (truncate most-negative-fixnum 1))
          (multiple-value-list (truncate (1+ most-negative-fixnum) -1))
          (multiple-value-list (truncate most-positive-fixnum -1))
          (multiple-value-list (truncate most-negative-fixnum 2))
          (multiple-value-list (truncate most-negative-fixnum -2))
          (multiple-value-list (rem most-negative-fixnum -1))
          (multiple-value-list (mod most-negative-fixnum -1))
          (typep (truncate most-negative-fixnum -1) 'bignum))
  ((-2147483648 0) (2147483647 0) (-2147483647 0) (-1073741824 0) (1073741824 0)
   (0) (0) t))
