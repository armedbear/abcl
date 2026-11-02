(in-package #:abcl.test.lisp)

;;; TIME reports the bytes the current thread allocated, where the JVM
;;; counts them, instead of a count of cons cells kept in a static field
;;; that every consing thread wrote to.
(deftest time.bytes-consed
    (let* ((out (with-output-to-string (*trace-output*)
                  (time (make-list 1000))))
           (end (search " bytes consed" out)))
      (and (not (search "cons cell" out))
           (or (null end)              ; a JVM that does not count them
               (let ((start (1+ (or (position #\Newline out :end end :from-end t)
                                    -1))))
                 (>= (parse-integer out :start start :end end)
                     (* 1000 16))))))
  t)
