(in-package :cl-user)

;; make sure we can compile even with non-opstack safe forms
(prove:plan 2)

(prove:ok
 (compile
  nil
  '(lambda (a)
    (ash 1
     (the (unsigned-byte 8)
      (block nil
        (format t "side effect1~%")
        (when a
          (return-from nil a))
        (format t "side effect2~%")
        0)))))
 "Compile opstack unsafe form within ash")

(prove:ok
 (compile
  nil
  '(lambda (a)
    (ash 1
     (bit #*0100
      (catch 'ct7 a)))))
 "Compile opstack unsafe form within ash (original reported failure, github issue #69)")

(prove:finalize)

;; see comments in jvm::p2-ash for which sections of the function each of these tests exercises
(let ((all-tests
        '((ash-fixnum1-pos-constant-shift2 ((lambda (x)
                                              (ash (the (unsigned-byte 29) x) 2))
                                            ((2) => 8)
                                            ((4) => 16)))
          (ash-fixnum1-neg-constant-shift2 ((lambda (x)
                                              (ash (the (unsigned-byte 29) x) -2))
                                            ((2) => 0)
                                            ((4) => 1)
                                            ((0) => 0)))
          (ash-fixnum1-neg-constant-shift-form2 ((lambda (x)
                                                   (ash (the (unsigned-byte 29) x) (- 2)))
                                                 ((2) => 0)
                                                 ((4) => 1)
                                                 ((0) => 0)))
          (ash-fixnum1-zero-shift2 ((lambda (x)
                                      (ash (the (unsigned-byte 29) x) 0))
                                    ((2) => 2)
                                    ((4) => 4)
                                    ((0) => 0)))
          (ash-long1-pos-constant-shift2 ((lambda (x)
                                            (ash (the (unsigned-byte 61) x) 2))
                                          ((2) => 8)
                                          ((4) => 16)))
          (ash-long1-neg-constant-shift2 ((lambda (x)
                                            (ash (the (unsigned-byte 61) x) -2))
                                          ((2) => 0)
                                          ((4) => 1)
                                          ((0) => 0)))
          (ash-long1-neg-constant-shift-form2 ((lambda (x)
                                                 (ash (the (unsigned-byte 61) x) (- 2)))
                                               ((2) => 0)
                                               ((4) => 1)
                                               ((0) => 0)))
          (ash-long1-zero-shift2 ((lambda (x)
                                    (ash (the (unsigned-byte 61) x) 0))
                                  ((2) => 2)
                                  ((4) => 4)
                                  ((0) => 0)))
          (ash-fixnum1-neg-2 ((lambda (x y)
                                (ash (the (unsigned-byte 29) x) (the (integer -5 -1) y)))
                              ((2 -1) => 1)
                              ((4 -2) => 1)
                              ((0 -2) => 0)))
          (ash-long1-pos-fixnum2 ((lambda (x y)
                                    (ash (the (unsigned-byte 40) x) (the (unsigned-byte 4) y)))
                                  ((2 2) => 8)
                                  ((4 2) => 16)))
          (ash-long1-neg-fixnum2 ((lambda (x y)
                                    (ash (the (unsigned-byte 40) x) (the (integer -5 -1) y)))
                                  ((4 -2) => 1)
                                  ((32 -1) => 16)))
          (ash-long1-fixnum2 ((lambda (x y)
                                (ash (the (unsigned-byte 40) x) (the (integer -10 10) y)))
                              ((2 3) => 16)
                              ((4 -2) => 1)
                              ((32 -1) => 16)))
          (ash-regular ((lambda (x y)
                          (ash x y))
                        ((2 3) => 16)
                        ((4 -2) => 1)
                        ((32 -1) => 16)))
          (ash-constant ((lambda ()
                           (ash 2 4))
                         (() => 32)))
          (ash-constant2 ((lambda ()
                            (ash 2 (+ 4)))
                          (() => 32))))))
  (dolist (test-data all-tests)
    (destructuring-bind (test-sym (test-fn &rest tests)) test-data
      (prove:plan (1+ (length tests)))
      (prove:ok
       (compile test-sym test-fn)
       (format nil "Checking for successful compilation of ~S: ~S" test-sym test-fn))
      (dolist (test tests)
        (destructuring-bind (args _ result) test
          (declare (ignore _))
          (prove:is
           (apply test-sym args)
           result
           (format nil "Calling ~S with args ~S should be ~S" test-sym args result))))
      (prove:finalize))))

