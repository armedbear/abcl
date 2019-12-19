(in-package :cl-user)

(prove:plan 1)

(defun two-arg-fn (one two)
 (format t "Two args: ~S and ~S~%" one two))

(prove:ok 
 (let ((fn (compile nil '(lambda ()
                          (two-arg-fn
                           (block test-block
                             (unwind-protect
                                  30
                               (return-from test-block 8)))
                           -1)))))
   (let ((result (funcall fn)))
     (values
      (not result)
      result)))
 (format nil "Checking for successfull compilation of two argument function…"))

(prove:plan 1)
(let* ((file (asdf:system-relative-pathname :abcl "t/eg/progn-compiler-inconsistency.lisp"))
       (fasl (compile-file file)))
  (prove:ok
   (handler-case
       (load fasl)
     (error (e)
       (format *standard-output* "Failed to load fasl: ~a~%" e)
       nil))
   "(ash 1343225879 (- 1)) should compile to a constant"))

(prove:finalize)


 
