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

(prove:finalize)


 
