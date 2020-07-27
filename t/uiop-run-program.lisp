(in-package :cl-user)

(prove:plan 2)

(let ((program (if (uiop:os-unix-p)
                   "ls"
                   "dir")))
  (prove:is 
   (uiop/run-program:run-program program :output t)
   nil)

  (prove:ok
   (uiop/run-program:run-program program :output :string)))

(prove:finalize)

