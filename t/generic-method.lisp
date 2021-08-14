(prove:plan 1)

;;; <https://abcl.org/trac/ticket/485>
(prove:ok
 (handler-case 
     (progn
       (in-package :cl-user)

       (defmethod test ((a integer) &key b c)
         (declare (ignore a b c)))

       (defmethod test ((a real) &key b)
         (declare (ignore a b)))
       t)
   (error () nil))
 "Evaluation of generic methods with incongruent keywords succeeds")

(prove:finalize)
