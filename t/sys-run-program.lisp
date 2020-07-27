(in-package :cl-user)

(prove:plan 1)
(prove:isnt
 (handler-case
     (let ((result 
             (sys:run-program "true" nil)))
       result)
   (error (e)
     (prog1
         nil
       (prove:diag (format nil "error: ~a" e)))))
 nil
 "Invoking sys:run-program")
     
(prove:finalize)

