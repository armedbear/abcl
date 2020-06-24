(in-package :cl-user)

(prove:plan 1)
(prove:isnt
 (handler-case 
     (jcoerce 2.4 "double")
   (error (e) (format *standard-output* "error: ~a~%" e)))
 nil
 "Coercing 2.4 to a double")

(let ((values '(2.44 -2.44)))
  (prove:plan (length values))
  (dolist (value values)
    (prove:is
     (= (#"doubleValue" (jcoerce value "double"))
        value)
     (format nil "JCOERCE ~a to a 'double'" value))))

(prove:finalize)


