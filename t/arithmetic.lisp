;;; <https://github.com/armedbear/abcl/issues/177>

(prove:plan 1) 

(prove:ok
 (let ((condition (handler-case (/ 6 0) (division-by-zero (c) c))))
   (and
    (equal 
     (arithmetic-error-operands condition)
     '(6 0))
    (equal 
     (arithmetic-error-operation condition)
     '/)))
 "DIVISION-BY-ZERO problems with operands and operation")

(prove:finalize)
