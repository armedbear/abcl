;;; <https://github.com/armedbear/abcl/issues/177>

(prove:plan 2) ;;; why is this two tests?  

(prove:is
 (let ((condition (handler-case (/ 6 0) (division-by-zero (c) c)))
       result)
   (and 
    (arithmetic-error-operands condition)
    (arithmetic-error-operation condition)))
 t
 "DIVISION-BY-ZERO problems with operands and operation")

(prove:finalize)
