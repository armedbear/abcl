;;; <https://github.com/armedbear/abcl/issues/177>

(prove:plan 1)

(prove:is
 (let ((condition (handler-case (/ 6 0) (division-by-zero (c) c))))
   (arithmetic-error-operands condition))
 '(6 0)
 "DIVISION-BY-ZERO problems with operands")

(prove:finalize)
