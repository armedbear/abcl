(in-package :cl-user)

;;; <https://github.com/armedbear/abcl/issues/398>

(prove:plan 1)
(prove:is 
 (format nil "~:<~W ~(~W~)~:>" '(a b))
 '(A B))

(prove:finalize)
