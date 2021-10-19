(in-package :cl-user)

;;; <https://github.com/armedbear/abcl/issues/399>

(prove:plan 1)
(prove:ok
 (functionp
  (coerce 'documentation 'function)))
(prove:finalize)
