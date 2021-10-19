(in-package :cl-user)

;;;  <https://github.com/armedbear/abcl/issues/396>
(prove:plan 1)
(prove:ok 
 (with-compilation-unit () (signal 'warning)))
(prove:finalize)
