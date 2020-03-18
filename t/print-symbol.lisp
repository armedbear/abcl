(in-package :cl-user)

;; <https://github.com/armedbear/abcl/issues/152>
(progn
  (defpackage a (:use))
  (import 'cl-user::sym 'a)
  (export 'cl-user::sym 'a)
  (defpackage b (:use a))
  (prove:plan 2)
  (prove:is
   (let ((*package* (find-package 'b)))
     (read-from-string "sym"))
   'sym)
  (prove:is
   (let ((*package* (find-package 'b)))
     (prin1-to-string (find-symbol "SYM" 'b)))
   "SYM")
  (unintern 'sym :cl-user)
  (delete-package :a)
  (delete-package :b))


(prove:finalize)

   
