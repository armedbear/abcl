(in-package :cl-user)

;;; <https://abcl.org/trac/ticket/479>
(prove:plan 1)
(let ((file
        (asdf:system-relative-pathname
         :abcl
         "t/eg/clos-unbound-use-mop.lisp")))
  (prove:ok 
   (handler-case
       (load file)
     (t (e)
       (prove:diag (format nil "Failed to load ~a: ~a" file e))
       nil))
   "Testing compilation of slot class allocation finalization"))

(prove:finalize)


        
