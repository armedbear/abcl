(prove:plan 1)

;;; FIXME test shouldn't signal error
(prove:is-error 
 (compile-file (asdf:system-relative-pathname :abcl "t/eg/compiler-fails-on-inline-recursion.lisp"))
 'compiler-error
 "FIXME: this test should not result in a STORAGE-CONDITION as a compiler error")

(prove:finalize)

