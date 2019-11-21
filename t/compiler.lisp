(prove:plan 1)

(prove:ok
 (compile-file (asdf:system-relative-pathname :abcl "t/eg/compiler-fails-on-inline-recursion.lisp")))

(prove:finalize)

