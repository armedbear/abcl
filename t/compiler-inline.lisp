;;;; <https://github.com/armedbear/abcl/issues/591> 
(unless (ignore-errors (asdf:load-system :fset))
  (ql:quickload :fset))

(load 
 (compile-file
  (asdf:system-relative-pathname :abcl "t/eg/inline-labels.lisp")))

(prove:plan 1)
(prove:is
    (fset-user::foo '(a &rest b))
    '(a fset-user::local-tail b))
(prove:finalize)

