(in-package :abcl/build/t)

(prove:plan 1)
(prove:diag "Testing private installation of Ant…")
(prove:ok (build-abcl:ant/install))

(prove:plan 1)
(let ((antfile (asdf:system-relative-pathname :abcl "build.xml")))
  (prove:diag (format nil "Testing invocation of private Ant on main ABCL build artifact at ~&~2,t~a…"
                      antfile))
  (prove:ok (abcl-build:ant/call antfile "abcl.diagnostic")))

(prove:finalize)
