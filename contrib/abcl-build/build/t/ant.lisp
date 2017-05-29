(in-package :abcl/build/t)

(prove:plan 1)
(prove:ok
 (build-abcl:ant/install)
 "Testing ABCL-specific Ant installation of Ant into XDG hierarchy…")

(if (not (ignore-errors (asdf:find-system :abcl)))
    (prove:diag "Unable to find 'abcl.asd'.~&Enable ASDF to find 'abcl.asd' by adding symlink to ~~/common-lisp/ to ABCL source directory.")
    (let ((ant-file (asdf:system-relative-pathname :abcl "build.xml")))
      (prove:plan 1)
      (prove:ok
       (abcl-build:ant/call ant-file "abcl.diagnostic")
       (format nil "Testing invocation of private Ant on main ABCL build artifact at ~&~2,t~a…" ant-file))))
            
(prove:finalize)
