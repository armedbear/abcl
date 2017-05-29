(in-package :abcl/build/t)

(prove:plan 1)
(prove:ok
 (abcl/build:mvn/install)
 "Testing ABCL-specific Ant installation of Maven into XDG hierarchy…")

(if (not (ignore-errors (asdf:find-system :abcl)))
        (prove:diag "Unable to find 'abcl.asd'.~&Enable ASDF to find 'abcl.asd' by adding symlink to ~~/common-lisp/ to ABCL source directory.")
    (let ((pom (asdf:system-relative-pathname :abcl "pom.xml")))
      (prove:ok (abcl/build:mvn/call pom "install")
                (format nil "Testing invocation of private Maven on root ABCL POM at~&~2,t~a…"
                        pom))))

(prove:finalize)
