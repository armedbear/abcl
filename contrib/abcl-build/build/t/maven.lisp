(in-package :abcl/build/t)

(prove:plan 1)
(prove:diag "Testing private installation of Maven…")
(prove:ok
 (abcl/build:mvn/install))

(prove:plan 1)
(let ((pom (asdf:system-relative-pathname :abcl "pom.xml")))
  (prove:diag (format nil "Testing invocation of private Maven on root ABCL POM at~&~2,t~a…"
                      pom))
  (prove:ok (abcl/build:mvn/call pom "install")))

(prove:finalize)
