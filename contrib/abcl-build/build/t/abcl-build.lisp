(in-package :abcl/build/t)

(if (not (ignore-errors (asdf:find-system :abcl)))
    (prove:diag "Unable to find 'abcl.asd'.~&Enable ASDF to find 'abcl.asd' by adding symlink to ~~/common-lisp/ to ABCL source directory.")
    (prove:subtest "Testing BUILD-ABCL."
      (prove:plan 2)
      (prove:ok 
       (abcl-build:build-abcl)
       "Testing BUILD-ABCL…")
      (prove:ok
       (abcl-build:make-dist (format nil "test-" (random (expt 2 32))))
       "Testing MAKE-DIST…")
      #+abcl-build-test-more
      (progn
        (prove:ok
         (abcl-build:build-abcl :clean t)
         "Testing BUILD:ABCL clean…"))
      #+abcl-build-test-more
      (prove:ok 
       (abcl-build:build-abcl :force t)
       "Testing BUILD-ABCL force…")))

(prove:finalize)


