(in-package :abcl/build/t)

(prove:plan 2)
(prove:diag "Testing BUILD-ABCL…")
(prove:ok 
 (abcl-build:build-abcl))
(prove:diag "Testing MAKE-DIST…")
(prove:ok
 (abcl-build:make-dist (format nil "test-" (random (expt 2 32)))))

#+abcl-build-test-more
(progn
  (prove:plan 1)
  (prove:ok (abcl-build:build-abcl :clean t)))

#+abcl-build-test-more
(progn
  (prove:plan 1)
  (abcl-build:build-abcl :force t))

(prove:finalize)


