(in-package :abcl/build/t)

(let ((executable
       (abcl/build:localize-executable-name "java"))) ;; RUNTIME difference
  (prove:plan 1)
  (prove:ok executable)
  (if (uiop:os-windows-p)
      (prove:plan 1)
      (prove:is (pathname-type executable) "exe")))

(prove:plan 1)
(prove:is (length (abcl/build:split-string "one.two.three." #\.))
          3)

(prove:finalize)




