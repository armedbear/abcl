(in-package :abcl/build/t)

(let ((possible-executables
       (abcl/build:possible-executable-names "java"))) 
  (prove:plan 1)
  (prove:is-type possible-executables
                 'cons))

(prove:plan 1)
(prove:is (length (abcl/build:split-string "one.two.three." #\.))
          3)

(prove:finalize)




