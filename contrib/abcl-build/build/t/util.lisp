(in-package :abcl/build/t)

(prove:plan 1)
(prove:is-type (abcl/build:possible-executable-names "java")
               'cons)

(prove:plan 1)
(prove:is (length (abcl/build:split-string "one.two.three." #\.))
          4)

(prove:finalize)




