(in-package :cl-user)

(let ((set (list '(2 3 5 7 11))))
  (prove:plan 1)
  (prove:is-type (jss:to-hashset set)
                 'java:java-object
                 "Checking whether JSS:TO-HASHSET produces a Java object…"))

(prove:finalize)



