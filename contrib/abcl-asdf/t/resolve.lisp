(in-package :cl-user)

(prove:plan 1)

(prove:is-type
 (abcl-asdf:resolve-dependencies "org.armedbear.lisp" "abcl")
 'string
 "Resolving ABCL from distributed Maven POM graph.")

(prove:finalize)
