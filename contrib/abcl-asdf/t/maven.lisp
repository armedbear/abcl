(in-package :cl-user)

(prove:plan 5)

(prove:diag "Testing local bootable Maven version.")

(multiple-value-bind (good version)
    (abcl-asdf:ensure-mvn-version)
  (prove:ok good)
  (prove:is-type version 'list)
  (prove:ok (every #'fixnump version)))

(prove:is-type (abcl-asdf:resolve-dependencies "log4j" "log4j") 'string)
(prove:is-type (abcl-asdf:resolve "org.abcl/abcl") 'string)

(prove:finalize)
