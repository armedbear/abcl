(in-package :abcl-asdf/test)

(plan 5)

(diag "Testing local bootable Maven version.")

(multiple-value-bind (good version)
    (ensure-mvn-version)
  (ok good)
  (is-type version 'list)
  (ok (every #'fixnump version)))

(is-type (abcl-asdf:resolve-dependencies "log4j" "log4j") 'string)
(is-type (abcl-asdf:resolve "org.abcl/abcl") 'string)

(finalize)
