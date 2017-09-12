(in-package :abcl-asdf/test)

(plan 4)

(diag "Testing local bootable Maven version.")
(diag (format nil "~{~&~a~}" (multiple-value-list (ensure-mvn-version))))

(is-type (abcl-asdf:resolve-dependencies "log4j" "log4j") 'string)
(is-type (abcl-asdf:resolve "org.abcl/abcl") 'string)

(finalize)
