#|
(abcl-asdf:resolve-dependencies "log4j" "log4j")


|#
(in-package :abcl-asdf/test)

(plan 4)

(ok
 (diag "Testing local bootable Maven version.")
 (diag (format nil
               "~{~&~a~}" (multiple-value-list (ensure-mvn-version)))))

(ok 
 (let ((result (abcl-asdf:resolve-dependencies "log4j" "log4j")))
   (and result
        (diag (format nil"~&~A~%" result))
        (type-p result 'cons))))
(ok
 (abcl-asdf:resolve "org.abcl/abcl"))

(finalize)



