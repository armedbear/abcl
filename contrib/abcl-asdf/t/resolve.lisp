(prove:plan 1)

(prove:ok 
 (let ((result (abcl-asdf:resolve-dependencies "org.armedbear.lisp" "abcl")))
   (and result
        (format *standard-output* "~&~A~%" result)
        (type-p result 'cons)))
 "Resolving ABCL from distributed Maven POM graph.")

(prove:finalize)

