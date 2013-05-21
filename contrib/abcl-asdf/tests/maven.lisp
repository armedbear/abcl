#|
(abcl-asdf:resolve-dependencies "log4j" "log4j")

(abcl-asdf:resolve-dependencies "org.armedbear.lisp" "abcl")

|#

; TODO figure out what sort of test framework we can hook in.  Probably ABCL-RT

(in-package :abcl-asdf-test)

(rt:deftest LOG4J.1
    (let ((result (abcl-asdf:resolve-dependencies "log4j" "log4j")))
      (and result
           (format *standard-output* "~&~A~%" result)
           (type-p result 'cons)))
  t)


(rt:deftest ABCL.1 
  (let ((result (abcl-asdf:resolve-dependencies "org.armedbear.lisp" "abcl")))
    (and result
         (format *standard-output* "~&~A~%" result)
         (type-p result 'cons)))
  t)



