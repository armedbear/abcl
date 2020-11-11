;;; -*- Mode: LISP; Syntax: COMMON-LISP -*-
(defsystem :abcl-prove
  :version "1.8.0"
  :defsystem-depends-on (prove-asdf)
  :depends-on (prove)
  :perform (test-op (o c)
              (uiop:symbol-call :prove-asdf 'run-test-system c))
  :components ((:module t
                :components ((:test-file "abcl-asdf")
                             (:test-file "arithmetic")
                             (:test-file "ash")
                             (:test-file "byte-vectors")
                             (:test-file "compiler-stack-inconsistency")
                             (:test-file "compiler")
                             (:test-file "decode-float")
                             (:test-file "disassemble")
                             (:test-file "format-dollar")
                             (:test-file "java-arrays")
                             (:test-file "java-call-sites")
                             (:test-file "jcoerce-numerics")
                             (:test-file "pathname")
                             (:test-file "print-symbol")
                             (:test-file "read-suppress")
                             (:test-file "run-program")
                             (:test-file "url-stream")
                             (:test-file "without-use-cl")))))







  
  
