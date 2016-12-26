;;;; -*- Mode: LISP -*-
(asdf:defsystem :jss-tests
  :depends-on (jss)
  :components ((:module tests :pathname "" 
                        :components ((:file "test-optimize-java-call")
                                     (:file "tests")))))





   


