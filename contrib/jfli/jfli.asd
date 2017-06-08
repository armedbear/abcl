;;;; -*- Mode: LISP -*-
(defsystem jfli
  :long-description "<urn:abcl.org/release/1.5.0/contrib/jfli#>"
  :version "0.2.0"
  :components ((:file "jfli")))

;;; Requires integration with IntelliJ IDEA editor (free download)
(defsystem jfli/intellij-tests
  :version "0.2.0"
  :depends-on (jfli)
  :components ((:module test 
                        :components ((:file "yanking")))))
