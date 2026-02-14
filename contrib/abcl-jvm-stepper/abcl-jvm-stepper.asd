;;;; -*- Mode: LISP -*-
(defsystem abcl-jvm-stepper
  :author "Alejandro Zamora Fonseca"
  :description "An operational stepper for compiled code"
  :long-description "<urn:abcl.org/release/1.9.2/contrib/abcl-jvm-stepper#>"
  :version "0.0.1"
  :components ((:module base
                :pathname ""
                :components ((:file "abcl-jvm-stepper")
                             (:static-file "README.markdown")))))
