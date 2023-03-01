;;;; -*- Mode: LISP -*-
(defsystem abcl-stepper
  :author "Alejandro Zamora Fonseca"
  :description "An operational stepper for ABCL"
  :long-description "<urn:abcl.org/release/1.9.1/contrib/abcl-stepper#>"
  :version "0.0.2"
  :depends-on (steppenwolf)
  :components  ((:module package :pathname "./"
                 :components ((:file "package")))
                (:module base :pathname "./"
                 :depends-on (package)
                 :components ((:file "abcl-stepper")
                              (:static-file "README.markdown")))))
