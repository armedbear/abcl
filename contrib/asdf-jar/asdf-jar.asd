;;;; -*- Mode: LISP -*-
(defsystem asdf-jar
  :author "Mark Evenson"
  :description "Packaging ASDF systems into jar files"
  :long-description "<urn:abcl.org/release/1.8.0/contrib/asdf-jar#>"
  :version "0.3.2"
  :components ((:module base
                        :pathname ""
                        :components ((:file "asdf-jar")
                                     (:static-file "README.markdown")))))
