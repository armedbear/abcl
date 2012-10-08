;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to have a chance of working.
;;; XXX jna-3.4.0 seems much more capable, but doesn't have a resolvable pom.xml from Maven central
(asdf:defsystem :jna 
    :version "3.0.9"
    :defsystem-depends-on (abcl-asdf)
    :components ((:mvn "com.sun.jna/jna/3.0.9")))
