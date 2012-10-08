;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to have a chance of working.
(asdf:defsystem :jna 
    :version "3.4.0"
    :defsystem-depends-on (abcl-asdf)
    :components ((:mvn "net.java.dev.jna/jna/3.4.0")))
