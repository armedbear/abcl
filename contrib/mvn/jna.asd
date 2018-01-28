;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to work.
(defsystem jna 
  :long-description  "<urn:abcl.org/release/1.6.0/contrib/jna#4.5.1>"
  :version "4.5.1"
  :defsystem-depends-on (jss abcl-asdf)
  :components ((:mvn "net.java.dev.jna/jna/4.5.1"
                :alternate-uri "http://repo1.maven.org/maven2/net/java/dev/jna/jna/4.5.1/jna-4.5.1.jar"
                :classname "com.sun.jna.Native")))

                         
