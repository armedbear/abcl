;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to work.
(defsystem jna 
  :long-description  "<urn:abcl.org/release/1.8.0/contrib/jna#5.6.0>"
  :version "5.6.0"
  :defsystem-depends-on (jss abcl-asdf)
  :components ((:mvn "net.java.dev.jna/jna/5.6.0"
                :alternate-uri "http://repo1.maven.org/maven2/net/java/dev/jna/jna/5.6.0/jna-5.6.0.jar"
                :classname "com.sun.jna.Native")))

                         
