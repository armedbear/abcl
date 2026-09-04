;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to work.
(defsystem jna 
  :version "5.18.1"
  :long-description  "<urn:abcl.org/release/1.10.0/contrib/jna#5.18.1>"  
  :defsystem-depends-on (jss abcl-asdf)
  :components ((:mvn "net.java.dev.jna/jna/5.18.1"
                :alternate-uri "https://repo1.maven.org/maven2/net/java/dev/jna/jna/5.18.1/jna-5.18.1.jar"
                :classname "com.sun.jna.Native")))

                         
