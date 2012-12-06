;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to work.
(asdf:defsystem :jna 
    :version "3.5.1"
    :description  "<> asdf:defsystem <urn:abcl.org/release/1.1.0/contrib/jna#3.5.1"
    :defsystem-depends-on (jss abcl-asdf)
;; FIXME: always seems to be resolving the LATEST maven artifact.
    :components ((:mvn "net.java.dev.jna/jna/3.5.1"
                  :alternate-uri "http://repo1.maven.org/maven2/net/java/dev/jna/jna/3.5.1/jna-3.5.1.jar"
                  :classname "com.sun.jna.Native")))

(in-package :asdf) 
(defmethod perform :after ((o load-op) (c (eql (find-system :jna))))
  (when (jss:find-java-class "com.sun.jna.Native")
    (provide :jna)))

                         
