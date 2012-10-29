;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to have a chance of working.
(asdf:defsystem :jna 
    :version "3.5.1"
    :defsystem-depends-on (jss abcl-asdf)
;; FIXME: always seems to be resolving the LATEST maven artifact.
    :components ((:mvn "net.java.dev.jna/jna/3.5.1")))

(in-package :asdf) 
(defmethod perform :after ((o load-op) (c (eql (find-system :jna))))
  (when (jss:find-java-class "com.sun.jna.Native")
    (provide :jna)))

;;; After ASDF performs COMPILE-OP, one expects that the JNA Java
;;; classes can be instantiated.  If not, execute various loading strategies.
(defmethod perform ((o compile-op) (c (eql (find-system :jna))))
  ;; Theoretically this should be the same thing as the MVN component.
  (format *debug-io* "~&Attemping to locate jvm binary artifacts for JNA...~&")
  (handler-case 
      (jss:find-java-class "com.sun.jna.Native")
    (java:java-exception (e)
      (unless 
          (java:add-to-classpath (abcl-asdf:resolve "net.java.dev.jna:jna:3.5.1"))
        (unless 
            ;; Might want to download to local filesystem, then place in classpath
            (java:add-to-classpath #p"http://repo1.maven.org/maven2/net/java/dev/jna/jna/3.5.1/jna-3.5.1.jar")
          (error "Failed to load jna-3.5.0.jar from the network via URI."))
        (error "Failed to load jna.jar via ABCL-ASDF.")))
    (t (e) 
      (error "Failed to resolve 'jna.jar' because~&~A.~&" e))))

                         
