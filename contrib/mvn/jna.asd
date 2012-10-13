;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to have a chance of working.
(asdf:defsystem :jna 
    :version "3.4.0"
    :defsystem-depends-on (jss abcl-asdf))
;; FIXME:  install a better handler in abcl-asdf  :components ((:mvn "net.java.dev.jna/jna/3.4.0")))

(defmethod asdf:perform :after ((o asdf:load-op) (c (eql (asdf:find-system :jna))))
  (when (jss:find-java-class "com.sun.jna.Native")
    (provide :jna)))

(defmethod asdf:perform :before ((o asdf:load-op) (c (eql (asdf:find-system :jna))))
  ;; Theoretically this should be the same thing as the MVN component.
  (handler-case 
      (unless (jss:find-java-class "com.sun.jna.Native")
        (unless (java:add-to-classpath (abcl-asdf:resolve "net.java.dev.jna:jna:3.4.0"))
          (java:add-to-classpath "http://repo1.maven.org/maven2/net/java/dev/jna/jna/3.4.0/jna-3.4.0.jar")))
    (t (e) 
      (error "Failed to resolve 'jna.jar' because~&~A." e))))

                         
