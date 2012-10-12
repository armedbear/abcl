;;;; -*- Mode: LISP -*-

;;;; Need to have jna.jar present for CFFI to have a chance of working.
(asdf:defsystem :jna 
    :version "3.4.0"
    :defsystem-depends-on (abcl-asdf))
;; FIXME:  install a better handler in abcl-asdf  :components ((:mvn "net.java.dev.jna/jna/3.4.0")))

(defmethod asdf:perform :after ((o asdf:load-op) (c (eql (asdf:find-system :jna))))
  ;; Theoretically this should be the same thing as the MVN component.
  (java:add-to-classpath (abcl-asdf:resolve "net.java.dev.jna:jna:3.4.0")))

                         
