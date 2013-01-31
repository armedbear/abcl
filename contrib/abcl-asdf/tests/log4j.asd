;;;; -*- Mode: LISP -*-

(asdf:defsystem :log4j  
  :defsystem-depends-on (abcl-asdf)
  :components ((:module log4j.jar :components 
            ((:mvn "log4j/log4j/1.2.15")))
   (:module source :pathname "" :components
            ((:file "example"))
            :depends-on (log4j.jar))))

(defmethod perform ((o asdf:test-op) (s (eql (asdf:find-system :log4j))))
  (asdf:load-system :log4j)
  (eval (read-from-string "(cl-user::test-log4j.2)")))
