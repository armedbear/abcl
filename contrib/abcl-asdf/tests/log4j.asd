;;;; -*- Mode: LISP -*-
(in-package :asdf)

(defsystem :log4j
  :components 
  ((:module log4j.jar :components 
            ((:mvn "log4j/log4j/1.2.15")))
   (:module source :pathname "" :components
            ((:file "example"))
            :depends-on (log4j.jar))))

  
