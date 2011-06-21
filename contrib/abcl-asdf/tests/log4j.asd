;;;; -*- Mode: LISP -*-
(in-package :asdf)

(defsystem :log4j
  :components 
  ((:mvn "log4j/log4j" :version "1.4.9")
   (:module src :pathname "")
   ((:file "example"))))

  
