;;;; -*- Mode: LISP -*-
(require :asdf)
(in-package :cl-user)

(asdf:defsystem :log4j
  :defsystem-depends-on (abcl-asdf)
  :components ((:module log4j.jar
                        :components ((:mvn "log4j/log4j")))))


