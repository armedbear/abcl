(in-package :cl-user)
(defpackage jna
  (:nicknames :jna)
  (:use :cl))

(in-package :jna)

(defmethod asdf:perform :after ((o asdf:load-op) (c (eql (asdf:find-system :jna))))
  (when (jss:find-java-class "com.sun.jna.Native")
    (provide :jna)))
