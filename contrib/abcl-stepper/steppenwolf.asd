;;;; -*- Mode: LISP -*-
(defsystem steppenwolf
  :version "0.0.1"
  :license nil
  :defsystem-depends-on (abcl-build)
  :depends-on (jss)
#|
  ;;; TODO DEBUG me; currently invoked manually
  :perform (compile-op (o c)
              (uiop:symbol-call :steppenwolf :build))
|#
#|
  :perform (load-op (o c)
              (uiop:symbol-call :steppenwolf :init))
|#
  :components ((:module package :pathname "./"
                :components ((:file "package")))
               (:module base :pathname "./"
                :depends-on (package)
                :components ((:file "steppenwolf")
                             (:static-file "steppenwolf.org")))))





