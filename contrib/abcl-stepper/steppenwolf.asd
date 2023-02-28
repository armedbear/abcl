;;;; -*- Mode: LISP -*-
(defsystem steppenwolf
  :version "0.0.1"
  :license nil
  :defsystem-depends-on (abcl-build)
  :depends-on (jss)
  ;;; TODO DEBUG me; currently invoked manually
  :in-order-to ((compile-op (o c)
                  (with-ensured-ant ()
                    (let ((ant-file
                            (merge-pathnames "build.xml" (root-directory))))
                      (abcl-build:ant/call  ant-file "compile")))
                  (abcl-build/ant-call ant-file  "compile"))
                (load-op (o c)
                   (steppenwolf:init)))
  :components ((:module package :pathname "./"
                :components ((:file "package")))
               (:module base :pathname "./"
                :depends-on (package)
                :components ((:file "steppenwolf")
                             (:static-file "steppenwolf.org")))))





