(defsystem steppenwolf
  :version "0.0.1"
  :license nil ;; FIXME
  :defsystem-depends-on (abcl-build)
  :depends-on (jss)
  ;;; TODO DEBUG me; currently invoked manually
  :in-order-to ((compile-op (o c)
                   (with-ensured-ant ()
                     (abcl-build/ant-call "build.xml" "compile"))))
  :components ((:module package :pathname "./"
                        :components ((:file "package")))
               (:module base :pathname "./"
                        :depend-on (package)
                :components ((:file "steppenwolf")
                             (:static-file "steppenwolf.org")))))


  
  
               
