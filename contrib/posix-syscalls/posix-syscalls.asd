(defsystem posix-syscalls
  :depends-on (jna)
  :components ((:module package :pathname ""
                :components ((:file "package")))
               (:module source :pathname ""
                :depends-on (package)
                :components ((:file "getenv")))
               (:module shim-system :pathname ""
                :depends-on (source)
                :components ((:file "system")))))

                


