(defpackage system/posix-syscalls
  (:nicknames #:sys/posix-syscalls)
  (:use :common-lisp)
  (:export
   #:getenv
   #:putenv
   #:unsetenv))

