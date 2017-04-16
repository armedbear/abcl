(defpackage build-abcl
  (:use "COMMON-LISP")
  (:nicknames "ABCL-BUILD" "ABCL/BUILD")
  (:export

   #:install
   #:install/ant
   
   #:install-zip
   #:download-artifact
   
   #:platform

   ;; Deprecated
   #:build-abcl
   #:make-dist)
  #+abcl
  (:import-from #:extensions #:run-shell-command #:probe-directory)
  #+allegro
  (:import-from #:excl #:probe-directory)
  #+clisp
  (:import-from #:ext #:probe-directory))
