(in-package :cl-user)

(defpackage build-abcl
  (:use :cl)
  (:nicknames :build-abcl :abcl-build :abcl/build)
  (:export

   #:abcl/build
   #:abcl/dist
   #:abcl/test

   ;; deprecated  TODO: hook into new interfaces
   #:build-abcl
   #:make-dist

   ;; utility functions that should be moved into utility package
   #:introspect-path-for
   #:split-string
   #:possible-executable-names
   #:probe-for-executable
   #:stringify
   #:listify
   #:some-directory

   ;;; lower-level
   #:xdg/abcl-install-root
   #:xdg/abcl-download-root

   #:xdg/install
   #:locally-install-and-unzip

   #:download-and-unzip
   #:download

   #:xdg/ant-executable
   #:with-ensured-ant
   #:ant/install
   #:ant/call

   #:with-ensured-maven
   #:mvn/install
   #:mvn/call
   
   #:install-zip
   #:download-artifact)

  ;;; TODO: use UIOP, currently only used for deprecated, old build system
  #+abcl
  (:import-from #:extensions #:run-shell-command #:probe-directory)
  #+allegro
  (:import-from #:excl #:probe-directory)
  #+clisp
  (:import-from #:ext #:probe-directory))

