(defpackage #:abcl-asdf
  (:use :cl)
  (:export 
;;; Public API
   #:resolve

   #:resolve-dependencies
   #:resolve-artifact

   #:find-mvn

   #:*mvn-directory*

   #:init

;;; "Internal" API

;;;; Maven 
   #:*mvn-libs-directory*
   #:*maven-http-proxy*
   #:make-remote-repository
   #:*maven-remote-repository*
   #:*maven-verbose*

   #:resolve-artifact
   #:resolve-dependencies

   #:as-classpath

   #:add-directory-jars-to-class-path
   #:need-to-add-directory-jar?
   
   #:*added-to-classpath*
   #:*inhibit-add-to-classpath*))

(defpackage #:abcl-asdf-test
  (:use :cl #+nil :abcl-test-lisp) ;;; FIXME include some sort of test framework
  (:export #:run))
