(defpackage #:abcl-asdf
  (:use :cl)
  (:export 
;;; Public API
   #:resolve-dependencies

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

   #:satisfy
   #:as-classpath

   #:add-directory-jars-to-class-path
   #:need-to-add-directory-jar?
   
   #:*added-to-classpath*
   #:*inhibit-add-to-classpath*))

(defpackage #:abcl-asdf-test
  (:use :cl #+nil :abcl-test-lisp) ;;; FIXME include some sort of test framework
  (:export #:run))
