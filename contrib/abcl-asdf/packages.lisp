(defpackage #:abcl-asdf
  (:use :cl)
  (:export 
;;; Public API
   #:resolve

   #:ensure-mvn-version

   #:find-mvn

   #:*mvn-directory*

   #:init

;;; "Internal" API
   #:resolve-dependencies
   #:resolve-artifact

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
  (:use :cl :abcl-asdf)
  (:export #:run))
