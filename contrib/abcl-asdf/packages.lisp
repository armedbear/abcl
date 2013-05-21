(defpackage #:abcl-asdf
  (:use :cl)
  (:export 
;;; Public API
   #:resolve

   #:ensure-mvn-version

;;; Leaning towards deprecation
   #:resolve-dependencies
   #:resolve-artifact

   #:find-mvn

   #:*mvn-directory*

   #:init

;;; ASDF
;;;   #:iri #:mvn
;;;   #:ensure-parsed-mvn

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
  (:use :cl :abcl-asdf)
  (:export #:run))
