(in-package :cl-user)

(defpackage abcl-asdf
  (:use cl)
  (:import-from :abcl/build
                #:split-string)
  (:export 
;;; Public API
   #:resolve


   ;; Configuring Maven

   #:with-aether

   #:ensure-mvn-version
   
   #:find-mvn
   #:mvn-version

   #:*mvn-directory*

   #:init

;;; "Internal" API
   #:resolve-dependencies
   #:resolve-artifact

;;;; Maven 
   #:*mvn-libs-directory*
   #:*maven-http-proxy*
   #:*default-repository*
   #:make-remote-repository
   #:*maven-remote-repository*
   #:resolve-multiple-maven-dependencies

   #:as-classpath

   #:add-directory-jars-to-class-path
   #:need-to-add-directory-jar?
   
   #:*added-to-classpath*
   #:*inhibit-add-to-classpath*))

(defpackage abcl-asdf/osgi
  (:use cl)
  (:nicknames osgi)
  (:export
   #:add-bundle
   #:find-bundle-class
   #:ensure-osgi-initialized
   #:add-to-comma-separated-osgi-config

   #:*before-osgi-starting-hooks*
   #:*loaded-osgi-bundles*
   #:*osgi-cache-location*
   #:*osgi-configuration*
   #:*osgi-clean-cache-on-start*))

   
