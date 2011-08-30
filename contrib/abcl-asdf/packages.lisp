(defpackage #:abcl-asdf
  (:use :cl)
  (:export 
   #:satisfy
   #:as-classpath

   #:resolve-artifact
   #:resolve-dependencies

   #:add-directory-jars-to-class-path
   #:need-to-add-directory-jar?
   
   #:*added-to-classpath*
   #:*inhibit-add-to-classpath*))

