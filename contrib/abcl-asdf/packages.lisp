(defpackage #:abcl-asdf
  (:use :cl)
  (:export 
;;; Public API
   #:resolve-dependencies

;;; "Internal" API
   #:satisfy
   #:as-classpath

   #:resolve-artifact

   #:add-directory-jars-to-class-path
   #:need-to-add-directory-jar?
   
   #:*added-to-classpath*
   #:*inhibit-add-to-classpath*))

(defpackage #:abcl-asdf-test
  (:use :cl :abcl-rt)
  (:export #:run))
