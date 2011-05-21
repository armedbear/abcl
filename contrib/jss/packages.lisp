(defpackage :jss
  (:nicknames "java-simple-syntax" "java-syntax-sucks")
  (:use :common-lisp :extensions :java)
  (:export 
   #:*inhibit-add-to-classpath*
   #:*added-to-classpath*
   #:add-to-classpath
   #:new
   #:need-to-add-directory-jar?
   #:add-directory-jars-to-class-path

;;; compatibility
   #:ensure-compatiblity #:*cl-user-compatibility*
   #:get-java-field)
   (:shadow #:add-to-classpath))

