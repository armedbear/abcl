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

(eval-when (:compile-toplevel :load-toplevel)
  (java:add-to-classpath
   (merge-pathnames "../../../lsw2/lib/jscheme.jar" (asdf:component-pathname (asdf:find-system :jss)))))


