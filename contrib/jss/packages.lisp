(defpackage :jss
  (:nicknames "java-simple-syntax" "java-syntax-sucks")
  (:use :common-lisp :extensions :java)
  (:export 
   #:*inhibit-add-to-classpath*
   #:*added-to-classpath*
   #:*do-auto-imports*

   #:add-directory-jars-to-class-path
   #:add-to-classpath
   #:find-java-class
   #:need-to-add-directory-jar?

;;; deprecated
   #:new ; use JAVA:NEW
   #:get-java-field ; use JAVA:JFIELD

;;; Move to JAVA?
   #:jclass-all-interfaces

;;; Useful utilities to convert common Java items to Lisp counterparts
   #:hashmap-to-hashtable
   #:iterable-to-list
   #:list-to-list
   #:set-to-list
   #:vector-to-list

;;; Enable compatibility with jss-1.0 by placing symbols in CL-USER
   #:ensure-compatiblity #:*cl-user-compatibility*)
   (:shadow #:add-to-classpath))

