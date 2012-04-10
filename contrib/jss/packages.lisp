(defpackage :jss
  (:nicknames "java-simple-syntax" "java-syntax-sucks")
  (:use :common-lisp :extensions :java)
  (:export 
   #:*inhibit-add-to-classpath*
   #:*added-to-classpath*
   #:*do-auto-imports*

   #:invoke-restargs
   #:with-constant-signature

   #:invoke-add-imports
   #:find-java-class
   #:jcmn
   #:japropos
   #:new 
   
   #:jar-import
   #:classfiles-import

;;; Useful utilities to convert common Java items to Lisp counterparts
   #:hashmap-to-hashtable
   #:iterable-to-list
   #:jlist-to-list
   #:set-to-list
   #:vector-to-list
   #:jarray-to-list

;;; XXX Necessary to work in OSGi?
   #:get-java-field ; use JAVA:JFIELD
   #:set-java-field ; use JAVA-JFIELD

;;; deprecated
   #:list-to-list

;;; Move to JAVA?
   #:jclass-all-interfaces

;;; Enable compatibility with jss-1.0 by placing symbols in CL-USER
   #:ensure-compatibility #:*cl-user-compatibility*))


