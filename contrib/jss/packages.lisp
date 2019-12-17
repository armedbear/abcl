(defvar cl-user::*before-osgi-starting-hooks* nil)
(export 'cl-user::*before-osgi-starting-hooks* 'cl-user)
(defpackage :jss
  (:nicknames "java-simple-syntax" "java-syntax-sucks")
  (:use :common-lisp :extensions :java)
  (:import-from "CL-USER" cl-user::*before-osgi-starting-hooks*)
  (:export 
   #:*inhibit-add-to-classpath*
   #:*added-to-classpath*
   #:*do-auto-imports*
   #:*muffle-warnings*

   #:invoke-restargs
   #:with-constant-signature

   #:invoke-add-imports
   #:find-java-class

   #:add-bundle
   #:find-bundle-class
   #:ensure-osgi-initialized
   #:*osgi-cache-location*
   #:*osgi-configuration*
   #:*osgi-clean-cache-on-start*
   
   #:jcmn #:java-class-method-names
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
   #:to-hashset

   #:j2list
   #:jmap

;;; XXX Necessary to work in OSGi?
   #:get-java-field ; use JAVA:JFIELD
   #:set-java-field ; use JAVA-JFIELD

;;; deprecated
   #:list-to-list

;;; Move to JAVA?
   #:jclass-all-interfaces

;;; Enable compatibility with jss-1.0 by placing symbols in CL-USER
   #:ensure-compatibility #:*cl-user-compatibility*))


