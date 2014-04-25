(in-package #:abcl.test.lisp)

(deftest java.truth.1
  (let ((java.lang.boolean.compare 
         (java:jmethod "java.lang.Boolean" "compare" "boolean" "boolean"))
        (java.lang.boolean.equals 
         (java:jmethod "java.lang.Boolean" "equals" "java.lang.Object")))
    (values 
     (java:jstatic java.lang.Boolean.compare "java.lang.Boolean" java:+true+ t)
     (java:jstatic java.lang.Boolean.compare "java.lang.Boolean" java:+false+ nil)
     (java:jcall java.lang.Boolean.equals java:+true+ t)
     (java:jcall java.lang.Boolean.equals java:+false+ nil)
     (java:jstatic java.lang.Boolean.compare "java.lang.Boolean" java:+false+ t)
     (java:jstatic java.lang.Boolean.compare "java.lang.Boolean" java:+false+ t)
     (java:jcall java.lang.Boolean.equals java:+true+ nil)
     (java:jcall java.lang.Boolean.equals java:+false+ t)))
  0 0 t t -1 -1 nil nil)

