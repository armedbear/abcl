(in-package #:abcl.test.lisp)

(deftest java.truth.1
  (let ((java.lang.boolean.compare-to
         (java:jmethod "java.lang.Boolean" "compareTo" "java.lang.Boolean"))
        (java.lang.boolean.equals 
         (java:jmethod "java.lang.Boolean" "equals" "java.lang.Object")))
    (values 
     (java:jcall java.lang.Boolean.compare-to java:+true+ t)
     (java:jcall java.lang.Boolean.compare-to java:+false+ nil)
     (java:jcall java.lang.Boolean.equals java:+true+ t)
     (java:jcall java.lang.Boolean.equals java:+false+ nil)
     (java:jcall java.lang.Boolean.compare-to java:+false+ t)
     (java:jcall java.lang.Boolean.compare-to java:+false+ t)
     (java:jcall java.lang.Boolean.equals java:+true+ nil)
     (java:jcall java.lang.Boolean.equals java:+false+ t)))
  0 0 t t -1 -1 nil nil)

