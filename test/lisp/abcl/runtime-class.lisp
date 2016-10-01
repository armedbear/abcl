(in-package :abcl.test.lisp)


;; method with no arguments
(deftest runtime-class.1
    (java:jnew-runtime-class 
     "Actor"
     :fields `(("name" "java.lang.String"))
     :methods `(("getName" "java.lang.String" nil
                           (lambda (this)
                             (java:jfield this "name")))))
  t)

;; method with primitive type
(deftest runtime-class.2
    (java:jnew-runtime-class 
     "Actor"
     :fields `(("name" "java.lang.String"))
     :methods `(("getName" "java.lang.String" (:int)
                           (lambda (this) 
                             (java:jfield this "name")))))
  t)

;; inheritance of type 

(deftest runtime-class.3
    (progn 
      (java:jnew-runtime-class 
       "foo.Actor"
       :fields `(("name" "java.lang.String")))
      (java:jnew-runtime-class 
       "foo.StageActor"
       :superclass "foo.Actor"
       :fields (list '("givenName" "java.lang.String"))))
  t)


#|
// Simple constructor test
public class Actor {
  String name;
  
  public Actor(String name) {
    this.name = name;
  }

  public String getName() {
    return name;
  }
  
}
|#

;; constructor
(deftest runtime-class.4
    (java:jnew-runtime-class 
     "Actor"
     :constructors `(("java.lang.String") 
                     (lambda (name) 
                       (setf (jfield this "name")
                             name)))
     :methods `(("getName" "java.lang.String" ("java.lang.String")  ;; no-arg methods not working
                           (lambda (this dummy) 
                             (declare (ignore dummy))
                             (java:jfield this "name"))))
     :fields `(("name" "java.lang.String")))
  t)


    
