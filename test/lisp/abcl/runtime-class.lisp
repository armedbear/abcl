(in-package :abcl.test.lisp)

;; method with no arguments
(deftest runtime-class.1
    (java:jclass-name
     (java:jnew-runtime-class
      "Actor"
      :fields '(("name" "java.lang.String" :getter NIL))
      :methods '(("getName" "java.lang.String" NIL
                  (lambda (this)
                    (java:jfield "name" this))))))
  "Actor")

;; method with primitive type
(deftest runtime-class.2
    (java:jclass-name
     (java:jnew-runtime-class
      "Actor"
      :fields '(("name" "java.lang.String" :getter NIL))
      :methods '(("getName" "java.lang.String" (:int)
                  (lambda (this x)
                    (declare (ignore x))
                    (java:jfield "name" this))))))
  "Actor")

;; inheritance of type
(deftest runtime-class.3
    (let ((class-loader (java::make-memory-class-loader)))
      (java:jnew-runtime-class
       "foo.Actor"
       :fields '(("name" "java.lang.String"))
       :class-loader class-loader)
      (java:jclass-name
       (java:jnew-runtime-class
        "foo.StageActor"
        :superclass "foo.Actor"
        :fields '(("givenName" "java.lang.String"))
        :class-loader class-loader)))
  "foo.StageActor")

;; constructor
(deftest runtime-class.4
    (java:jcall "getName"
                (java:jnew
                 (java:jnew-runtime-class
                  "Actor"
                  :constructors '((("java.lang.String")
                                   (lambda (this name)
                                     (setf (java:jfield "name" this) name))))
                  :methods '(("getName" "java.lang.String" NIL
                              (lambda (this)
                                (java:jfield "name" this))))
                  :fields '(("name" "java.lang.String" :getter NIL)))
                 "Someone"))
  "Someone")
