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

(deftest runtime-class.doubles
  (flet ((dub (ignored-this x) (* 2 x)))
    (let ((class
            (java:jnew-runtime-class "Doubler"
                                :superclass "java.lang.Object"
                                :access-flags '(:public)
                                :methods `(
                                           ("dub" :double (:double) ,#'dub)))))
      (java:jcall "dub" (java:jnew class) 2)))
  4.0d0)

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

;; print-object
(deftest runtime-class.print-object
    (subseq
     (with-output-to-string (stream)
       (print-object
        (java:jnew
         (java:jnew-runtime-class
          "FooList"
          :superclass "java.util.AbstractList"
          :methods '(("get" "java.lang.Object" (:int)
                      (lambda (this index)
                        "Foo"))
                     ("size" :int ()
                      (lambda (this)
                        15)))))
        stream))
     0
     20)
  "#<FooList [Foo, Foo,")


;; class annotations
(deftest runtime-class.annotations.deprecated
  (let* ((class (java:jnew-runtime-class
                "Foo"
                :annotations '("java.lang.Deprecated")))
         (annotations (java:jcall "getAnnotations" class)))
    (assert (java:jinstance-of-p (aref annotations 0) "java.lang.Deprecated"))
    t)
  t)
