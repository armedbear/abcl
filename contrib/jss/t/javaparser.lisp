(in-package :cl-user)

(defparameter expanded '(let ((jss::this jss::*object-for-this*))
  (jcall "getLoaded"
         (jcall "load"
                (jcall "make"
                       (jcall "intercept"
                              (jcall "method"
                                     (jcall "subclass"
                                            (new '|ByteBuddy|)
                                            (find-java-class '|Object|)
                                            t)
                                     (jstatic "named"
                                              (find-java-class '|ElementMatchers|)
                                              "toString"))
                              (jstatic "value"
                                       (find-java-class '|FixedValue|)
                                       "Hello World!")))
                (jcall "getClassLoader"
                       (jcall "getClass" jss::this))))))

(defparameter source '#1"new ByteBuddy().subclass(Object.class,t)
   .method(ElementMatchers.named("toString"))
   .intercept(FixedValue.value("Hello World!"))
   .make()
   .load(getClass().getClassLoader())
   .getLoaded()" )

(in-package :jss-test)

(prove:plan 1)
(prove:is source expanded)

(prove:finalize)
