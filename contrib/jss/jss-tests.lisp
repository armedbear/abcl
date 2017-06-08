(in-package :cl-user)

(defpackage jss-test
  (:use :cl :cl-user :jss :prove))

(in-package :jss-test)

(plan 6)

(is (read-from-string "#\"{bar}.{foo}\"") '(get-java-field bar foo t))
(is (read-from-string "#\"q.bar.{foo}\"") '(get-java-field (load-time-value (find-java-class "q.bar")) foo t))
(is (read-from-string "#\"{bar}.foo\"") '(get-java-field bar "foo" t))
(is-error (read-from-string "#\".bar.foo\"") 'simple-error)
;;; http://abcl.org/trac/ticket/205
(is (with-constant-signature ((substring "substring")) (substring "01234" 2)) "234")
;;; http://abcl.org/trac/ticket/229 - note: version of test for this ticket was broken in tests.lisp
(is (#"toString" (find "size" 
      (#"getMethods" (find-java-class "java.util.Collections$UnmodifiableMap"))
	  :test 'string-equal :key #"getName"))
    (#"toString" (java::jmethod "java.util.Collections$UnmodifiableMap" "size" )))

;; test that optimized jss is much faster than unoptimized
(defun optimized-jss (count)
  (loop repeat count do (#"compile" 'regex.Pattern ".*")))

(let ((jss::*inhibit-jss-optimization* t))
  (defun unoptimized-jss (count)
  (loop repeat count do (#"compile" 'regex.Pattern ".*"))))

(defun just-loop (count)
  (loop repeat count))

(let ((jss::*inhibit-jss-optimization* nil))
  (compile 'just-loop)
  (compile 'optimized-jss))
(let ((jss::*inhibit-jss-optimization* t))
  (compile 'unoptimized-jss))

(defmacro timeit (&body body)
  `(let ((start (#"currentTimeMillis" 'system)))
    ,@body
    (- (#"currentTimeMillis" 'system) start)))


(plan 1)
(is-type (let ((just-loop (timeit (just-loop 10000))))
	   (+ 0.0 
	      (/ (-  (timeit (optimized-jss 10000)) just-loop)
			(-  (timeit (unoptimized-jss 10000)) just-loop))))
	 '(float 0 0.1))

(plan 2)
(let* ((jss::*inhibit-jss-optimization* nil)
       (optimized-jss (macroexpand (precompiler::precompile-form '(#"compile" 'regex.Pattern ".*") t))))
  (let* ((jss::*inhibit-jss-optimization* t)
         (unoptimized-jss (macroexpand (precompiler::precompile-form '(#"compile" 'regex.Pattern ".*") t))))
    (is (car optimized-jss) 'java:jstatic)
    (is (caar unoptimized-jss) 'lambda)))

(finalize)

(plan 1)

(in-package :jss)
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
						  '|ElementMatchers|
						  "toString"))
				  (jstatic "value"
					   '|FixedValue|
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

(is jss::source
    jss::expanded)

(finalize)
