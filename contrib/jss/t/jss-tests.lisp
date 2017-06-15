(in-package :cl-user)

(prove:plan 8)

(prove:is
 (read-from-string "#\"{bar}.{foo}\"")
 '(get-java-field bar foo t))
(prove:is
 (read-from-string "#\"q.bar.{foo}\"")
 '(get-java-field (load-time-value (find-java-class "q.bar")) foo t))
(prove:is
 (read-from-string "#\"{bar}.foo\"")
 '(get-java-field bar "foo" t))
(prove:is-error
 (read-from-string "#\".bar.foo\"")
 'simple-error)
;;; http://abcl.org/trac/ticket/205
(prove:is
 (with-constant-signature ((substring "substring"))
   (substring "01234" 2)) "234")
;;; http://abcl.org/trac/ticket/229 - note: version of test for this ticket was broken in tests.lisp
(prove:is (#"toString"
           (find "size" (#"getMethods" (find-java-class "java.util.Collections$UnmodifiableMap"))
                 :test 'string-equal :key #"getName"))
	  (#"toString" (java::jmethod "java.util.Collections$UnmodifiableMap" "size" )))

(prove:is 
 (jss::with-class-lookup-disambiguated (lang.object) (find-java-class 'object))
 (find-java-class 'java.lang.object))

;; Object is ambiguous in default java 
(prove:is-error
 (find-java-class 'object)
 'simple-error)

;; test that optimized jss is much faster than unoptimized
(let ()
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

  (prove:plan 1)
  (prove:is-type (let ((just-loop (timeit (just-loop 10000))))
	   (+ 0.0 
	      (/ (-  (timeit (optimized-jss 10000)) just-loop)
			(-  (timeit (unoptimized-jss 10000)) just-loop))))
                 '(float 0 0.1)
                 "Testing JSS compiler optimization…"))

(prove:plan 2)
(let* ((jss::*inhibit-jss-optimization* nil)
       (optimized-jss
        (macroexpand (precompiler::precompile-form
                                    '(#"compile" 'regex.Pattern ".*") t))))
  (let* ((jss::*inhibit-jss-optimization* t)
         (unoptimized-jss
          (macroexpand (precompiler::precompile-form '(#"compile" 'regex.Pattern ".*") t))))
    (prove:is (car optimized-jss) 'java:jstatic)
    (prove:is (caar unoptimized-jss) 'lambda)))

(prove:finalize)

