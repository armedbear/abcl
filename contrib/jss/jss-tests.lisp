(in-package :cl-user)

(defpackage jss-test
  (:use :cl :cl-user :jss :prove))

(in-package :jss-test)

(plan 8)

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

(defun unoptimized-jss (count)
  (loop repeat count do (#"compile" 'regex.Pattern ".*")))

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


(is-type (let ((just-loop (cl-user::print-db (timeit (just-loop 10000)))))
	   (+ 0.0 
	      (print (/ (-  (timeit (optimized-jss 10000)) just-loop)
			(-  (timeit (unoptimized-jss 10000)) just-loop)))))
	 '(float 0 0.1))

(is (let* ((jss::*inhibit-jss-optimization* nil)
	   (optimized-jss (macroexpand (precompiler::precompile-form '(#"compile" 'regex.Pattern ".*") t))))
      (let* ((jss::*inhibit-jss-optimization* t)
	     (unoptimized-jss (macroexpand (precompiler::precompile-form '(#"compile" 'regex.Pattern ".*") t))))
	(and (eq (car optimized-jss) 'jstatic)
	     (eq (caar unoptimized-jss) 'lambda)))))

(finalize)


