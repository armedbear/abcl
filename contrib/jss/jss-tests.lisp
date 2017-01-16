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
;;; http://abcl.org/trac/ticket/229
;;; http://abcl.org/trac/ticket/229 - note: version of test for this ticket was wrong in tests.lisp
(is (#"toString" (find "size" 
      (#"getMethods" (find-java-class "java.util.Collections$UnmodifiableMap"))
	  :test 'string-equal :key #"getName"))
    (#"toString" (java::jmethod "java.util.Collections$UnmodifiableMap" "size" )))

(finalize)
