(in-package :cl-user)

(defpackage jss-test
  (:use :cl :cl-user :jss :prove))

(in-package :jss-test)

(plan 4)

(is (read-from-string "#\"{bar}.{foo}\"") '(get-java-field bar foo t))
(is (read-from-string "#\"q.bar.{foo}\"") '(get-java-field (load-time-value (find-java-class "q.bar")) foo t))
(is (read-from-string "#\"{bar}.foo\"") '(get-java-field bar "foo" t))
(is-error (read-from-string "#\".bar.foo\"") 'simple-error)
(finalize)
