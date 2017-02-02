(require :asdf)
(in-package :cl-user)

(asdf:defsystem :jss
  :author "Alan Ruttenberg, Mark Evenson"
  :long-description "<urn:abcl.org/release/1.5.0/contrib/jss#>"
  :version "3.2.3" 
  :components ((:module base 
                        :pathname "" :serial t 
                        :components ((:file "packages")
                                     (:file "invoke")
                                     (:file "collections")
				     (:file "optimize-java-call")
                                     (:file "classpath")
				     (:file "transform-to-field")
                                     (:file "compat"))))
;;  :defsystem-depends-on (:prove-asdf)
;;  :in-order-to ((test-op (test-op jss/tests)))
  )

;; Until prove-asdf works
(let ((where (merge-pathnames "jss-tests.lisp" (load-time-value *load-pathname*))))
  (defun cl-user::test-jss()
    (funcall (intern "QUICKLOAD" 'ql) :prove)
    (funcall (intern "RUN" 'prove) where)))

;; (asdf:defsystem :jss/tests
;;   :depends-on (jss)
;;   :components ((:module tests
;; 		:pathname "" 
;; 		:components ((:test-file "jss-tests"))
;; 		))
;;   :perform (test-op :after (op c)
;;                     (funcall (intern #.(string :run) :prove) c)))







   


