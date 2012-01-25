;;;; -*- Mode: LISP -*-
(in-package :asdf)

(defsystem :abcl-asdf
  :author "Mark Evenson"
  :version "0.5.0"
  :depends-on (jss)
  :components 
  ((:module packages :pathname "" 
            :components
            ((:file "packages")))
   (:module base :pathname "" 
            :components
            ((:file "abcl-asdf")
             (:file "asdf-jar" 
                    :depends-on ("abcl-asdf"))
             (:file "maven-embedder" 
                    :depends-on ("abcl-asdf" "asdf-jar")))
            :depends-on (packages))))

(defsystem :abcl-asdf-test
  :author "Mark Evenson"
  :depends-on (abcl abcl-test-lisp)
  :components
  ((:module tests :components
            (#+nil (:file "example")
                   (:file "maven")))))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl-asdf))))
   "Invoke tests with (asdf:test-system 'abcl-asdf)."
   (asdf:load-system 'abcl-asdf-test)
   (funcall (intern (symbol-name 'run) 'abcl-asdf-test)))

