;;;; -*- Mode: LISP -*-

(asdf:defsystem :abcl-asdf
  :author "Mark Evenson"
  :version "0.9.2"
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

(asdf:defsystem :abcl-asdf-test
  :author "Mark Evenson"
  :depends-on (abcl abcl-test-lisp abcl-asdf)
  :components ((:module tests :serial t 
                        :components ((:file "example")
                                     (:file "maven")
                                     (:file "test")))))

#|
(defmethod perform ((o test-op) (c (eql (find-system 'abcl-asdf-test))))
  (funcall (intern (symbol-name 'run) 'abcl-asdf-test)))

(defmethod perform ((o test-op) (c (eql (find-system 'abcl-asdf))))
  (asdf:load-system :abcl-asdf-test))
  (asdf:test-system :abcl-asdf-test))

 ;;; FIXME
(defmethod perform ((o test-op) (c (eql (find-system 'abcl-asdf))))
   "Invoke tests with (asdf:test-system 'abcl-asdf)."
   (asdf:load-system 'abcl)
   (asdf:load-system 'abcl-test-lisp)
   (asdf:load-system 'abcl-asdf-test)
   (funcall (intern (symbol-name 'run) 'abcl-asdf-test)))
|#
