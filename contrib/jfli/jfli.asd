(require :asdf)

(asdf:defsystem jfli
  :version "0.2.0"
  :components ((:file "jfli")))


;;; Requires integration with 
(asdf:defsystem jfli-intellij-tests
  :version "0.1.0"
  :components ((:module test 
                       :components ((:file "yanking")))))
