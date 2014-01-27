(require :asdf)

(asdf:defsystem jfli
  :version "0.2.0"
  :components ((:file "jfli")))

;;; Requires integration with IntelliJ IDEA editor (free download)
(asdf:defsystem jfli-intellij-tests
  :version "0.1.0"
  :description "<> asdf:defsystem <urn:abcl.org/release/1.3.0/contrib/jfli#0.2.0> ."
  :components ((:module test 
                       :components ((:file "yanking")))))
