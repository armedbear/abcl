(require :asdf)
(asdf:defsystem jfli
  :version "0.1.0"
  :components ((:file "jfli")
               (:module test :components
                        ((:file "yanking")))))
