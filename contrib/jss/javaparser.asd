(defsystem javaparser
  :description "https://github.com/javaparser/javaparser"
  :defsystem-depends-on (abcl-asdf)
  :components
  ((:module jar
            :components ((:mvn "com.github.javaparser/javaparser-core/3.0.1")))
   (:module source :depends-on (jar)
            :pathname ""
            :serial t
            :components ((:file "javaparser")
                         (:file "read-sharp-quote-expression"))))
  :perform (asdf:test-op (op c)
                         (asdf:test-system :javaparser-tests)))


