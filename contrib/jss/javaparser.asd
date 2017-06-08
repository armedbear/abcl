(asdf:defsystem :javaparser
  :description "https://github.com/javaparser/javaparser"
  :serial t
  :components ((:mvn "com.github.javaparser/javaparser-core/3.0.1")
	       (:file "javaparser")
	       (:file "read-sharp-quote-expression")))
