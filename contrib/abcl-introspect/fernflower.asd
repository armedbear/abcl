(defsystem fernflower
  :homepage "https://github.com/fesh0r/fernflower"
  :description "An analytical decompiler for Java" :components
  ((:module mvn-libs :components
            ((:mvn "org.jboss.windup.decompiler.fernflower/fernflower/2.5.0.Final")))
   (:module source
    :depends-on (mvn-libs)
    :pathname "" :components
    ((:file "fernflower")))))


