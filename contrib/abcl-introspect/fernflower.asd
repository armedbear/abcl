(defsystem fernflower
  :depends-on (alexandria abcl-introspect)
  :homepage "https://github.com/fesh0r/fernflower"
  :version "1.0.0.20271018"
  :description "An analytical decompiler for Java" :components
  ((:module mvn-libs :components
	    ((:mvn "org.jboss.windup.decompiler.fernflower/windup-fernflower/1.0.0.20171018")))
   (:module source
    :depends-on (mvn-libs)
    :pathname "" :components
    ((:file "fernflower")))))


