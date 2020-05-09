(defsystem procyon
  :version "4.3.1"
  :homepage "https://bitbucket.org/mstrobel/procyon/wiki/Java%20Decompiler"
  :depends-on (alexandria) :components
  ((:module mvn-libs :components
            ((:mvn "org.jboss.windup.decompiler/decompile-procyon/4.3.1.Final")))
   (:module source
    :depends-on (mvn-libs) :components
    ((:file "procyon")))))






