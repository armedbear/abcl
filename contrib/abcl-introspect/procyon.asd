(defsystem procyon
  :homepage "https://bitbucket.org/mstrobel/procyon/wiki/Java%20Decompiler"
  :description "A Java decompiler by Mike Strobel"
  :version "0.5.26"
  :depends-on (alexandria) :components
  ((:module mvn-libs :components
            ((:mvn "org.bitbucket.mstrobel/procyon-compilertools/0.5.36")))
   (:module source
    :depends-on (mvn-libs)
    :pathname "" :components
    ((:file "procyon")))))






