(defsystem procyon
  :defsystem-depends-on (abcl-asdf)
  :homepage "https://bitbucket.org/mstrobel/procyon/wiki/Java%20Decompiler"
  :description "A Java decompiler by Mike Strobel"
  :version "0.6.0"
  :depends-on (alexandria) :components
  ((:module mvn-libs :components
            ((:mvn "org.bitbucket.mstrobel/procyon-compilertools/0.6.0")))
   (:module source
    :depends-on (mvn-libs)
    :pathname "" :components ((:file "procyon")))))






