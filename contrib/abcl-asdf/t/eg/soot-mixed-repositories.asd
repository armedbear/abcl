(defsystem #:soot-mixed-repositories
  :defsystem-depends-on (#:jss #:abcl-asdf)
  :components ((:mvn "ca.mcgill.sable/soot/3.0.0-20170622.230711-112"
                :repository "http://repo1.maven.org/maven2/"
                :repositories ("https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-snapshot/")
                :classname "soot.SootClass")))
