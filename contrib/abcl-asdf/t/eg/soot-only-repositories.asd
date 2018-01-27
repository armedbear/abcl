(defsystem #:soot-only-repositories
  :defsystem-depends-on (#:jss #:abcl-asdf)
  :components ((:mvn "ca.mcgill.sable/soot/3.0.0-20170622.230711-112"
                :repositories ("https://soot-build.cs.uni-paderborn.de/nexus/repository/soot-snapshot/"
                               "http://repo1.maven.org/maven2/")
                :classname "soot.SootClass")))
