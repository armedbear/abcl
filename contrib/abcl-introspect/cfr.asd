(defsystem cfr
  :homepage "https://www.benf.org/other/cfr"
  :defsystem-depends-on (abcl-asdf)
  :version "0.152"
  :description "CFR - a Class File Reader decompiler" :components
  ((:module mvn-libs :components
            ((:mvn "org.benf/cfr/0.152")))
   (:module source
    :depends-on (mvn-libs)
    :pathname "" :components ((:file "cfr")))))


    
