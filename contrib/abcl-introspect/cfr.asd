(defsystem cfr
  :homepage "https://www.benf.org/other/cfr"
  :description "CFR - a Class File Reader decompiler" :components
  ((:module mvn-libs :components
            ((:mvn "org.benf/cfr/0.149")))
   (:module source
    :depends-on (mvn-libs)
    :pathname "" :components
    ((:file "cfr")))))


    
