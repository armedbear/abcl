(defsystem objectweb
  :homepage "http://asm.ow2.org"
  :description "Disassembly to JVM byte code via Objectweb"
  :defsystem-depends-on (abcl-asdf)
  :components
  ((:module maven
            :components
            ((:mvn "org.ow2.asm/asm-all/5.2")))
   (:module source
            :depends-on (maven)
            :pathname ""
            :components
            ((:file "objectweb")))))






