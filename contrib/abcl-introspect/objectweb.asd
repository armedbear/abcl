(defsystem objectweb
  :homepage "https://asm.ow2.org"
  :description "Disassembly to JVM byte code via Objectweb"
  :version "9.10.1"
  :defsystem-depends-on (abcl-asdf) :components
  ((:module maven :components
            ((:mvn "org.ow2.asm/asm-util/9.10.1")))
   (:module source
            :depends-on (maven)
            :pathname "" :components ((:file "objectweb")))))






