(defsystem jad
  :description "Introspect runtime architecture, install appropiate JAD binary, use it."
  :depends-on (abcl-build)
  :components ((:file "jad")))

