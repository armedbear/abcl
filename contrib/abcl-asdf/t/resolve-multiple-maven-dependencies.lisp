(in-package :cl-user)

(prove:plan 3)

(let ((deps (abcl-asdf:resolve-multiple-maven-dependencies
             '("net.sourceforge.owlapi:org.semanticweb.hermit:1.3.8.413"
               "net.sourceforge.owlapi:owlapi-distribution:4.2.6"
               "net.sourceforge.owlapi/pellet-cli-ignazio1977/2.4.0-ignazio1977"
               "org.semanticweb.elk/elk-reasoner/0.4.3"
               "net.sourceforge.owlapi/owlexplanation/2.0.0")
             '("net.sourceforge.owlapi:owlapi-distribution:4.2.6")
             '("net.sourceforge.owlapi:owlapi-osgidistribution"
               "edu.stanford.protege:org.protege.editor.owl"))))
  (prove:is (length deps) 87)
  (prove:ok (not (find "owlapi-osgidistribution" deps :test 'search)))
  (prove:ok (not (find "protege" deps :test 'search))))

(prove:finalize)
