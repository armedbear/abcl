;;; From https://github.com/alanruttenberg/lsw2/blob/owlapiv4/owl2/owl2libs-mvn2.asd

(defsystem test-mvn-module
  :description "Non-Lisp dependencies necessary for OWL to function."
  :defsystem-depends-on (asdf-mvn-module)
  :components
  ((:mvn-module maven
		:dependencies 
		("net.sourceforge.owlapi/pellet-cli-ignazio1977/2.4.0-ignazio1977"
		  "org.semanticweb.elk/elk-owlapi/0.4.3"
		  "net.sourceforge.owlapi/org.semanticweb.hermit/1.3.8.413"
		  "net.sourceforge.owlapi/owlapi-distribution/4.2.6"
		  "net.sourceforge.owlapi/owlexplanation/2.0.0"
		  "de.sciss/prefuse-core/1.0.1"
		  "de.sciss/prefuse-demos/1.0.1")
		:managed-dependencies
		("org.slf4j/slf4j-api/1.7.21"
		 "net.sourceforge.owlapi:owlapi-distribution:4.2.6")
		:exclusions
		("net.sourceforge.owlapi:owlapi-osgidistribution"
		 "edu.stanford.protege:org.protege.editor.owl"))
   #+(or)
   (:module rest :pathname "lib" :components
            ((:bundle "uk.ac.manchester.cs.owl.factplusplus-1.6.5")
	     (:jar-file "LSWTreeview-1.0.0")
	     (:jar-file "QuotedStringAnnotationVisitor-1.0.0")))
   (:module lib :pathname "lib"
    :depends-on (maven #+(or) rest)))
    :perform (load-op :after (o c)
		      (progn
			(#"configure" 'org.apache.log4j.BasicConfigurator (jss::new 'NullAppender))
			(print "configured log4j"))))

