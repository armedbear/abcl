;; https://maven.apache.org/guides/introduction/introduction-to-optional-and-excludes-dependencies.html
;; if a artifact is root then its optional dependencies are
;; collected. If the same artifact is not root, then the optional
;; dependencies are not collected. We don't need optionals since from
;; our point of view we are the top pom and everything specified are
;; dependencies

(defun resolve-multiple-maven-dependencies (dependencies &optional managed-dependencies exclusions (first-is-root nil))
  "Return a list of jar file paths that satisfy dependencies

 dependencies: a list of maven artifacts. color or slash separated
   components groupid:artifactid:versionid

 managed-dependencies: a list of maven artifacts. If an dependency
   with same groupid and artifactid are encountered, the version
   specified here overrides.

 exclusions: a list of partial maven artifacts
   groupid:artifactid. Dependencies with same groupid and artifactid are
   exluded

 first-is-root: If the first dependency should include optional
   dependencies, set this to t. Usually not.
 "
  (let ((collect-request (java:jnew (jss:find-java-class "CollectRequest")))
	(exclusions-collection (new 'hashset) )
	(compile-scope #"JavaScopes.COMPILE"))
    (loop for e  in exclusions
	  for (groupid artifactid) = (split-at-char e #\:)
	  ;; If i have scope be compile-scope it doesn't get excluded!!
	  for exclusion = (new 'aether.graph.Exclusion groupid artifactid "" "jar")
	  do (#"add" exclusions-collection exclusion))
    (loop for a in dependencies
	  for artifact = (abcl-asdf::make-artifact (#"replaceAll" a "/" ":"))
	  for dependency = (new 'aether.graph.Dependency artifact compile-scope)
	  do  
	     ;; setExclusions returns a new dependency. We have to use that. That passed dependency is not modified!
	     ;; http://grepcode.com/file/repo1.maven.org/maven2/org.eclipse.aether/aether-api/1.0.2.v20150114/org/eclipse/aether/graph/Dependency.java#Dependency.getOptional%28%29
	     ;; Nice of them to clearly document that :-/
	     (setq dependency (#"setExclusions" dependency exclusions-collection))
	     (if first-is-root
		 (#"setRoot" collect-request dependency)
		 (#"addDependency" collect-request dependency))
	     (setq first-is-root nil))
    (loop for a in managed-dependencies
	  for artifact = (abcl-asdf::make-artifact (#"replaceAll" a "/" ":"))
	  for dependency = (new 'aether.graph.Dependency artifact compile-scope)
	  do (setq dependency (#"setExclusions" dependency exclusions-collection))
	     (#"addManagedDependency" collect-request dependency))
    (let ((dependencies (#"collectDependencies"
			 (abcl-asdf::ensure-repository-system)
			 (abcl-asdf::ensure-session) collect-request))
	  (nodelist-generator (new 'PreorderNodeListGenerator))
	  (dependency-request (new 'DependencyRequest)))
      (#"setRoot" dependency-request (#"getRoot" dependencies))
      (#"resolveDependencies" (abcl-asdf::ensure-repository-system) (abcl-asdf::ensure-session) dependency-request)
      (#"accept" (#"getRoot" dependencies) nodelist-generator)
      (split-at-char (#"getClassPath" nodelist-generator) #\:)
      )))



(prove:plan 3)
(let ((deps  (resolve-multiple-maven-dependencies '("net.sourceforge.owlapi:org.semanticweb.hermit:1.3.8.413"
				     "net.sourceforge.owlapi:owlapi-distribution:4.2.6"
				     "net.sourceforge.owlapi/pellet-cli-ignazio1977/2.4.0-ignazio1977"
				     "org.semanticweb.elk/elk-reasoner/0.4.3"
				     "net.sourceforge.owlapi/owlexplanation/2.0.0")
				   '("net.sourceforge.owlapi:owlapi-distribution:4.2.6")
				   '("net.sourceforge.owlapi:owlapi-osgidistribution"
				     "edu.stanford.protege:org.protege.editor.owl")
				   )))
  (prove:ok (= (length deps) 87))
  (prove:ok (not (find "owlapi-osgidistribution" deps :test 'search)))
  (prove:ok (not (find "protege" deps :test 'search))))
(prove:finalize)


