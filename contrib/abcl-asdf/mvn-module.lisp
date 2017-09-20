(in-package :abcl-asdf)

;;; <https://maven.apache.org/guides/introduction/introduction-to-optional-and-excludes-dependencies.html>
;;; If a artifact is root then its optional dependencies are
;; collected. If the same artifact is not root, then the optional
;;; dependencies are not collected. We don't need optionals since from
;;; our point of view we are the top pom and everything specified are
;;; dependencies
;;; Used by asdf-mvn-module.  
(defun resolve-multiple-maven-dependencies
    (dependencies &optional managed-dependencies exclusions (first-is-root nil))
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
  (let (aether)
    (with-aether (aether)
      (let ((collect-request (java:jnew (jss:find-java-class "CollectRequest")))
            (exclusions-collection (jss:new 'hashset) )
            (compile-scope (java:jfield (jss:find-java-class "JavaScopes") "COMPILE")))
	(#"addRepository" collect-request (ensure-remote-repository))
        (loop for e  in exclusions
           for (groupid artifactid) = (abcl-build:split-string e #\:)
           ;; If i have scope be compile-scope it doesn't get excluded!!
           for exclusion = (jss:new 'aether.graph.Exclusion groupid artifactid "" "jar")
           do (#"add" exclusions-collection exclusion))
        (loop for a in dependencies
           for artifact = (make-artifact (#"replaceAll" a "/" ":"))
           for dependency = (jss:new 'aether.graph.Dependency artifact compile-scope)
           do  
           ;; setExclusions returns a new dependency. We have to use
           ;; that. That passed dependency i not modified!
           ;; http://grepcode.com/file/repo1.maven.org/maven2/org.eclipse.aether/aether-api/1.0.2.v0150114/org/eclipse/aether/graph/Dependency.java#Dependency.getOptional%28%29
           ;; Nice of them to clearly document that :-/
	     (setq dependency (#"setExclusions" dependency exclusions-collection))
	     (if first-is-root
		 (#"setRoot" collect-request dependency)
		 (#"addDependency" collect-request dependency))
	     (setq first-is-root nil))
        (loop for a in managed-dependencies
           for artifact = (make-artifact (#"replaceAll" a "/" ":"))
           for dependency = (jss:new 'aether.graph.Dependency artifact compile-scope)
           do (setq dependency (#"setExclusions" dependency exclusions-collection))
	     (#"addManagedDependency" collect-request dependency))
        (let ((dependencies (#"collectDependencies" (ensure-repository-system) (ensure-session) collect-request))
              (nodelist-generator (jss:new 'PreorderNodeListGenerator))
              (dependency-request (jss:new 'DependencyRequest)))
          (#"setRoot" dependency-request (#"getRoot" dependencies))
          (#"resolveDependencies" (ensure-repository-system) (ensure-session) dependency-request)
          (#"accept" (#"getRoot" dependencies) nodelist-generator)
          (abcl-build:split-string (#"getClassPath" nodelist-generator) #\:))))))
