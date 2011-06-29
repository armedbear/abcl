;;; Use the Aether system in a default maven distribution to download
;;; and install dependencies.
;;;
;;; https://docs.sonatype.org/display/AETHER/Home
;;;

(in-package :abcl-asdf)

(require :abcl-contrib)
(require :jss)

#| 
Test:
(resolve-dependencies "org.slf4j" "slf4j-api" "1.6.1")

(resolve-dependencies "org.apache.maven" "maven-aether-provider" "3.0.3")
|#

(defvar *mavens* '("/opt/local/bin/mvn3" "mvn3" "mvn" "mvn.bat")
  "Locations to search for the Maven executable.")

(defun find-mvn () 
  (dolist (mvn-path *mavens*)
    (let ((mvn 
           (handler-case 
               (truename (read-line (sys::process-output 
                                     (sys::run-program "which" `(,mvn-path)))))
             (end-of-file () nil))))
      (when mvn
        (return-from find-mvn mvn)))))

(defun find-mvn-libs ()
  (let ((mvn (find-mvn)))
    (unless mvn
      (warn "Failed to find Maven3 libraries.")
      (return-from find-mvn-libs))
    (truename (make-pathname 
               :defaults (merge-pathnames "../lib/" mvn)
               :name nil :type nil))))

(defparameter *mvn-libs-directory*
  nil
  "Location of 'maven-core-3.<m>.<p>.jar', 'maven-embedder-3.<m>.<p>.jar' etc.")

(defun mvn-version ()
  (let* ((line
         (read-line (sys::process-output 
                     (sys::run-program 
                      (namestring (find-mvn)) '("-version")))))
         (pattern (#"compile" 
                   'regex.Pattern
                   "Apache Maven ([0-9]+)\\.([0-9]+)\\.([0-9]+)"))
         (matcher (#"matcher" pattern line))
         (found (#"find" matcher)))
    (unless found 
      (return-from mvn-version nil))
    (mapcar #'parse-integer
            `(,(#"group" matcher 1)
              ,(#"group" matcher 2)
              ,(#"group" matcher 3)))))

(defun ensure-mvn-version ()
  "Return t if Maven version is 3.0.3 or greater."
  (let* ((version (mvn-version))
         (major (first version))
         (minor (second version))
         (patch (third version)))
    (or 
     (and (>= major 3)
          (>= minor 1))
     (and (>= major 3)
          (>= major 0)
          (>= patch 3)))))

(defparameter *init* nil)

(defun init ()
  (unless *mvn-libs-directory*
    (setf *mvn-libs-directory* (find-mvn-libs)))
  (unless (probe-file *mvn-libs-directory*)
    (error "You must download maven-3.0.3 from http://maven.apache.org/download.html, then set ABCL-ASDF:*MVN-DIRECTORY* appropiately."))
  (unless (ensure-mvn-version)
    (error "We need maven-3.0.3 or later."))
  (jss:add-directory-jars-to-class-path *mvn-libs-directory* nil)
  (setf *init* t))

(defun make-wagon-provider ()
  (unless *init* (init))
  (java:jinterface-implementation 
   "org.sonatype.aether.connector.wagon.WagonProvider"
   "lookup"
   (lambda (role-hint)
     (if (string-equal "http" role-hint)
       (java:jnew "org.apache.maven.wagon.providers.http.LightweightHttpWagon")
       java:+null+))
   "release"
   (lambda (wagon)
     (declare (ignore wagon)))))

(defun repository-system ()
  (unless *init* (init))
  (let ((locator 
         (java:jnew "org.apache.maven.repository.internal.DefaultServiceLocator"))
        (repository-connector-factory-class 
         (java:jclass "org.sonatype.aether.spi.connector.RepositoryConnectorFactory"))
        (wagon-repository-connector-factory-class
         (java:jclass "org.sonatype.aether.connector.wagon.WagonRepositoryConnectorFactory"))
        (wagon-provider-class 
         (java:jclass "org.sonatype.aether.connector.wagon.WagonProvider"))
        (repository-system-class
         (java:jclass "org.sonatype.aether.RepositorySystem")))
    (#"addService" locator
                   repository-connector-factory-class 
                   wagon-repository-connector-factory-class)
    (#"setServices" locator
                    wagon-provider-class
                    (java:jnew-array-from-list 
                     "org.sonatype.aether.connector.wagon.WagonProvider"
                     (list 
                      (make-wagon-provider))))
    (#"getService" locator
                   repository-system-class)))
        
(defun new-session (repository-system)
  (let ((session 
         (java:jnew (jss:find-java-class "MavenRepositorySystemSession")))
        (local-repository 
         (java:jnew (jss:find-java-class "LocalRepository")
                  (namestring (merge-pathnames ".m2/repository/"
                                               (user-homedir-pathname))))))
    (#"setLocalRepositoryManager" 
     session
     (#"newLocalRepositoryManager" repository-system local-repository))))

(defun resolve-artifact (group-id artifact-id version)
  (let* ((system 
          (repository-system))
         (session 
          (new-session system))
         (repository 
          (jss:new "org.sonatype.aether.repository.RemoteRepository"
                   "central" "default" "http://repo1.maven.org/maven2/"))
         (artifact-string (format nil "~A:~A:~A"
                                  group-id artifact-id version))
         (artifact 
          (jss:new "org.sonatype.aether.util.artifact.DefaultArtifact" artifact-string))
         (artifact-request 
          (java:jnew "org.sonatype.aether.resolution.ArtifactRequest")))
    (#"setArtifact" artifact-request artifact)
    (#"addRepository" artifact-request repository)
    (#"resolveArtifact" system session artifact-request)))

(defun resolve-dependencies (group-id artifact-id version)
  (unless *init* (init))
  (let* ((system 
          (repository-system))
         (session 
          (new-session system))
         (artifact
          (java:jnew (jss:find-java-class "aether.util.artifact.DefaultArtifact")
                     (format nil "~A:~A:~A"
                             group-id artifact-id version)))
         (dependency 
          (java:jnew (jss:find-java-class "aether.graph.Dependency")
                     artifact "compile"))
         (central
          (java:jnew (jss:find-java-class "RemoteRepository")
                     "central" "default" "http://repo1.maven.org/maven2/"))
         (collect-request (java:jnew (jss:find-java-class "CollectRequest"))))
    (#"setRoot" collect-request dependency)
    (#"addRepository" collect-request central)
    (let* ((node 
            (#"getRoot" (#"collectDependencies" system session collect-request)))
           (dependency-request 
            (java:jnew (jss:find-java-class "DependencyRequest")
                       node java:+null+))
           (nlg 
            (java:jnew (jss:find-java-class "PreorderNodeListGenerator"))))
      (#"resolveDependencies" system session dependency-request)
      (#"accept" node nlg)
      (#"getClassPath" nlg))))




         

