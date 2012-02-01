;;; Use the Aether system in a default Maven3 distribution to download
;;; and install dependencies.
;;;
;;; References:
;;; -- javadoc
;;; http://sonatype.github.com/sonatype-aether/apidocs/overview-summary.html 
;;; -- incomplete, seemingly often wrong
;;; https://docs.sonatype.org/display/AETHER/Home 

(in-package :abcl-asdf)

(require :abcl-contrib)
(require :jss)

#| 
Test:
(resolve-dependencies "org.slf4j" "slf4j-api" "1.6.1")

(resolve-dependencies "org.apache.maven" "maven-aether-provider" "3.0.4")
|#

(defparameter *maven-verbose* t
  "Stream to send output from the Maven Aether subsystem to, or NIL to muffle output")

(defvar *mavens* '("/opt/local/bin/mvn3" "mvn3" "mvn" "mvn3.bat" "mvn.bat")
  "Locations to search for the Maven executable.")

(defun find-mvn () 
  "Attempt to find a suitable Maven ('mvn') executable on the hosting operating system."
  (dolist (mvn-path *mavens*)
    (let ((mvn 
           (handler-case 
               (truename (read-line (sys::process-output 
                                     (sys::run-program "which" `(,mvn-path))))) ;; TODO equivalent for MSDOS
             (end-of-file () nil))))
      (when mvn
        (return-from find-mvn mvn)))))

(defun find-mvn-libs ()
  (let ((mvn (find-mvn)))
    (unless mvn
      (warn "Failed to find Maven3 libraries.")
      (return-from find-mvn-libs nil))
    (truename (make-pathname 
               :defaults (merge-pathnames "../lib/" mvn)
               :name nil :type nil))))

(defparameter *mvn-libs-directory*
  nil
  "Location of 'maven-core-3.<m>.<p>.jar', 'maven-embedder-3.<m>.<p>.jar' etc.")

(defun mvn-version ()
  "Return the Maven version used by the Aether connector."
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

(defun init (&optional &key (force nil))
  "Run the initialization strategy to bootstrap a Maven dependency node."
  (unless (or force *mvn-libs-directory*)
    (setf *mvn-libs-directory* (find-mvn-libs)))
  (unless (probe-file *mvn-libs-directory*)
    (error "You must download maven-3.0.3 or later from http://maven.apache.org/download.html, then set ABCL-ASDF:*MVN-DIRECTORY* appropiately."))
  (unless (ensure-mvn-version)
    (error "We need maven-3.0.3 or later."))
  (add-directory-jars-to-class-path *mvn-libs-directory* nil)
  (setf *init* t))

(defparameter *http-wagon-implementations*
  `("org.apache.maven.wagon.providers.http.HttpWagon" ;; introduced as default with maven-3.0.4
    "org.apache.maven.wagon.providers.http.LightweightHttpWagon")
  "A list of possible candidate implementations that provide access to http and https resources.

Supposedly configurable with the java.net.protocols (c.f. reference maso2000 in the Manual.")

(defun make-wagon-provider ()
  "Returns an implementation of the org.sonatype.aether.connector.wagon.WagonProvider contract.

The implementation is specified as Lisp closures.  Currently, it only
specializes the lookup() method if passed an 'http' role hint."
  (unless *init* (init))
  (java:jinterface-implementation 
   "org.sonatype.aether.connector.wagon.WagonProvider"
   "lookup"
   (lambda (role-hint)
     (if (string-equal "http" role-hint)
         (some (lambda (provider) (java:jnew provider)) *http-wagon-implementations*)
       java:+null+))
   "release"
   (lambda (wagon)
     (declare (ignore wagon)))))

(defun make-repository-system ()
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
        
(defun make-session (repository-system)
  "Construct a new org.sonatype.aether.RepositorySystemSession from REPOSITORY-SYSTEM"
  (let ((session 
         (java:jnew (jss:find-java-class "MavenRepositorySystemSession")))
        (local-repository 
         (java:jnew (jss:find-java-class "LocalRepository")
                  (namestring (merge-pathnames ".m2/repository/"
                                               (user-homedir-pathname))))))
    (#"setLocalRepositoryManager" 
     session
     (#"newLocalRepositoryManager" repository-system local-repository))))

(defparameter *maven-http-proxy* nil
  "A string containing the URI of an http proxy for Maven to use.")

(defun make-proxy ()
  "Return an org.sonatype.aether.repository.Proxy instance initialized form *MAVEN-HTTP-PROXY*."
  (unless *maven-http-proxy*
    (warn "No proxy specified in *MAVEN-HTTP-PROXY*")
    (return-from make-proxy nil))
  (let* ((p (pathname *maven-http-proxy*))
         (scheme (sys::url-pathname-scheme p))
         (authority (sys::url-pathname-authority p))
         (host (if (search ":" authority)
                   (subseq authority 0 (search ":" authority))
                   authority))
         (port (when (search ":" authority)
                 (parse-integer (subseq authority (1+ (search ":" authority))))))
         ;; TODO allow specification of authentication
         (authentication java:+null+))
    (jss:new 'org.sonatype.aether.repository.Proxy
             scheme host port authentication)))

(defparameter *repository-system*  nil
  "The org.sonatype.aether.RepositorySystem used by the Maeven Aether connector.")
(defun ensure-repository-system ()
  (unless *repository-system*
    (setf *repository-system* (make-repository-system)))
  *repository-system*)

(defparameter *session* nil
  "Reference to the Maven RepositorySystemSession")
(defun ensure-session ()
  "Ensure that the RepositorySystemSession has been created.

If *MAVEN-HTTP-PROXY* is non-nil, parse its value as the http proxy."
  (unless *session*
    (ensure-repository-system)
    (setf *session* (make-session *repository-system*))
    (#"setRepositoryListener" *session* (make-repository-listener))
    (when *maven-http-proxy*
      (let ((proxy (make-proxy)))
        (#"add" (#"getProxySelector" *session*)
                proxy 
                ;; A string specifying non proxy hosts, or null
                java:+null+))))
    *session*)

;;; TODO change this to work on artifact strings like log4j:log4j:jar:1.2.16
(defun resolve-artifact (group-id artifact-id &optional (version "LATEST" versionp))
  "Directly resolve Maven dependencies for item with GROUP-ID and ARTIFACT-ID at VERSION, ignoring dependencies.

Declared dependencies are not attempted to be located.

If unspecified, the string \"LATEST\" will be used for the VERSION.

Returns the Maven specific string for the artifact "
  (unless versionp
    (warn "Using LATEST for unspecified version."))
  (unless *init* (init))
  (let* ((artifact-string (format nil "~A:~A:~A" group-id artifact-id version))
         (artifact 
          (jss:new "org.sonatype.aether.util.artifact.DefaultArtifact" artifact-string))
         (artifact-request 
          (java:jnew "org.sonatype.aether.resolution.ArtifactRequest")))
    (#"setArtifact" artifact-request artifact)
    (#"addRepository" artifact-request (ensure-remote-repository))
    (#"toString" (#"resolveArtifact" (ensure-repository-system) (ensure-session) artifact-request))))

(defun make-remote-repository (id type url) 
  (jss:new 'aether.repository.RemoteRepository id type url))

(defparameter *maven-remote-repository*  nil
    "The remote repository used by the Maven Aether embedder.")
(defun ensure-remote-repository () 
  (unless *init* (init))
  (unless *maven-remote-repository*
    (let ((r (make-remote-repository "central" "default" "http://repo1.maven.org/maven2/")))
      (when *maven-http-proxy*
        (#"setProxy" r (make-proxy)))
      (setf *maven-remote-repository* r)))
  *maven-remote-repository*)

(defun resolve-dependencies (group-id artifact-id &optional (version "LATEST" versionp))
  "Dynamically resolve Maven dependencies for item with GROUP-ID and ARTIFACT-ID at VERSION.

All recursive dependencies will be visited before resolution is successful.

If unspecified, the string \"LATEST\" will be used for the VERSION.

Returns a string containing the necessary jvm classpath entries packed
in Java CLASSPATH representation."
  (unless *init* (init))
  (unless versionp
    (warn "Using LATEST for unspecified version."))
  (let* ((artifact
          (java:jnew (jss:find-java-class "aether.util.artifact.DefaultArtifact")
                     (format nil "~A:~A:~A"
                             group-id artifact-id version)))
         (dependency 
          (java:jnew (jss:find-java-class "aether.graph.Dependency")
                     artifact "compile"))
         (collect-request (java:jnew (jss:find-java-class "CollectRequest"))))
    (#"setRoot" collect-request dependency)
    (#"addRepository" collect-request (ensure-remote-repository))
    (let* ((node 
            (#"getRoot" (#"collectDependencies" (ensure-repository-system) (ensure-session) collect-request)))
           (dependency-request 
            (java:jnew (jss:find-java-class "DependencyRequest")
                       node java:+null+))
           (nlg 
            (java:jnew (jss:find-java-class "PreorderNodeListGenerator"))))
      (#"resolveDependencies" (ensure-repository-system) (ensure-session) dependency-request)
      (#"accept" node nlg)
      (#"getClassPath" nlg))))

(defun make-repository-listener ()
  (flet ((log (e) 
           (format *maven-verbose* "~&~A~%" (#"toString" e))))
    (java:jinterface-implementation 
     "org.sonatype.aether.RepositoryListener"
     "artifactDeployed" 
     #'log
     "artifactDeploying" 
     #'log
     "artifactDescriptorInvalid" 
     #'log
     "artifactDescriptorMissing" 
     #'log
     "artifactDownloaded" 
     #'log
     "artifactDownloading" 
     #'log
     "artifactInstalled" 
     #'log
     "artifactInstalling" 
     #'log
     "artifactResolved" 
     #'log
     "artifactResolving" 
     #'log
     "metadataDeployed" 
     #'log
     "metadataDeploying" 
     #'log
     "metadataDownloaded" 
     #'log
     "metadataDownloading" 
     #'log
     "metadataInstalled"
     #'log
     "metadataInstalling" 
     #'log
     "metadataInvalid" 
     #'log
     "metadataResolved" 
     #'log
     "metadataResolving"
     #'log)))

         

