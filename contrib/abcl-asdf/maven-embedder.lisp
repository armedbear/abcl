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

(resolve-dependencies "org.apache.maven" "maven-aether-provider" "3.0.4")
|#

(defvar *mavens* '("/opt/local/bin/mvn3" "mvn3" "mvn" "mvn.bat")
  "Locations to search for the Maven executable.")

(defun find-mvn () 
  "Attempt to find a suitable Maven ('mvn') executable on the hosting operating system."
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
      (return-from find-mvn-libs nil))
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
  "Run the initialization strategy to bootstrap a Maven dependency node."
  (unless *mvn-libs-directory*
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

(defparameter *session* nil
  "Reference to the Maven RepositorySystemSession")

(defparameter *maven-http-proxy* nil
  "A string containing the URI of an http proxy for Maven to use.")

(defparameter *repository-system* nil)

(defun ensure-repository-system ()
  (unless *repository-system*
    (setf *repository-system* (repository-system)))
  *repository-system*)

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

(defun ensure-session ()
  "Ensure that the RepositorySystemSession has been created.

If *MAVEN-HTTP-PROXY* is non-nil, parse its value as the http proxy."
  (unless *session*
    (ensure-repository-system)
    (setf *session* (new-session *repository-system*))
    (#"setRepositoryListener" *session* (make-repository-listener))
    (when *maven-http-proxy*
      (let ((proxy (make-proxy)))
        (#"add" (#"getProxySelector" *session*)
                proxy 
                ;; A string specifying non proxy hosts, or null
                java:+null+))))
    *session*)
    

(defun resolve-artifact (group-id artifact-id &optional (version "LATEST" versionp))
  "Directly resolve Maven dependencies for item with GROUP-ID and ARTIFACT-ID at VERSION, ignoring dependencies.

Declared dependencies are not attempted to be located.

If unspecified, the string \"LATEST\" will be used for the VERSION.

Returns a string containing the necessary jvm classpath entries packed
in Java CLASSPATH representation."

  (unless versionp
    (warn "Using LATEST for unspecified version."))
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

(defparameter *aether-remote-repository* nil) ;;; TODO

(defun resolve-dependencies (group-id artifact-id &optional (version "LATEST" versionp))
  "Dynamically resolve Maven dependencies for item with GROUP-ID and ARTIFACT-ID at VERSION.

All recursive dependencies will be visited before resolution is successful.

If unspecified, the string \"LATEST\" will be used for the VERSION.

Returns a string containing the necessary jvm classpath entries packed
in Java CLASSPATH representation."
  (unless *init* (init))
  (unless versionp
    (warn "Using LATEST for unspecified version."))
  (let* ;;((system 
        ;; (repository-system))
        ;; (session 
        ;;  (new-session system))
         ((artifact
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
    (when *maven-http-proxy*
      (#"setProxy" central (make-proxy)))
    (#"addRepository" collect-request central)
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

(defparameter *maven-verbose* t
  "Stream to send output from the Maven Aether subsystem to, or NIL to muffle output")

(defun make-repository-listener ()
  ;;; XXX why does the (flet ((log (e) ...)) (java:jinterface-implementation ...) version not work?
  (java:jinterface-implementation 
   "org.sonatype.aether.RepositoryListener"
   "artifactDeployed" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "artifactDeploying" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "artifactDescriptorInvalid" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "artifactDescriptorMissing" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "artifactDownloaded" (lambda (e) (format *maven-verbose*  "~&transfer-listener: ~A~%" (#"toString" e))) 
   "artifactDownloading" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "artifactInstalled" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e))) 
   "artifactInstalling" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "artifactResolved" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "artifactResolving" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e))) 
   "metadataDeployed" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e))) 
   "metadataDeploying" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e))) 
   "metadataDownloaded" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "metadataDownloading" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "metadataInstalled" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e))) 
   "metadataInstalling" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "metadataInvalid" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e))) 
   "metadataResolved" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))
   "metadataResolving" (lambda (e) (format *maven-verbose* "~&transfer-listener: ~A~%" (#"toString" e)))))

         

