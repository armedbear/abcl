;;;; Use the Aether system in a localy installed Maven3 distribution to download
;;;; and install JVM artifact dependencies.

#|

# Implementation 

Not necessarily multi-threaded safe, and unclear how much work that
would be, as it is unknown how the Maven implementation behaves.

## Installing Maven
http://maven.apache.org/download.html

## Current Javadoc for Maven Aether connector
http://sonatype.github.com/sonatype-aether/apidocs/overview-summary.html 

## Incomplete, seemingly often wrong
https://docs.sonatype.org/display/AETHER/Home 

Note that this is not an implementation of Maven per se, but the use
of the Maven Aether connector infrastructure.  Among other things, this means
that the Maven specific "~/.m2/settings.xml" file is NOT parsed for settings.

|#

;;; N.b. evaluated *after* we load the ABCL specific modifications of
;;;      ASDF in abcl-asdf.lisp

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

(defparameter *mavens* 
  (if (find :windows *features*)
      '("mvn.bat" "mvn3.bat")
      '("/opt/local/bin/mvn3" "mvn3" "mvn"))
  "Locations to search for the Maven executable.")

(defun find-mvn () 
  "Attempt to find a suitable Maven ('mvn') executable on the hosting operating system.

Returns the path of the Maven executable or nil if none are found.

Returns the version of Maven found as the second value.

Emits warnings if not able to find a suitable executable."

  (let ((m2-home (ext:getenv "M2_HOME"))
        (m2 (ext:getenv "M2"))
        (mvn-executable (if (find :unix *features*)
                               "mvn"
                               "mvn.bat")))
    (when (and m2-home (probe-file m2-home))
      (let* ((m2-home (truename m2-home))
             (mvn-path (merge-pathnames 
                        (format nil "bin/~A" mvn-executable)
                        m2-home))
             (mvn (truename mvn-path)))
        (if mvn
            (values (return-from find-mvn mvn)
                    (ensure-mvn-version))
            (warn "M2_HOME was set to '~A' in the process environment but '~A' doesn't exist." 
                  m2-home mvn-path))))
    (when (and m2 (probe-file m2))
      (let* ((m2 (truename m2))
             (mvn-path (merge-pathnames mvn-executable m2))
             (mvn (truename mvn-path)))
        (if mvn
            (values (return-from find-mvn mvn)
                    (ensure-mvn-version))
            (warn "M2 was set to '~A' in the process environment but '~A' doesn't exist." 
                  m2 mvn-path))))
    (let* ((which-cmd 
            (if (find :unix *features*)
                "which" 
                ;; Starting with Windows Server 2003
                "where.exe"))
           (which-cmd-p 
            (handler-case 
                (sys::run-program which-cmd nil)
              (t () nil))))
      (when which-cmd-p
        (dolist (mvn-path *mavens*)
          (let ((mvn 
                 (handler-case 
                     (truename (read-line (sys::process-output 
                                           (sys::run-program 
                                            which-cmd `(,mvn-path))))) 
                   (end-of-file () nil)
                   (t (e) 
                     (format *maven-verbose* 
                             "~&Failed to find Maven executable '~A' in PATH because~&~A" 
                             mvn-path e)))))
            (when mvn
              (return-from find-mvn mvn)))))))
  (warn "Unable to locate Maven executable to find Maven Aether adaptors."))

(defun find-mvn-libs ()
  (unless (find-mvn)
    (warn "Failed to find Maven executable to determine Aether library location."))
  (some 
   (lambda (d)
     (when (directory (merge-pathnames "maven-core-*.jar" d))
       (truename d)))
   (list (make-pathname :defaults (merge-pathnames "../lib/" (find-mvn))
                        :name nil :type nil)
         #p"/usr/local/share/java/maven3/lib/" ;; FreeBSD ports
         #p"/usr/local/maven/lib/"))) ;; OpenBSD location suggested by Timo Myyrä
                                  
(defparameter *mvn-libs-directory*
  nil
  "Location of 'maven-core-3.<m>.<p>.jar', 'maven-embedder-3.<m>.<p>.jar' etc.")

(defun mvn-version ()
  "Return the Maven version used by the Aether connector located by
  FIND-MVN as a list of (MAJOR MINOR PATHLEVEL) integers.

Signals a simple-error with additional information if this attempt fails."
  (handler-case 
      (let* ((process (sys:run-program (truename (find-mvn)) '("-version")))
             (output (sys:process-output process))
             (pattern (#"compile"
                       'regex.Pattern
                       "Apache Maven ([0-9]+)\\.([0-9]+)\\.([0-9]+)"))
             lines)
        (do ((line (read-line output nil :eof) 
                   (read-line output nil :eof)))
            ((or (not line) (eq line :eof)) nil)
          (push line lines)
          (let ((matcher (#"matcher" pattern line)))
            (when (#"find" matcher)
              (return-from mvn-version
                (mapcar #'parse-integer 
                        `(,(#"group" matcher 1) 
                           ,(#"group" matcher 2) 
                           ,(#"group" matcher 3)))))))
        (when lines 
          (signal "No parseable Maven version found in ~{~&  ~A~}" (nreverse lines)))
        (let ((error (sys:process-error process)))
          (do ((line (read-line error nil :eof) 
                     (read-line error nil :eof)))
              ((or (not line) (eq line :eof)) nil)
            (push line lines)
            (signal "Invocation of Maven returned the error ~{~&  ~A~}" (nreverse lines)))))
    (t (e) 
      (error "Failed to determine Maven version: ~A." e))))

(defun ensure-mvn-version ()
  "Return t if Maven version is 3.0.3 or greater."
  (let* ((version (mvn-version))
         (major (first version))
         (minor (second version))
         (patch (third version)))
    (values
     (or 
      (and (>= major 3)
           (>= minor 1))
      (and (>= major 3)
           (>= minor 0)
           (>= patch 3)))
     (list major minor patch))))

(defparameter *init* nil)

(defun init (&optional &key (force nil))
 "Run the initialization strategy to bootstrap a Maven dependency node.

Set *MVN-LIBS-DIRECTORY* to an explicit value before running this
function in order to bypass the dynamic introspection of the location
of the mvn executable with an explicit value."
 (unless (or force *mvn-libs-directory*)
   (setf *mvn-libs-directory* (find-mvn-libs)))
  (unless (and *mvn-libs-directory*
               (probe-file *mvn-libs-directory*))
   (error "Please obtain and install maven-3.0.4 or lates locally from <http://maven.apache.org/download.html>, then set ABCL-ASDF:*MVN-LIBS-DIRECTORY* to the directory containing maven-core-3.*.jar et. al."))
 (unless (ensure-mvn-version)
   (error "We need maven-3.0.4 or later."))  (add-directory-jars-to-class-path *mvn-libs-directory* nil)
  (setf *init* t))

(defun find-http-wagon ()
  "Find an implementation of the object that provides access to http and https resources.

Supposedly configurable with the java.net.protocols (c.f. reference
maso2000 in the Manual.)"
  (handler-case 
      ;; maven-3.0.4
      (java:jnew "org.apache.maven.wagon.providers.http.HttpWagon") 
    (error () 
      ;; maven-3.0.3 reported as not working with all needed functionality
      (java:jnew  "org.apache.maven.wagon.providers.http.LightweightHttpWagon"))))

(defun make-wagon-provider ()
  "Returns an implementation of the org.sonatype.aether.connector.wagon.WagonProvider contract.

The implementation is specified as Lisp closures.  Currently, it only
specializes the lookup() method if passed an 'http' or an 'https' role
hint."
  (unless *init* (init))
  (java:jinterface-implementation 
   (#"getName" 
    (or
     (ignore-errors  ;; Maven 3.1.0+
       (jss:find-java-class 'aether.connector.wagon.WagonProvider))
     (ignore-errors  ;; Maven 3.0.x
      (jss:find-java-class 'org.sonatype.aether.connector.wagon.WagonProvider))))
   "lookup"
   (lambda (role-hint)
     (cond 
       ((find role-hint '("http" "https") :test #'string-equal)
        (find-http-wagon))
       (t
        (progn 
          (format *maven-verbose* 
                  "~&WagonProvider stub passed '~A' as a hint it couldn't satisfy.~%" role-hint)
           java:+null+))))
   "release"
   (lambda (wagon)
     (declare (ignore wagon)))))

(defun find-service-locator ()
  (or 
   (ignore-errors 
     (#"newServiceLocator" 'org.apache.maven.repository.internal.MavenRepositorySystemUtils)) ;; maven-3.1.0
   (ignore-errors
      (java:jnew "org.apache.maven.repository.internal.MavenServiceLocator")) ;; maven-3.0.4
   (ignore-errors
     (java:jnew "org.apache.maven.repository.internal.DefaultServiceLocator"))
   (ignore-errors  ;; maven-3.1.0 using org.eclipse.aether...
     (jss:find-java-class 'aether.impl.DefaultServiceLocator))))

(defun make-repository-system ()
  (unless *init* (init))
  (let ((locator 
         (find-service-locator))
        (wagon-provider-class 
	 (or 
	  (ignore-errors 
	    (java:jclass "org.sonatype.aether.connector.wagon.WagonProvider"))
	  (ignore-errors  ;; Maven-3.1.x 
	    (jss:find-java-class 'aether.connector.wagon.WagonProvider))))
        (wagon-repository-connector-factory-class
	 (or 
	  (ignore-errors 
	    (java:jclass "org.sonatype.aether.connector.wagon.WagonRepositoryConnectorFactory"))
	  (ignore-errors 
	    (jss:find-java-class 'aether.connector.wagon.WagonRepositoryConnectorFactory))))
        (repository-connector-factory-class 
	 (or 
	  (ignore-errors 
	    (java:jclass "org.sonatype.aether.spi.connector.RepositoryConnectorFactory"))
	  (ignore-errors
	    (jss:find-java-class 'aether.spi.connector.RepositoryConnectorFactory))))
        (repository-system-class
	 (or
	  (ignore-errors
	    (java:jclass "org.sonatype.aether.RepositorySystem"))
	  (ignore-errors 
	    (jss:find-java-class 'aether.RepositorySystem)))))
    (#"setServices" locator
                    wagon-provider-class
                   (java:jarray-from-list
                    (list (make-wagon-provider))))
    (#"addService" locator
                   repository-connector-factory-class
                   wagon-repository-connector-factory-class)
    (values (#"getService" locator
                           repository-system-class)
            locator)))
        
(defun make-session (repository-system)
  "Construct a new aether.RepositorySystemSession from the specified REPOSITORY-SYSTEM."
  (let ((session
	 (or 
	  (ignore-errors (#"newSession" 'org.apache.maven.repository.internal.MavenRepositorySystemUtils))
	  (ignore-errors (java:jnew (jss:find-java-class "MavenRepositorySystemSession")))))
        (local-repository 
         (java:jnew (jss:find-java-class "LocalRepository")
                  (namestring (merge-pathnames ".m2/repository/"
                                               (user-homedir-pathname))))))
    (#"setLocalRepositoryManager" 
     session
     (or 
      (ignore-errors      ;; maven-3.1.0
	(#"newLocalRepositoryManager" 
	 repository-system session local-repository))
      (ignore-errors 
	(#"newLocalRepositoryManager" 
	 repository-system local-repository))))))

(defparameter *maven-http-proxy* nil
  "A string containing the URI of an http proxy for Maven to use.")

(defun make-proxy ()
  "Return an aether.repository.Proxy instance initialized from *MAVEN-HTTP-PROXY*."
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
    (or 
     (ignore-errors
       (jss:new 'org.eclipse.aether.repository.Proxy
		scheme host port authentication))
     (ignore-errors
       (jss:new 'org.sonatype.aether.repository.Proxy
		scheme host port authentication)))))

(defparameter *repository-system*  nil
  "The aether.RepositorySystem used by the Maeven Aether connector.")
(defun ensure-repository-system (&key (force nil))
  (when (or force (not *repository-system*))
    (setf *repository-system* (make-repository-system)))
  *repository-system*)

(defparameter *session* nil
  "Reference to the Maven RepositorySystemSession")
(defun ensure-session (&key (force nil))
  "Ensure that the RepositorySystemSession has been created.

If *MAVEN-HTTP-PROXY* is non-nil, parse its value as the http proxy."
  (when (or force (not *session*))
    (ensure-repository-system :force force)
    (setf *session* (make-session *repository-system*))
    (#"setRepositoryListener" *session* (make-repository-listener))
    (when *maven-http-proxy*
      (let ((proxy (make-proxy)))
        (#"add" (#"getProxySelector" *session*)
                proxy 
                ;; A string specifying non proxy hosts, or null
                java:+null+))))
    *session*)

(defun make-artifact (artifact-string)
  "Return an instance of aether.artifact.DefaultArtifact initialized from ARTIFACT-STRING." 
  (or 
   (ignore-errors
     (jss:new "org.sonatype.aether.util.artifact.DefaultArtifact" artifact-string))
   (ignore-errors
     (jss:new 'aether.artifact.DefaultArtifact artifact-string))))

(defun make-artifact-request () 
  "Construct a new aether.resolution.ArtifactRequest."
  (or 
   (ignore-errors
     (java:jnew (jss:find-java-class 'aether.resolution.ArtifactRequest)))
   (ignore-errors
     (java:jnew "org.sonatype.aether.resolution.ArtifactRequest"))))

;;; TODO change this to work on artifact strings like log4j:log4j:jar:1.2.16
(defun resolve-artifact (group-id artifact-id &key (version "LATEST" versionp))
  "Resolve artifact to location on the local filesystem.

Declared dependencies are not attempted to be located.

If unspecified, the string \"LATEST\" will be used for the VERSION.

Returns the Maven specific string for the artifact "
  (unless versionp
    (warn "Using LATEST for unspecified version."))
  (unless *init* (init))
  (let* ((artifact-string 
	  (format nil "~A:~A:~A" group-id artifact-id version))
         (artifact 
	  (make-artifact artifact-string))
         (artifact-request 
	  (make-artifact-request)))
    (#"setArtifact" artifact-request artifact)
    (#"addRepository" artifact-request (ensure-remote-repository))
    (#"toString" (#"getFile" 
                  (#"getArtifact" (#"resolveArtifact" (ensure-repository-system) 
                                                      (ensure-session) artifact-request))))))

(defun make-remote-repository (id type url) 
  (or 
   (ignore-errors 
     (#"build" (jss:new "org.eclipse.aether.repository.RemoteRepository$Builder" id type url)))
   (ignore-errors
     (jss:new 'aether.repository.RemoteRepository id type url))))

(defparameter *default-repository* 
   "http://repo1.maven.org/maven2/")

(defun add-repository (repository)
  (ensure-remote-repository :repository repository))

(defparameter *maven-remote-repository*  nil
    "The remote repository used by the Maven Aether embedder.")
(defun ensure-remote-repository (&key 
                                   (force nil)
                                   (repository *default-repository* repository-p))
  (unless *init* (init))
  (when (or force  
            repository-p 
            (not *maven-remote-repository*))
    (let ((r (make-remote-repository "central" "default" repository)))
      (when *maven-http-proxy*
        (#"setProxy" r (make-proxy)))
      (setf *maven-remote-repository* r)))
  *maven-remote-repository*)


(defun resolve-dependencies (group-id artifact-id 
                             &key
                             (version "LATEST" versionp)
                             (repository *maven-remote-repository* repository-p))
  "Dynamically resolve Maven dependencies for item with GROUP-ID and ARTIFACT-ID 
optionally with a VERSION and a REPOSITORY.  Users of the function are advised 

All recursive dependencies will be visited before resolution is successful.

If unspecified, the string \"LATEST\" will be used for the VERSION.

Returns a string containing the necessary jvm classpath entries packed
in Java CLASSPATH representation."
  (unless *init* (init))
  (unless versionp
    (warn "Using LATEST for unspecified version."))
  (let* ((coords 
	  (format nil "~A:~A:~A" group-id artifact-id (if versionp version "LATEST")))
	 (artifact 
	  (make-artifact coords))
         (dependency 
          (java:jnew (jss:find-java-class 'aether.graph.Dependency)
		     artifact (java:jfield (jss:find-java-class "JavaScopes") "RUNTIME")))
         (collect-request (java:jnew (jss:find-java-class "CollectRequest"))))
    (#"setRoot" collect-request dependency)
    (#"addRepository" collect-request 
                      (if repository-p
                          (ensure-remote-repository :repository repository)
                          (ensure-remote-repository)))
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
     (#"getName" (jss:find-java-class 'aether.RepositoryListener))
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

         
(defmethod resolve ((string string))
  "Resolve a colon separated GROUP-ID:ARTIFACT-ID[:VERSION] reference to a Maven artifact.

Examples of artifact references: \"log4j:log4j:1.2.14\" for
'log4j-1.2.14.jar'.  Resolving \"log4j:log4j\" would return the latest
version of the artifact known to the distributed Maven pom.xml graph.

Returns a string containing the necessary classpath entries for this
artifact and all of its transitive dependencies."
  (let ((result (split-string string ":")))
    (cond 
      ((= (length result) 3)
       (resolve-dependencies 
        (first result) (second result) :version (third result)))
      ((string= string "com.sun.jna:jna")
       (warn "Replacing request for no longer available com.sun.jna:jna with net.java.dev.jna:jna")
       (resolve-dependencies "net.java.dev.jna" "jna" :version "LATEST"))
      ((= (length result) 2)
       (resolve-dependencies
        (first result) (second result)))
      (t 
       (destructuring-bind (group-id artifact-id &optional version repository)
           (split-string string "/")
         (setf result 
               (apply #'resolve-dependencies group-id artifact-id
                      (append (when version
                                `(:version ,version))
                              (when repository
                                `(:repository ,repository))))))))))
  
;;; Currently the last file listed in ASDF
(provide 'abcl-asdf)
