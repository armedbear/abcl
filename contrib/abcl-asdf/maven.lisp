;;;; Use the Aether system packaged as jar files in a locally
;;;; installed Maven3 distribution to download and install JVM
;;;; artifact dependencies.


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

#|

We aim to be compatible with the "current" version of Maven back to
maven-3.0.4.  The necessary internals of Maven are messy, and not very
well abstracted, especially in the earlier releases.  In maintaining
this code over the past decade, it has been the case that entire APIs
will disappear during what are advertised as "patchlevel" upgrades of
Maven.


|#

;;; N.b. evaluated *after* we load the ABCL specific modifications of
;;;      ASDF in abcl-asdf.lisp

(in-package :abcl-asdf)

(require :abcl-contrib)
(require :jss)

#| 
Test:
(abcl-asdf:resolve "org.slf4j:slf4j-api:1.6.1")

(abcl-asdf:resolve "org.apache.maven:maven-aether-provider:3.0.4")

(abcl-asdf:resolve "com.google.gwt:gwt-user")

|#

(defparameter *mavens* 
  (if (find :windows *features*)
      '("mvn" "mvn.bat" "mvn.cmd" "mvn3.bat")
      '("mvn" "mvn3"
        ;; MacPorts
        "/opt/local/bin/mvn" "/opt/local/bin/mvn3"))
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
    (let ((which-cmd 
            (if (find :unix *features*)
                "which" 
                ;; Starting with Windows Server 2003
                "where.exe")))
      (dolist (mvn-path *mavens*)
	(let ((mvn 
	       (handler-case
		   (truename 
		    (string-trim
		     '(#\space #\newline #\return #\tab)
		     (uiop:run-program
		      (format nil "~a ~a" which-cmd mvn-path)
		      :output :string)))
		 (t (e) 
		   (format cl:*load-verbose*
			   "~&; abcl-asdf; Failed to find Maven executable '~a' in PATH because~%~a" 
			   mvn-path e)))))
	  (when mvn
	    (return-from find-mvn mvn)))))
  (warn "Unable to locate Maven executable to find Maven Aether adaptors.")))

(defun find-mvn-libs ()
  (unless (find-mvn)
    (warn "Failed to find Maven executable to determine Aether library location.  Continuing anyways."))
  (some 
   (lambda (d)
     (when (and
            (pathnamep d)
            (directory (merge-pathnames "maven-core*.jar" d)))
       (truename d)))
   (list (ignore-errors
           (make-pathname :defaults (merge-pathnames "../lib/" (find-mvn))
                          :name nil :type nil))
         (ignore-errors
           (make-pathname :defaults (merge-pathnames "lib/" (mvn-home))
                          :name nil :type nil))
         ;; library location for homebrew maven package on OS X
         (ignore-errors
           (make-pathname :defaults (merge-pathnames "../libexec/lib/" (find-mvn))
                          :name nil :type nil))
         #p"/usr/local/share/java/maven3/lib/" ;; FreeBSD ports
         #p"/usr/local/maven/lib/"))) ;; OpenBSD location suggested by Timo Myyrä

(defparameter *mvn-libs-directory*
  nil
  "Location of 'maven-core-3.<m>.<p>.jar', 'maven-embedder-3.<m>.<p>.jar' etc.")

(defun normalize-mvn-libs ()
  "Ensure that any *mvn-libs-directory* is a both directory and a pathname"
  (unless *mvn-libs-directory*
    (return-from normalize-mvn-libs nil))
  (when (not (pathnamep *mvn-libs-directory*))
    (setf *mvn-libs-directory* (pathname *mvn-libs-directory*)))
  (when (not (#"endsWith" (namestring *mvn-libs-directory*) "/"))
    (setf *mvn-libs-directory*
          (pathname (concatenate 'string *mvn-libs-directory* "/"))))
  *mvn-libs-directory*)

(defun mvn-version ()
  "Return the version of Maven libaries in use"
  (unless (normalize-mvn-libs)
    (error "Need to specify a value of *mvn-libs-directory*"))
  (let* ((pattern
          "maven-core*.jar")
         (maven-core-jars
          (directory (merge-pathnames pattern
                                      *mvn-libs-directory*)))
         (maven-core-jar 
          (cond
            ((= (length maven-core-jars) 0)
             (error "No file matching '~a' found in '~a'." pattern *mvn-libs-directory*))
            ((> (length maven-core-jars) 1)
             (warn "More than one file matching '~a' found in '~a'."
                   pattern *mvn-libs-directory*)
             (first maven-core-jars))
            (t
             (first maven-core-jars)))))
    (let* ((manifest
            (#"getManifest" (jss:new 'java.util.jar.JarFile (namestring maven-core-jar))))
           (attributes
            (#"getMainAttributes" manifest))
           (version
            (#"getValue" attributes "Implementation-Version")))
      (parse-mvn-version
       version))))

;;; deprecated, unused:  we now get the version directly from the JAR manifest
(defun mvn-version-from-mvn-executable ()
  "Return the Maven version used by the Aether connector located by
  FIND-MVN as a list of (MAJOR MINOR PATHLEVEL) integers.

Signals a simple-error with additional information if this attempt fails."
  (handler-case
      (let* ((mvn
              (truename (find-mvn)))
             (pattern (#"compile"
                       'regex.Pattern
                       "^Apache Maven ([0-9]+\\.[0-9]+\\.[0-9]+)")))
        (multiple-value-bind (output error)
            (uiop:run-program
             (format nil "~a --version" mvn)
             :output :string :error :string)
          (let ((matcher (#"matcher" pattern output)))
            (when (#"find" matcher)
              (return-from mvn-version-from-mvn-executable
                (parse-mvn-version (#"group" matcher 1)))))
          (when output
            (signal "No parseable Maven version found in ~a" output))
          (signal "Invocation of Maven returned the error ~{~&  ~A~}" error)))
    (t (e) 
      (error "Failed to determine Maven version: ~A." e))))

(defun parse-mvn-version (version-string)
  (let* ((pattern (#"compile"
                   'regex.Pattern
                   "([0-9]+)\\.([0-9]+)\\.([0-9]+)"))
         (matcher (#"matcher" pattern version-string)))
    (if (#"find" matcher)
        (mapcar #'parse-integer 
                `(,(#"group" matcher 1) 
                   ,(#"group" matcher 2)
                   ,(#"group" matcher 3)))
        (error "Failed to parse a MAJOR.MINOR.PATCHLEVEL version from '~a'" version-string))))

  
(defun mvn-home ()
  "If the Maven executable can be invoked, introspect the value
  reported as Maven home."
  (handler-case 
      (multiple-value-bind (output error-output status)
          (uiop:run-program
           (format nil "~a --version" (truename (find-mvn)))
           :output :string
           :error-output :string)
        (unless (zerop status)
          (error "Failed to invoke Maven executable to introspect library locations: ~a." error-output))
        (let ((pattern (#"compile"
                        'regex.Pattern
                        "Maven home: (.+)$")))
          (with-input-from-string (s output)
            (do ((line (read-line s nil :eof) 
                       (read-line s nil :eof)))
                ((or (not line) (eq line :eof)) nil)
              (let ((matcher (#"matcher" pattern line)))
                (when (#"find" matcher)
                  (return-from mvn-home
                    (uiop/pathname:ensure-directory-pathname (#"group" matcher 1)))))))))
    (subprocess-error (e)
          (error "Failed to invoke Maven executable to introspect library locations: ~a." e))))

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

(define-condition no-aether-maven-libs (error)
  ((locations :initarg :locations
              :initform nil
              :reader locations))
  (:report (lambda (condition stream)
             (format stream "No Maven Aether libraries found locally in '~a'."
                     (locations condition)))))
             
(defparameter *init-p* nil
  "Whether we have successfully located the necessary Maven libraries")
  
(defun init (&optional &key (force nil))
  "Run the initialization strategy to bootstrap a Maven dependency node

Set *MVN-LIBS-DIRECTORY* to an explicit value before running this
function in order to bypass the dynamic introspection of the location
of the mvn executable with an explicit value."
  (when force
    (setf *session* nil
          *repository-system* nil))
  (unless (or force *mvn-libs-directory*)
    (setf *mvn-libs-directory* (find-mvn-libs)))
  (unless (and *mvn-libs-directory*
               (probe-file *mvn-libs-directory*))
    ;; FIXME Remove warning; put message in restart
    (warn "Please obtain and install maven-3.0.3 or later locally from <http://maven.apache.org/download.html>, then set ABCL-ASDF:*MVN-LIBS-DIRECTORY* to the directory containing maven-core-3.*.jar et. al.")
    (error (make-condition 'abcl-asdf::no-aether-maven-libs
                           :locations (list *mvn-libs-directory*))))
  (unless (ensure-mvn-version)
    (error "We need maven-3.0.3 or later."))
  (add-directory-jars-to-class-path *mvn-libs-directory* nil)
  (setf *init-p* t))

;;; The AETHER-DIRECTORY parameter is conceptually a little broken:
;;; because we can't "unload" jar files, we can't easily switch
;;; between Maven implementation at runtime.  Maybe this would be
;;; possible with some sort of classloader chaining, but such effort
;;; is not currently deemed as worthwhile.  Instead, to change Aether
;;; libraries, you'll have to restart ABCL.
(defmacro with-aether ((&optional aether-directory) &body body)
  "Ensure that the code in BODY is executed with the Maven Aether libraries on the classpath"
  (if aether-directory
      `(let ((*mvn-libs-directory* ,aether-directory))
         (init :force t)
         ,@body)
      `(progn (unless *init-p*
                (init))
              ,@body)))

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
  "Returns an implementation of the org.sonatype.aether.connector.wagon.WagonProvider contract

The implementation is specified as Lisp closures.  Currently, it only
specializes the lookup() method if passed an 'http' or an 'https' role
hint."
  (unless *init-p* (init))
  (java:jinterface-implementation 
   (#"getName" 
    (or
     (ignore-errors  ;; Maven 3.2.5+
       (jss:find-java-class 'aether.transport.wagon.WagonProvider))
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
          (format cl:*load-verbose*
                  "~&; abcl-asdf; WagonProvider stub passed '~A' as a hint it couldn't satisfy.~%"
                  role-hint)
          java:+null+))))
   "release"
   (lambda (wagon)
     (declare (ignore wagon)))))

(defun find-service-locator ()
  (or
   (ignore-errors
     ;; maven-3.0.4
     (jss:new "org.apache.maven.repository.internal.MavenServiceLocator")) 
   (ignore-errors
     ;; maven-3.1.0 using org.eclipse.aether...
     (jss:new "aether.impl.DefaultServiceLocator"))
   (ignore-errors
     (jss:new "org.apache.maven.repository.internal.DefaultServiceLocator"))
   (ignore-errors
     ;; maven-3.1.0
     (#"newServiceLocator" 'org.apache.maven.repository.internal.MavenRepositorySystemUtils))))


(defun make-repository-system ()
  (unless *init-p* (init))
  (let ((locator 
         (find-service-locator))
        (wagon-provider-class 
         (or
          (ignore-errors 
            (java:jclass "org.sonatype.aether.connector.wagon.WagonProvider"))
          (ignore-errors ;; Maven-3.3.x
            (jss:find-java-class 'connector.transport.TransporterFactory))
          (ignore-errors ;; Maven-3.2.5
            (jss:find-java-class 'org.eclipse.aether.transport.wagon.WagonProvider))
          (ignore-errors  ;; Maven-3.1.x 
            (jss:find-java-class 'aether.connector.wagon.WagonProvider))))
        (wagon-repository-connector-factory-class
         (or
          (ignore-errors 
            (jss:find-java-class 'org.sonatype.aether.connector.wagon.WagonRepositoryConnectorFactory))
          (ignore-errors 
            (jss:find-java-class 'org.eclipse.aether.connector.basic.BasicRepositoryConnectorFactory))
          (ignore-errors 
            (java:jclass "org.sonatype.aether.connector.wagon.WagonRepositoryConnectorFactory"))))
        (repository-connector-factory-class 
         (or
          (ignore-errors
            (jss:find-java-class 'aether.spi.connector.RepositoryConnectorFactory))
          (ignore-errors
            (jss:find-java-class 'org.eclipse.aether.spi.connector.RepositoryConnectorFactory))
          (ignore-errors 
            (java:jclass "org.sonatype.aether.spi.connector.RepositoryConnectorFactory"))))
        (repository-system-class
         (or
          (ignore-errors
            (java:jclass "org.sonatype.aether.RepositorySystem"))
          (ignore-errors 
            (jss:find-java-class 'org.eclipse.aether.RepositorySystem))
          (ignore-errors 
            (jss:find-java-class 'aether.RepositorySystem)))))
    (if (equal wagon-provider-class (ignore-errors (jss:find-java-class 'TransporterFactory)))
        ;;; Maven-3.3.3
        (let ((wagon-transporter-factory (jss:new 'WagonTransporterFactory)))
          (#"setWagonProvider" wagon-transporter-factory (make-wagon-provider))
          (#"setServices" locator
                          wagon-provider-class
                          (java:jarray-from-list (list wagon-transporter-factory))))
        (#"setServices" locator
                        wagon-provider-class
                        (java:jarray-from-list
                         (list (make-wagon-provider)))))
    (#"addService" locator
                   repository-connector-factory-class
                   wagon-repository-connector-factory-class)
    (values (#"getService" locator
                           repository-system-class)
            locator)))

(defun make-session (repository-system)
  "Construct a new aether.RepositorySystemSession from the specified REPOSITORY-SYSTEM."
  (with-aether ()
    (let ((session
           (or 
            (ignore-errors
              (java:jnew
               (jss:find-java-class "MavenRepositorySystemSession")))
            (ignore-errors
              (#"newSession"
               'org.apache.maven.repository.internal.MavenRepositorySystemUtils))))
          (local-repository
           (make-local-repository)))
      (#"setLocalRepositoryManager"
       session
       (make-local-repository-manager repository-system local-repository session)))))


(defun make-local-repository-manager (repository-system local-repository session)
  (or 
   (ignore-errors 
     (#"newLocalRepositoryManager" 
      repository-system local-repository))
   (ignore-errors      ;; maven-3.1.0
     (#"newLocalRepositoryManager" 
      repository-system session local-repository))))

(defun make-local-repository ()
  (java:jnew
   (or
    (ignore-errors
      (jss:find-java-class "org.sonatype.aether.repository.LocalRepository"))
    (ignore-errors
      (jss:find-java-class "org.eclipse.aether.repository.LocalRepository")))
   (namestring (merge-pathnames ".m2/repository/"
                                (user-homedir-pathname)))))

(defparameter *maven-http-proxy* nil
  "A string containing the URI of an http proxy for Maven to use.")

(defun make-proxy ()
  "Return an aether.repository.Proxy instance initialized from *MAVEN-HTTP-PROXY*."
  (unless *maven-http-proxy*
    (warn "No proxy specified in *MAVEN-HTTP-PROXY*")
    (return-from make-proxy nil))
  (let* ((p (pathname *maven-http-proxy*))
         (scheme (ext:url-pathname-scheme p))
         (authority (ext:url-pathname-authority p))
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
  "Return an instance of aether.artifact.DefaultArtifact initialized from ARTIFACT-STRING" 
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
  (unless *init-p* (init))
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
     (jss:new 'org.sonatype.aether.repository.RemoteRepository id type url))
   (ignore-errors 
     (#"build" (jss:new "org.eclipse.aether.repository.RemoteRepository$Builder" id type url)))))

(defvar *default-repository* 
  "https://repo1.maven.org/maven2/"
  "URI of default remote Maven repository")

(defun add-repository (repository)
  (ensure-remote-repository :repository repository))

(defparameter *maven-remote-repository*  nil
  "Reference to remote repository used by the Maven Aether
  embedder.")

(defun ensure-remote-repository (&key 
                                   (force nil)
                                   (repository *default-repository* repository-p))
  (unless *init-p* (init))
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
                               (repository *maven-remote-repository* repository-p)
                               (repositories NIL repositories-p))
  "Dynamically resolve Maven dependencies for item with GROUP-ID and ARTIFACT-ID 
optionally with a VERSION and a REPOSITORY.  

All recursive dependencies will be visited before resolution is successful.

If unspecified, the string \"LATEST\" will be used for the VERSION.

Returns a string containing the necessary jvm classpath entries packed
in Java CLASSPATH representation."
  (unless *init-p* (init))
  (unless versionp
    (warn "Using LATEST for unspecified version."))
  (let* ((coords 
          (format nil "~A:~A:~A" group-id artifact-id (if versionp version "LATEST")))
         (artifact 
          (make-artifact coords))
         (dependency
          (make-dependency artifact))
         (collect-request
          (or
           (ignore-errors
             (java:jnew (jss:find-java-class "org.sonatype.aether.collection.CollectRequest")))
           (ignore-errors
             (java:jnew (jss:find-java-class "org.eclipse.aether.collection.CollectRequest"))))))
    (#"setRoot" collect-request dependency)
    (setf repositories-p (or repository-p repositories-p))
    ;; Don't call addRepository if we explicitly specify a NIL repository
    (cond
      ((not repositories-p)
       (#"addRepository" collect-request (ensure-remote-repository)))
      (repository
       (if (stringp repository)
           (push repository repositories)
           (#"addRepository" collect-request repository))))
    (dolist (repository repositories)
      (#"addRepository" collect-request
                        (let ((r (make-remote-repository "central" "default" repository)))
                          (when *maven-http-proxy*
                            (#"setProxy" r (make-proxy)))
                          r)))
    (let* ((collect-result (#"collectDependencies" (ensure-repository-system)
                                                   (ensure-session) collect-request))
           (node 
            (#"getRoot" collect-result))
           (dependency-request
            (or
             (ignore-errors
            ;;; pre Maven-3.3.x
               (java:jnew (jss:find-java-class "DependencyRequest")
                          node java:+null+))
             (ignore-errors
               (jss:new 'DependencyRequest))))
           (nlg 
            (java:jnew (jss:find-java-class "PreorderNodeListGenerator"))))
      (#"setRoot" dependency-request node)
      (#"resolveDependencies" (ensure-repository-system) (ensure-session) dependency-request)
      (#"accept" node nlg)
      (#"getClassPath" nlg))))

(defun make-dependency (artifact)
  (or
   (ignore-errors
     (java:jnew (jss:find-java-class 'org.sonatype.aether.graph.Dependency)
                artifact
                (java:jfield
                 (jss:find-java-class "org.sonatype.aether.util.artifact.JavaScopes")
                 "COMPILE")))
   (ignore-errors
     (java:jnew (jss:find-java-class 'org.eclipse.aether.graph.Dependency)
                artifact
                (java:jfield
                 (jss:find-java-class "org.eclipse.aether.util.artifact.JavaScopes")
                 "COMPILE")))))


(defun make-repository-listener ()
  (flet ((log (e) 
           (format cl:*load-verbose* "~&; abcl-asdf; ~A~%" (#"toString" e))))
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
           (abcl-build:split-string string "/")
         (setf result 
               (apply #'resolve-dependencies group-id artifact-id
                      (append (when version
                                `(:version ,version))
                              (when repository
                                `(:repository ,repository))))))))))


;;; Currently the last file listed in ASDF
(provide 'abcl-asdf)


