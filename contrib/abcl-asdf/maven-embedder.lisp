;;; Use the Aether system in a default maven distribution to download
;;; and install dependencies.
;;;
;;; https://docs.sonatype.org/display/AETHER/Home
;;;

(in-package :abcl-asdf)

(require :abcl-contrib)
(require :jss)

(defparameter *mvn-directory*
  "/export/home/evenson/work/apache-maven-3.0.3/lib/"
  "Location of 'maven-core-3.<m>.<p>.jar', 'maven-embedder-3.<m>.<p>.jar' etc.")

(defun init () 
  (unless (probe-file *mvn-directory*)
    (error "You must download Maven 3 from http://maven.apache.org/download.html, then set ABCL-ASDF:*MVN-DIRECTORY* appropiately."))
  (jss:add-directory-jars-to-class-path *mvn-directory* nil))

(defun repository-system ()
  (let ((locator 
         (java:jnew "org.apache.maven.repository.internal.DefaultServiceLocator"))
        (wagon-class 
         (java:jclass "org.sonatype.aether.connector.wagon.WagonProvider"))
        (wagon-provider 
         (jss:find-java-class "LightweightHttpWagon"))
        (repository-connector-factory-class
         (java:jclass "org.sonatype.aether.connector.wagon.WagonRepositoryConnector"))
        (wagon-repository-connector-factory-class
         (java:jclass "org.sonatype.aether.connector.wagon.WagonRepositoryConnectorFactory"))
        (repository-system-class
         (java:jclass "org.sonatype.aether.RepositorySystem")))
    (#"setService" locator wagon-class wagon-provider)
    (#"addService" locator 
                   repository-connector-factory-class
                   wagon-repository-connector-factory-class)
    (#"getService" locator repository-system-class)))

#|
private static RepositorySystem newRepositorySystem()
{
  DefaultServiceLocator locator = new DefaultServiceLocator();
  locator.setServices( WagonProvider.class, new ManualWagonProvider() );
  locator.addService( RepositoryConnectorFactory.class, WagonRepositoryConnectorFactory.class );

  return locator.getService( RepositorySystem.class );
}
|#

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

#|
private static RepositorySystemSession newSession( RepositorySystem system )
{
  MavenRepositorySystemSession session = new MavenRepositorySystemSession();

  LocalRepository localRepo = new LocalRepository( "target/local-repo" );
  session.setLocalRepositoryManager( system.newLocalRepositoryManager( localRepo ) );
  
  return session;
}
|#

;;; XXX make-immediate-object is deprecated
(defconstant +null+ (java:make-immediate-object nil :ref))

(defun resolve (group-id artifact-id version)
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
                     "central" "default" 
                     "http://repo1.maven.org/maven2/"))
         (collect-request (java:jnew (jss:find-java-class "CollectRequest"))))
    (#"setRoot" collect-request dependency)
    (#"addRepository" collect-request central)
    (let* ((node 
            (#"getRoot" (#"collectDependencies" system session collect-request)))
           (dependency-request 
            (java:jnew (jss:find-java-class "DependencyRequest")
                       node +null+))
           (nlg 
            (java:jnew (jss:find-java-class "PreorderNodeListGenerator"))))
      (#"resolveDependencies" system session dependency-request)
      (#"accept" node nlg)
      (#"getClassPath" nlg))))

#|
public static void main( String[] args )
  throws Exception
{
  RepositorySystem repoSystem = newRepositorySystem();

  RepositorySystemSession session = newSession( repoSystem );

  Dependency dependency =
    new Dependency( new DefaultArtifact( "org.apache.maven:maven-profile:2.2.1" ), "compile" );
  RemoteRepository central = new RemoteRepository( "central", "default", "http://repo1.maven.org/maven2/" );

  CollectRequest collectRequest = new CollectRequest();
  collectRequest.setRoot( dependency );
  collectRequest.addRepository( central );
  DependencyNode node = repoSystem.collectDependencies( session, collectRequest ).getRoot();

  DependencyRequest dependencyRequest = new DependencyRequest( node, null );

  repoSystem.resolveDependencies( session, dependencyRequest  );

  PreorderNodeListGenerator nlg = new PreorderNodeListGenerator();
  node.accept( nlg );
  System.out.println( nlg.getClassPath() );
}
|#

#| 

Test:

(init)
(resolve "org.slf4j" "slf4j-api" "1.6.1")
|#