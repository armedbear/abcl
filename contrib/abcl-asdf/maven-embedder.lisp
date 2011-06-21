(in-package :abcl-asdf)

(require :abcl-contrib)
(require :jss)

(defvar *mvn-directory*
  "/export/home/evenson/work/apache-maven-3.0.3/lib/"
  "Location of 'maven-core-3.<m>.<p>.jar', 'maven-embedder-3.<m>.<p>.jar' etc.")

(defun init () 
  (jss:add-directory-jars-to-class-path *mvn-directory* nil))

(defconstant +null+
  (java:make-immediate-object :ref nil))

(defun resolve (group-id artifact-id version)
  (let* ((configuration (find-configuration))
         (embedder (jss:new 'MavenEmbedder configuration))
         (artifact (#"create" embedder group-id version +null+ "jar")))
    (pathname (#"toString" (#"getFile" artifact)))))

(defun find-configuration ())




#|
// http://developers-blog.org/blog/default/2009/09/18/How-to-resolve-an-artifact-with-maven-embedder

import java.util.ArrayList; 

import java.util.List; 
import org.apache.log4j.Logger; 
import org.apache.maven.artifact.Artifact; 
import org.apache.maven.artifact.repository.ArtifactRepository; 
import org.apache.maven.artifact.repository.DefaultArtifactRepository; 
import org.apache.maven.artifact.repository.layout.DefaultRepositoryLayout; 
import org.apache.maven.artifact.resolver.ArtifactNotFoundException; 
import org.apache.maven.artifact.resolver.ArtifactResolutionException; 
import org.apache.maven.embedder.Configuration; 
import org.apache.maven.embedder.ConfigurationValidationResult; 
import org.apache.maven.embedder.DefaultConfiguration; 
import org.apache.maven.embedder.MavenEmbedder; 
import org.apache.maven.embedder.MavenEmbedderException; 
import org.apache.maven.model.Profile; 
import org.apache.maven.model.Repository; 
import org.apache.maven.settings.SettingsUtils; 


/** 
 * resolve artifact. 
 * @param groupId group id of artifact 
 * @param artifactId artifact id of artifact 
 * @param version version of artifact 
 * @return downloaded artifact file 
 * @throws HostingOrderException error occured during resolution 
 */ 

public File resolveArtifact(String groupId, String artifactId, String version) 
  throws Exception 
{ 
  LOG.debug("request to resolve '" + groupId + ":" 
            + artifactId + ":" + version + "'"); 
  Artifact artifact = null; 
  LOG.debug("using settings: " + this.settingsFile); 
  File settings = new File(this.getClass().getClassLoader() 
                           .getResource(this.settingsFile).getFile()); 
  Configuration configuration = new DefaultConfiguration() 
    .setGlobalSettingsFile(SETTINGS) 
    .setClassLoader(this.classLoader); 
  ConfigurationValidationResult validationResult = 
    MavenEmbedder.validateConfiguration(configuration); 
  if (validationResult.isValid()) { 
    try { 
      MavenEmbedder embedder = new MavenEmbedder(configuration); 
      artifact = embedder.createArtifact(groupId, 
                                         artifactId, version, null, "jar"); 
      // assign repos, 
      List repos = new ArrayList(); 
      Profile profile = SettingsUtils.convertFromSettingsProfile((org.apache.maven.settings.Profile) 
                                                                 embedder.getSettings().getProfiles().get(0)); 
      for (Repository r : (List < Repository > ) profile. 
             getRepositories()) { 
        ArtifactRepository repo = new DefaultArtifactRepository(r.getId(), 
                                                                r.getUrl(), 
                                                                new DefaultRepositoryLayout()); 
        repos.add(repo); 
        LOG.debug("added repo " + r.getId() + ":" 
                  + r.getUrl()); 
      } 
      embedder.resolve(artifact, repos, 
                       embedder.getLocalRepository()); 
    } catch (MavenEmbedderException mee) {
    } catch (ArtifactResolutionException are) { 
    } catch (ArtifactNotFoundException ane) { 
    } finally {
      configuration = null; 
      validationResult = null; 
    } 
    LOG.info(artifact.getFile().getPath()); 
    return artifact.getFile(); 
  } else { 
    LOG.error("settings file did not validate !!"); 
    if (!validationResult.isUserSettingsFilePresent()) { 
      LOG.warn("The specific user settings file "'  + settings + "' is not present.);
    } else if (!validationResult.isUserSettingsFileParses()) { 
      LOG.warn("Please check your settings file, it is not well formed XML.");
    } 
  } 
  return null; 
} 
|#