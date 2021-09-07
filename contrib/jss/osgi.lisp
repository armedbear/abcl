(in-package :jss)

;; This is the start of an extension of JSS to be able to use OSGI
;; Bundles.  Currently one can, at least, take advantage of the class
;; hiding aspect - with the visible packages listed in a JAR manifest
;; "Exported-Packages", those classes can be accessed while the others
;; can't.

;; General use:
;; (add-bundle path-to-jar-file)
;; (find-java-class <some class exported from your bundle>)
;; Do stuff

;; The current implementation assumes you aren't aiming to have
;; multiple version of the same class exported from different bundles
;; in that find-java-class will try to complain when a class name is
;; ambiguous, but once a class is found it will continue to be found,
;; even if another version of the class becomes available in another
;; bundle. For finer control use:
;; (find-bundle-class bundle classname)
;; Class name can be abbreviated as with find-java-class

;; bundle arguments can either be a string result of
;; (#"getSymbolicName" bundle) or a bundle object loaded bundles are
;; in an association list in *loaded-osgi-bundles*, with each element
;; being (name object class-lookup-hash)

;; My primary use is to use a project with dependencies that conflict
;; with the jars I'm using.  Here's an example of how I package that
;; using maven. It is from module-bundle/pom.xml from the project
;; https://github.com/alanruttenberg/pagoda (that project uses maven
;; modules in order to also be able to run the usual packaging)
;;
;; <plugin>
;; boilerplate. The maven-bundle-plugin takes over the package phase from the default assembly plugin
;; 	<groupId>org.apache.felix</groupId>
;; 	<artifactId>maven-bundle-plugin</artifactId>
;; 	<extensions>true</extensions>
;; 	<configuration>
;; 	  <descriptorRefs>
;; This will be the prefix for the jar that is created. It will be prefix-<version>.jar
;; 	    <descriptorRef>pagoda-bundle</descriptorRef>
;; 	  </descriptorRefs>
;; 	  <instructions>
;; These are the packages that I want to be visible
;; 	    <Export-Package>uk.ac.ox.cs.pagoda.*,uk.ac.ox.cs.JRDFox.*</Export-Package>
;; This says to put absolutely every class/jar that they depend on in the created bundle
;; 	    <Embed-Dependency>*;scope=compile</Embed-Dependency>   
;; 	    <Embed-Transitive>true</Embed-Transitive>
;; This avoids having the bundle plugin write dependencies that imply
;; the jars of the dependency are also bundles. If they aren't then
;; you get link errors when trying to install the bundle. (sheesh!)
;; 	    <Import-Package/>
;; 	  </instructions>
;; 	</configuration>
;; </plugin>


(defvar *osgi-framework* nil)

;; Beware the cache. Bundles are installed in a cache folder and
;; reinstalling them without clearing that will cause a conflict - an
;; error about duplicates. (Among other things, the installation
;; unpacks the jars in the bundle and arranges the classpath to use
;; them).  While I believe that the cache will be refreshed if the
;; version number on the bundle is changed, it's annoying to do that
;; during development. Instead, if the bundle is already installed
;; (available via #"getBundles") then we compare the modification
;; dates of the installed verison and file-write-date of the jar, and
;; if the jar is newer uninstall the old bundle and install the new
;; one.

;; The cache location is taken from *osgi-cache-location*. The current
;; location can be had by (osgi-cache-path) You can ensure a clean
;; cache by calling (ensure-osgi-framework :clean-cache t) before
;; calling add-bundle, or set *osgi-clean-cache-on-start* to t

;; arg name is used to identify the bundle among the loaded bundles. If
;; not supplied then the "symbolic name" is used, the value of the
;; manifest header "Bundle-SymbolicName".

(defvar *osgi-cache-location* (namestring (merge-pathnames 
					   (make-pathname :directory '(:relative "abcl-felix-cache"))
					   (user-homedir-pathname)))
  "Where bundle jars are copied to and unpacked if necessary. Default is to have it distinct from the default, since who knows what's been put there")

;;http://felix.apache.org/documentation/subprojects/apache-felix-framework/apache-felix-framework-configuration-properties.html

(defvar *osgi-configuration* `(;; Where the cache should live
			       ("org.osgi.framework.storage" ,*osgi-cache-location*)
			       ;; So that imported system classes are loaded using ABCL's classloader
			       ("org.osgi.framework.bundle.parent" "framework")
			       ;; sounds good even though I'm not understanding bootdelegation yet
			       ("felix.bootdelegation.implicit" "true")
			       ;; just in case
			       ("org.osgi.framework.library.extensions" "jnilib,dylib")
			       ))

(defvar *osgi-clean-cache-on-start* t "Clear the cache on startup. First add-bundle has this set as t and then flips to nil (so you don't lose the rest of your packages)")

(defvar *osgi-native-libraries* nil "Alist of bundles -> native libraries they're to load. Informative - not prescriptive")

(defvar *before-osgi-starting-hooks* nil "A list of functions to call before before OSGI starts, for example to modify *osgi-configuration*")

;; Why is the native library not being loaded with system.load()?

;; Because system.load looks up the stack for a caller and that
;; caller's classloader is used to findLibrary, and the classloader
;; is the wrong one.
;; Proof: The call to loadlibary on the *right* classloader works.
;; The class in question is FactPlusPlus which calls System.load() in its init.
;; FaCTPlusPlusReasonerFactory is loaded by the same (osgi) classloader so we get *that* classloader
;; and then call the loadLibrary method on it. It takes another class (which classloader is used for findLibrary
;; (jstatic (find "loadLibrary"  (#"getDeclaredMethods"  (find-java-class 'lang.classloader)) :key #"getName" :test 'equal)  c (find-java-class 'FaCTPlusPlusReasonerFactory) "FaCTPlusPlusJNI" +false+)

;; However in the context, when system.load is called, it doesn't have
;; a class to look the classloader up with, so it looks down the stack
;; and grabs a class and uses its classloaer. Apparently that's *not*
;; the osgi classloader, presumably because it's called from the
;; framework, and the framework's classloader is not the same as the
;; classloader that the framework uses to load bundle classes.

;; here's a guess. If calling with felix.main, the framework gets an
;; osgi classloader and subsequently all is happy.
;;
;; Could be fixed in code by not calling system.load but rather
;; speaking to the classloader directly as above.
;; A theory: If a class in another bundle loaded factpp then it would work.
;; THIS DOESN'T HAPPEN IF felix.main is used rather than felix.framework!!!

;; configuration properties are set last to first, so you can prepend overrides to *osgi-configuration*

(eval-when (:load-toplevel :execute)
  (loop for function in *before-osgi-starting-hooks* do (funcall function)))

(defun ensure-osgi-initialized (&key (empty-cache *osgi-clean-cache-on-start*))
  (unless *osgi-framework*
    (loop for function in *before-osgi-starting-hooks* do (funcall function))
    (let ((map (new 'java.util.properties))
	  (configuration *osgi-configuration*))
      (loop for (prop val) in (reverse configuration) do (#"setProperty" map prop val))
      (when empty-cache
	(#"setProperty" map "org.osgi.framework.storage.clean" "onFirstInit"))
      (flet ((resolve (artifact)
	       (funcall (intern "RESOLVE" 'abcl-asdf) artifact)))
	(add-to-classpath ;; sometimes resolve returns ":" separated pathnames of both main and framework jars. Only need the first.
	 ;; 5.6.1 current as of Jan/17
	 (car (split-at-char (resolve "org.apache.felix/org.apache.felix.main/5.6.1") ":"))))
      (let* ((framework-factory-class (find-java-class 'org.osgi.framework.launch.FrameworkFactory))
	     (ffs (#"load" 'ServiceLoader framework-factory-class (#"getClassLoader" framework-factory-class)))
	     (factory (#"next" (#"iterator" ffs)))
	     (framework (#"newFramework" factory map)))
	(#"start" framework)
	(setq *osgi-framework* framework)))))

(defun force-unpack-native-libraries (bundle jar)
  "Not used unless OSGI misbehaves again"
  (let ((wiring (#"adapt" bundle (find-java-class 'BundleWiring))))
    (loop for native in (jss::j2list (#"getNativeLibraries" wiring))
	  for entry = (#"getEntryName" native)
	  for library-path = (#"getEntryAsNativeLibrary" (#"getContent" (#"getRevision" wiring)) entry)
	  do (pushnew (list jar library-path) *osgi-native-libraries* :test 'equalp)
	     ;;(#"load" 'system library-path)
	  )))

(defun stop-osgi ()
  (#"stop" *osgi-framework*)
  (setq *osgi-framework* nil)
  ;; should *loaded-osgi-bundles* be set to nil here?
  )

(defun reset-osgi ()
  "Restart OSGI after emptying the cache, and reload bundles that were loaded"
  (stop-osgi)
  (ensure-osgi-initialized :empty-cache t)
  (loop for (name nil nil jar) in (copy-list *loaded-osgi-bundles*)
	do (add-bundle jar :name name)))

(defun get-osgi-framework-property (property)
  (ensure-osgi-initialized)
  (#"getProperty" (#"getBundleContext" *osgi-framework*) property))

(defun add-to-comma-separated-osgi-config (config-parameter elements)
  (let* ((entry (find config-parameter *osgi-configuration* :test 'equal :key 'car))
	 (value (if entry (second entry) ""))
	 (new-value (format nil "~{~a~^,~}" 
			    (sort (union elements (if (equal value "") nil (jss::split-at-char value #\,))
					 :test 'equalp)
				  'string-lessp))))
    (if entry
	(setf (second entry) new-value)
	(push (list config-parameter new-value) *osgi-configuration*))))

(defun osgi-cache-path ()
  (ensure-osgi-initialized)
  (get-osgi-framework-property "org.osgi.framework.storage"))
			       
;; this: http://lisptips.com/post/11649360174/the-common-lisp-and-unix-epochs is wrong!
;; Compute the offset using (#"currentTimeMillis" 'system)
(defun universal-to-bundle-time (universal-time)
  "Convert from lisp time to unix time in milliseconds, used by osgi"
  (let ((offset (- (get-universal-time) (floor (#"currentTimeMillis" 'system) 1000))))
    (* 1000 (- universal-time offset))))

(defun add-bundle (jar &key name)
  (ensure-osgi-initialized)
  (setq *osgi-clean-cache-on-start* nil)
  (setq jar (namestring (translate-logical-pathname jar)))
  (let* ((bundle-context (#"getBundleContext" *osgi-framework*))
	 (bundle (find jar (#"getBundles" bundle-context) :key #"getLocation" :test 'search)))
    (when (or (not bundle)
	      (< (#"getLastModified" bundle) 
		 (universal-to-bundle-time (file-write-date jar))))
      (when bundle
	(warn "reinstalling bundle ~a" jar)
	(#"uninstall" bundle))
      (unless (member :scheme (pathname-host jar))
	(setq jar (concatenate 'string "file:" jar)))
      (setq bundle (#"installBundle" bundle-context jar)))

    (#"start" bundle)
    (let ((name (or name (#"getSymbolicName" bundle))))
      (let* ((index (index-class-names (bundle-exports bundle))))
	(setq *loaded-osgi-bundles* (remove name *loaded-osgi-bundles* :test 'equalp :key 'car))
	(push (list name bundle index jar) *loaded-osgi-bundles*)
;	(force-unpack-native-libraries bundle jar)
	bundle))))

(defun index-class-names (names &key (table (make-hash-table :test 'equalp)))
  (with-constant-signature ((matcher "matcher" t) (substring "substring")
			    (jreplace "replace" t) (jlength "length")
			    (matches "matches") 
			    (group "group"))
    (loop for name in names
	  with class-pattern = (jstatic "compile" "java.util.regex.Pattern" ".*\\.class{0,1}$")
	  with name-pattern = (jstatic "compile" "java.util.regex.Pattern" ".*?([^.]*)$")
	  when (matches (matcher class-pattern name))
	    do
	       (let* ((fullname (substring (jreplace name #\/ #\.) 0 (- (jlength name) 6)))
		      (matcher (matcher name-pattern fullname))
		      (name (progn (matches matcher) (group matcher 1))))
		 (pushnew fullname (gethash name table) 
			  :test 'equal))))
  table)



(defun bundle-headers (bundle)
  (loop with headers = (#"getHeaders" bundle)
	for key in (j2list (#"keys" headers))
	collect (list key (#"get" headers key) (#"get" headers key))))

(defun bundle-header (bundle key)
  (#"get"  (#"getHeaders" bundle) key))

;; Not useful yet
(defun bundle-capabilities (bundle)
  (let ((bundleWiring (#"adapt" bundle (find-java-class 'BundleWiring))))
    (loop with i = (#"iterator" (#"getCapabilities" bundlewiring +null+)) 
	  while (#"hasNext" i)
	  for cap = (#"next" i) 
	  for namespace = (#"getNamespace" cap)
	  for es = (#"entrySet" (#"getAttributes" cap)) 
	  collect (list* namespace cap (mapcar #"getValue" (set-to-list es))))))

;; This is ugly but will do until there's a better way The exported
;; packages are listed as the value of the header "Export-Package" The
;; format is a concatenation of entries like the below

;; package;key="...","..";key2="",
;; package2,
;; package3;..,

;; i.e. for each package there are some optional key values pairs
;; which we're not going to attend to now,

;; Step 1: Since there are "," inside the string we take this apart by
;; first emptying the strings, then splitting by ",", then tossing
;; anything past a ";"

;; Step 2: There may or may not be subpackages. Since we're going to
;; match on the prefix, we throw away everything but the prefix. This
;; is done by first sorting, then taking an element and comparing it
;; to subsequent ones. When the first start the other we toss the other.
;; Not really necessary - not doing it would just be wasted work.

;; Step 3: The bundlewiring interface lets one iterate over all
;; 'resources', which are like entries in a jar, some of which are
;; class files. We only want the exported class files, so we only keep
;; those that start with our prefix (.->/ to make it a path)

;; Step 4: Extract the class name from the path (keep ".class" at the end)

;; Learned about bundle wiring at
;; http://stackoverflow.com/questions/22688997/how-i-can-get-list-of-all-classes-from-given-bundle

;; Spun my wheels a while looking for a cleaner way to do this, but
;; its confusing. This should do for now.

(defun bundle-exports (bundle)
  (let ((entry (bundle-header bundle "Export-Package"))
	(bundleWiring (#"adapt" bundle (find-java-class 'BundleWiring))))
    ;; if there's an "Export-package" then respect it
    (if entry
	(loop for package-prefix
		in 
		(loop with candidates = (sort (mapcar (lambda(el) (#"replaceAll" el ";.*$" "")) 
						      (split-at-char (#"replaceAll" entry "(\\\".*?\\\")" "") #\,))			 
					      'string-lessp)
		      for first = (pop candidates)
		      until (null candidates)
		      do (loop for next = (car candidates)
			       while (and next (eql 0 (search first next))) do (pop candidates))
		      collect first)
	      for path = (substitute #\/ #\. package-prefix) 
	      append
	      (loop for entry in (set-to-list (#"listResources" bundlewiring (concatenate 'string "/" path)
								"*.*" (jfield (find-java-class 'BundleWiring) "FINDENTRIES_RECURSE")))
		    for url = (#"toString" (#"getEntry" bundle entry))
		    collect
		    (substitute #\. #\/ (subseq 
					 (#"toString" (#"getEntry" bundle entry))
					 (search path url :test 'char=)))))
	;; otherwise it's all good
	(loop for entry in (set-to-list (#"listResources"
					 bundlewiring "/"
					 "*.*" (jfield (find-java-class 'BundleWiring) "FINDENTRIES_RECURSE")))
	      for url = (#"toString" (#"getEntry" bundle entry))
	      when (#"matches" url ".*\\.class$")
		collect
		(substitute #\. #\/ (subseq (subseq url 9) (1+ (search "/" (subseq url 9)))))))))

(defun dwim-find-bundle-entry (name)
  (let ((string (string name)))
    (let ((candidates (remove-if-not (lambda(e) (search string (car e) :test 'string-equal))  jss::*loaded-osgi-bundles*)))
      (cond ((= (length candidates) 0) (error "Bundle ~a not found" name))
	    ((= (length candidates) 1) (car candidates))
	    (t (error "Ambiguous \"~a\" could mean ~{~a~^, ~}" (mapcar 'car candidates)))))))
    
;; Like find java class, but looks in a bundle. no-cache means don't
;; look for it like find-java-class and don't cache it for
;; find-java-class. Default currently is to do so, but I might change
;; the default, as it could lead to confusion in the case where both
;; find-java-class and find-bundle-class are used and there are two
;; versions of the same class in the environment.

(defun find-bundle-class (bundle classname &key no-cache &aux bundle-entry)
  (cond ((or (stringp bundle) (symbolp bundle) )
	 (setq bundle-entry (dwim-find-bundle-entry bundle))
	 (setq bundle (second bundle-entry)))
	((java-object-p bundle)
	 (setq bundle-entry (find bundle *loaded-osgi-bundles* :key 'second))))
  (assert bundle () "No bundle named ~a" bundle)
  ;; we'll allow one bundle to be in the cache. Check if we're the one.
  (or (let ((found (and (not no-cache) (gethash (string classname) *imports-resolved-classes*))))
	(and (consp found) (eq (car found) bundle) (second found)))
      (let ((found (lookup-class-name classname :table (third bundle-entry))))
	(if found
	    (progn 
	      (unless no-cache
		(unless (gethash classname *imports-resolved-classes*)
		  (setf (gethash classname *imports-resolved-classes*) (cons bundle found))))
	      (#"loadClass" bundle found))
	    (#"loadClass" bundle (string classname))))))
  

