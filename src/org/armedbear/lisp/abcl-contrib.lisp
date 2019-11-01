;;;; Mechanisms for finding loadable artifacts from the environment,
;;;; which are then used to locate the Common Lisp systems included as
;;;; `abcl-contrib`.
(require :asdf)

(in-package :system)

(defun boot-classloader ()
  (let ((boot-class (java:jclass "org.armedbear.lisp.Main"))
        (get-classloader (java:jmethod "java.lang.Class" "getClassLoader")))
    (java:jcall get-classloader boot-class)))

;;; java[678] packages the JVM system artifacts as jar files
;;; java11 uses the module system 
(defun system-artifacts-are-jars-p ()
  (java:jinstance-of-p (boot-classloader) "java.net.URLClassLoader"))

(defun system-jar-p (p)
  (or (named-jar-p "abcl" p)
      (named-jar-p "abcl-aio" p)))

(defun contrib-jar-p (p)
  (or 
   (named-jar-p "abcl-contrib" p)
   (named-jar-p "abcl-aio" p)))

(defun named-jar-p (name p)
  (and (pathnamep p)
       (equal (pathname-type p) "jar")
       (or
        (java:jstatic "matches"
                      "java.util.regex.Pattern"
                      (concatenate 'string name "(-[0-9]\\.[0-9]\\.[0-9]([+~-].+)?)?")
                      (pathname-name p))
        (java:jstatic "matches"
                      "java.util.regex.Pattern"
                      (concatenate 'string name "(-[0-9]\\.[0-9]\\.[0-9]\\.[0-9]([+~-]+)?)?")
                      (pathname-name p)))
       p))

(defun find-system ()
  "Find the location of the Armed Bear system implementation

Used to determine relative pathname to find 'abcl-contrib.jar'."
  (or
   (ignore-errors
     (find-system-jar))
   (ignore-errors
     (when (system-artifacts-are-jars-p)
       (some
        (lambda (u)
          (probe-file (make-pathname
                       :defaults (java:jcall "toString" u)
                       :name "abcl")))
        (java:jcall "getURLs" (boot-classloader)))))
   #+(or)
   ;; Need to test locating the system boot jar over the network, and
   ;; it would minimally need to check version information.
   (ignore-errors
     #p"http://abcl.org/releases/current/abcl.jar")))

(defun flatten (list)
  (labels ((rflatten (list accumluator)
	   (dolist (element list)
	     (if (listp element)
		 (setf accumluator (rflatten element accumluator))
		 (push element accumluator)))
	   accumluator))
    (let (result)
      (reverse (rflatten list result)))))

(defun java.class.path ()
  "Return a list of the directories as pathnames referenced in the JVM classpath."
  (let* ((separator (java:jstatic "getProperty" "java.lang.System" "path.separator"))
	 (paths (coerce (java:jcall "split"
			   (java:jstatic "getProperty" "java.lang.System"
					 "java.class.path")
			   separator)
                        'list))
         (p (coerce paths 'list)))
    (flet ((directory-of (p) (make-pathname :defaults p :name nil :type nil)))
      (values
       (mapcar #'directory-of p)
       p))))

(defun enumerate-resource-directories ()
  (flet ((directory-of (p)
           (make-pathname :defaults p
                          :name nil
                          :type nil)))
    (let ((result (java.class.path)))
      (dolist (entry (flatten (java:dump-classpath)))
        (cond
          ((java:jinstance-of-p entry "java.net.URLClassLoader") ;; java1.[678]
	   (dolist (url (coerce (java:jcall "getURLs" entry)
			        'list))
             (let ((p (directory-of (pathname (java:jcall "toString" url)))))
	       (when (probe-file p)
	         (pushnew p result :test 'equal)))))
        ((pathnamep entry)
         (pushnew (directory-of entry) result :test 'equal))
        ((and (stringp entry)
	      (probe-file (pathname (directory-of entry))))
         (pushnew (pathname (directory-of entry)) result :test 'equal))
        (t
         (format *standard-output*
                 "~&Skipping enumeration of resource '~a' with type '~a'.~%"
                 entry (type-of entry)))))
      result)))

(defun find-jar (predicate)
  (dolist (d (enumerate-resource-directories))
    (let ((entries (directory (make-pathname :defaults d
					     :name "*"
					     :type "jar"))))
      (let ((jar (some predicate entries)))
	(when jar
	  (return-from find-jar jar))))))

(defun find-system-jar ()
  "Return the pathname of the system jar, one of `abcl.jar` or
`abcl-m.n.p.jar` or `abcl-m.n.p[.~-]something.jar`."
  (find-jar #'system-jar-p))

(defun find-contrib-jar ()
  "Return the pathname of the contrib jar, one of `abcl-contrib.jar` or
`abcl-contrib-m.n.p.jar` or `abcl-contrib-m.n.p[.~-]something.jar`."
  (find-jar #'contrib-jar-p))

(defvar *abcl-contrib* nil
  "Pathname of the abcl-contrib artifact.

Initialized via SYSTEM:FIND-CONTRIB.")

;;; FIXME: stop using the obsolete ASDF:*CENTRAL-REGISTRY*
(defun add-contrib (abcl-contrib-jar
                    &key (verbose cl:*load-verbose*))
  "Introspects the ABCL-CONTRIB-JAR path for sub-directories which
  contain asdf definitions, adding those found to asdf."
  (let ((jar-path (if (ext:pathname-jar-p abcl-contrib-jar)
                      abcl-contrib-jar
                      (make-pathname :device (list abcl-contrib-jar)))))
    (dolist (asdf-file
             (directory (merge-pathnames "*/*.asd" jar-path)))
      (let ((asdf-directory (make-pathname :defaults asdf-file :name nil :type nil)))
        (unless (find asdf-directory asdf:*central-registry* :test #'equal)
          (push asdf-directory asdf:*central-registry*)
          (format verbose "~&; Added ~A to ASDF.~%" asdf-directory))))))

(defun find-and-add-contrib (&key (verbose cl:*load-verbose*))
  "Attempt to find the ABCL contrib jar and add its contents to ASDF.
returns the pathname of the contrib if it can be found."
   (if *abcl-contrib*
       (format verbose "~&; Finding contribs utilizing previously initialized value of SYS:*ABCL-CONTRIB* '~A'.~%"
               *abcl-contrib*)
       (progn
         (let ((contrib (find-contrib)))
           (when contrib
             (format verbose "~&; Using probed value of SYS:*ABCL-CONTRIB* '~A'.~%"
                     contrib)
             (setf *abcl-contrib* contrib)))))
   (when *abcl-contrib*  ;; For bootstrap compile there will be no contrib
     (add-contrib *abcl-contrib*)))

(defun find-name-for-implementation-title (file id)
  "For a jar FILE containing a manifest, return the name of the
  section which annotates 'Implementation-Title' whose string value is
  ID."
  (declare (type pathname file))
  (let* ((jar (java:jnew "java.util.jar.JarFile" (namestring file)))
         (manifest (java:jcall "getManifest" jar))
         (entries (java:jcall "toArray"
                              (java:jcall "entrySet"
                                          (java:jcall "getEntries" manifest)))))
    (dolist (entry 
              (loop :for entry :across entries
                 :collecting entry))
      (let ((title (java:jcall "getValue"
                               (java:jcall "getValue" entry)
                               "Implementation-Title")))
        (when (string-equal title id)
          (return-from find-name-for-implementation-title
            (java:jcall "getKey" entry))))
    nil)))

(defun find-contrib ()
  "Introspect runtime classpaths to return a pathname containing
  subdirectories containing ASDF definitions."

  (or
   ;; We identify the location of the directory within a jar file
   ;; containing abcl-contrib ASDF definitions by looking for a section
   ;; which contains the Implementation-Title "org.abcl-contrib".  The
   ;; name of that section then identifies the relative pathname to the
   ;; top-most directory in the Jar
   ;;
   ;; e.g. for an entry of the form
   ;;
   ;;     Name: contrib
   ;;     Implementation-Title: org.abcl-contrib
   ;;
   ;; the directory 'contrib' would be searched for ASDF definitions.
   (ignore-errors
        (let* ((system-jar
                (find-system-jar))
               (relative-pathname 
                (find-name-for-implementation-title system-jar "org.abcl-contrib")))
          (when (and system-jar relative-pathname)
            (merge-pathnames (pathname (concatenate 'string
                                                   relative-pathname "/"))
                            (make-pathname
                             :device (list system-jar))))))
   (ignore-errors
     (find-contrib-jar))
   (ignore-errors
     (let ((system-jar (find-system-jar)))
       (when system-jar
         (probe-file (make-pathname
                      :defaults system-jar
                      :name (concatenate 'string
                                         "abcl-contrib"
                                         (subseq (pathname-name system-jar) 4)))))))
   (when (java:jinstance-of-p (boot-classloader) "java.net.URLClassLoader")
     (some
      (lambda (u)
        (probe-file (make-pathname
                     :defaults (java:jcall "toString" u)
                     :name "abcl-contrib")))
      (java:jcall "getURLs" (boot-classloader))))))

(export '(find-system
          find-contrib
          system-artifacts-are-jars-p
          java.class.path
          *abcl-contrib*)
        :system)

(when (find-and-add-contrib :verbose cl:*load-verbose*)
  (provide :abcl-contrib))
