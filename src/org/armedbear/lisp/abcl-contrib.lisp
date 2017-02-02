(in-package :system)

(require :asdf)

(defconstant +get-classloader+
  (java:jmethod "java.lang.Class" "getClassLoader"))

(defun boot-classloader ()
  (let ((boot-class (java:jclass "org.armedbear.lisp.Main")))
    (java:jcall +get-classloader+ boot-class)))

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
     (some
      (lambda (u)
        (probe-file (make-pathname
                     :defaults (java:jcall "toString" u)
                     :name "abcl")))
      (java:jcall "getURLs" (boot-classloader))))
   (ignore-errors
     #p"http://abcl.org/releases/current/abcl.jar")))

(defun find-jar (predicate)
  (dolist (loader (java:dump-classpath))
    (let ((jar (some predicate loader)))
      (when jar
        (return jar)))))

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
          (format verbose "~&; abcl-contrib; Added ~A to ASDF.~&" asdf-directory))))))

(defun find-and-add-contrib (&key (verbose cl:*load-verbose*))
  "Attempt to find the ABCL contrib jar and add its contents to ASDF.
returns the pathname of the contrib if it can be found."
   (if *abcl-contrib*
       (format verbose "~&; abcl-contrib; Using already initialized value of SYS:*ABCL-CONTRIB* '~A'.~%"
               *abcl-contrib*)
       (progn
         (let ((contrib (find-contrib)))
           (when contrib
             (format verbose "~&; abcl-contrib; Using probed value of SYS:*ABCL-CONTRIB* '~A'.~%"
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
   (some
    (lambda (u)
      (probe-file (make-pathname
                   :defaults (java:jcall "toString" u)
                   :name "abcl-contrib")))
    (java:jcall "getURLs" (boot-classloader)))))

(export '(find-system
          find-contrib
          *abcl-contrib*)
        :system)

(when (find-and-add-contrib :verbose cl:*load-verbose*)
  (provide :abcl-contrib))
