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
  "Find the location of the system.

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
  "Pathname of the ABCL contrib.
Initialized via SYSTEM:FIND-CONTRIB.")

(defparameter *verbose* t)

(defun add-contrib (abcl-contrib-jar &optional relative)
  "Introspects ABCL-CONTRIB-JAR for asdf systems to add to ASDF:*CENTRAL-REGISTRY*"
  (when abcl-contrib-jar
    (dolist (asdf-file
             (directory (make-pathname :device (list abcl-contrib-jar)
                                       :directory (if relative `(:absolute ,relative :wild) '(:absolute :wild))
                                       :name :wild
                                       :type "asd")))
      (let ((asdf-directory (make-pathname :defaults asdf-file :name nil :type nil)))
        (unless (find asdf-directory asdf:*central-registry* :test #'equal)
          (push asdf-directory asdf:*central-registry*)
          (format *verbose* "~&Added ~A to ASDF.~&" asdf-directory))))))

(defun find-and-add-contrib (&key (verbose nil))
  "Attempt to find the ABCL contrib jar and add its contents to ASDF.
Returns the pathname of the contrib if it can be found."
  (if *abcl-contrib*
      (format verbose "~&Using already initialized value of abcl-contrib:~&'~A'.~%"
              *abcl-contrib*)
      (progn
	(setf *abcl-contrib* (find-contrib))
	(format verbose "~&Using probed value of abcl-contrib:~&'~A'.~%"
		*abcl-contrib*)))
  (add-contrib *abcl-contrib*
	       (and (equalp *abcl-contrib* (find-system-jar))
		    "contrib"))
  )

(defun find-contrib ()
  "Introspect runtime classpaths to find a loadable ABCL-CONTRIB."
  (or (ignore-errors
       (let ((system-jar (find-system-jar)))
	 (and
	  (probe-file (make-pathname
			:device (list system-jar)
			:directory '(:absolute "contrib")
			:name "README" :type "markdown" ))
	  system-jar)))
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

(export `(find-system
          find-contrib
          *abcl-contrib*))

(when (find-and-add-contrib :verbose t)
  (provide :abcl-contrib))
