;;;; The ABCL specific overrides in ASDF.  
;;;;
;;;; Done separately from asdf.lisp for stability.
(require :asdf)
(in-package :asdf)

(defclass iri (component) 
  ((schema :initform nil)
   (authority :initform nil)
   (path :initform nil)
   (query :initform nil)
   (fragment :initform nil)))

(defclass mvn (iri) 
  ((group-id :initarg :group-id :initform nil)
   (artifact-id :initarg :artifact-id :initform nil)
   (repository :initarg :repository :initform "http://repo1.maven.org/maven2/") ;;; XXX unimplemented
   (resolved-classpath :initform nil :accessor resolved-classpath)
   (classname :initarg :classname :initform nil)
   (alternate-uri :initarg :alternate-uri :initform nil)
   ;; inherited from ASDF:COMPONENT ??? what are the CL semantics on overriding -- ME 2012-04-01
   #+nil   (version :initform nil)))

#+nil
(defmethod find-component ((component iri) path)
  component)


;;; We intercept compilation to ensure that load-op will succeed
(defmethod perform ((op compile-op) (c mvn))
  (unless (resolved-classpath c)
    (setf (resolved-classpath c)
          (abcl-asdf:resolve   
           (ensure-parsed-mvn c)))))

(defmethod perform ((operation load-op) (c mvn))
  (let ((resolved-classpath (resolved-classpath c)))
    (when (stringp resolved-classpath)
      (java:add-to-classpath (abcl-asdf:as-classpath resolved-classpath)))))

;;; A Maven URI has the form "mvn:group-id/artifact-id/version"
;;;
;;; Sometimes people write "group-id:artifact-id:version" to refer to
;;; Maven artifacts.  One can use ABCL-ASDF:RESOLVE directly for
;;; serialized references to artifacts of this form.
;;;
;;; Currently we "stuff" the group-id/artifact-id into the 'name' and
;;; use the component 'version' for the version.  Parts of ASDF 
;;; *reallY* want ASDF:VERSION to be a triple of intergers, and never
;;; anything more, so that is part of the motivation behind this effort.
(defparameter *mvn-repositories* nil
  "A list of all Maven repositories encountered in the lifetime of this instance of the implementation.")

#+nil
(defmethod slot-missing ((class mvn) object slot-name operation &optional new-value)
  (setf (slot-value object slot-name) 
        (if new-value
            new-value
            nil)))

(defun ensure-parsed-mvn (component)
  (with-slots (name group-id artifact-id
                    version schema path repository) 
      component
    (when (null asdf::artifact-id) 
      (let ((parsed (abcl-asdf::split-string name "/"))
            (asdf-version-p (slot-boundp component 'version))
            (default-version "LATEST"))
        (cond ((= (length parsed) 3)
               (setf 
                group-id (first parsed)
                artifact-id (second parsed)
                version (third parsed)))
              ((= (length parsed) 2)
               (setf 
                group-id (first parsed)
                artifact-id (second parsed)
                version (if asdf-version-p
                            version
                            default-version)))
              (t
               (error "Failed to construct a mvn reference from name '~A' and version '~A'"
                      name version)))
        (setf schema "mvn")
        (when repository
          (pushnew repository *mvn-repositories*))
        ;;; Always set path to normalized path "on the way out" to
        ;;; contain group-id/artifact-id/version
        ;;; TODO? record repository as well in path of component
        (setf path (format nil "~A/~A/~A" group-id artifact-id version))))
    component))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (export `(mvn iri ensure-parsed-mvn group-id artifact-id version) 
          'asdf))

(defmethod source-file-type ((component iri) (system system))
  nil)

(defmethod component-relative-pathname ((component iri))
  nil)

(in-package #:abcl-asdf)

(defgeneric resolve (something)
  (:documentation "Returns a string in JVM CLASSPATH format as entries delimited by classpath separator string."))

(defmethod resolve ((mvn-component asdf::mvn))
  "Resolve all runtime dependencies of MVN-COMPONENT.

Returns either a string in jvm classpath format as entries delimited
by classpath separator string or T.  If the value T is returned, it
denotes that current JVM already has already loaded a given class. Can possibly be a
single entry denoting a remote binary artifact."
  (asdf:ensure-parsed-mvn mvn-component)
  (let ((name (slot-value mvn-component 'asdf::name))
        (group-id (slot-value mvn-component 'asdf::group-id))
        (artifact-id (slot-value mvn-component 'asdf::artifact-id))
        (classname (slot-value mvn-component 'asdf::classname))
        (alternate-uri (slot-value mvn-component 'asdf::alternate-uri))
        (repository (slot-value mvn-component 'asdf::repository))
        (version (if (slot-value mvn-component 'asdf::version)
                     (slot-value mvn-component 'asdf::version)
                     "LATEST")))
    (handler-case 
        (when (and classname 
                   (jss:find-java-class classname))
          (warn "Not loading ~A from the network because ~A is present in classpath."
                name classname)
          (return-from resolve t))
      (java:java-exception (e)
        (unless (java:jinstance-of-p (java:java-exception-cause e)
                                     "java.lang.ClassNotFoundException")
          (error "Unexpected Java exception~&~A.~&" e))))
    (if (find-mvn)
        (resolve-dependencies group-id artifact-id
                              :version version
                              :repository repository)
        (if alternate-uri
            (values (namestring alternate-uri) alternate-uri) 
            (error "Failed to resolve MVN component name ~A." name)))))

(defmethod resolve ((uri pathname))
  (warn "Unimplemented."))

(defun as-classpath (classpath)
  "Break apart the JVM CLASSPATH string into a list of its consituents."
  (split-string classpath 
                (java:jfield "java.io.File" "pathSeparator")))

(defun split-string (string split-char)
  (loop :for i = 0 :then (1+ j)
     :as j = (position split-char string :test #'string-equal :start i)
     :collect (subseq string i j)
     :while j))
