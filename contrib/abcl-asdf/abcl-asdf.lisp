(in-package :asdf)
(defclass iri (component) 
  ((schema :initform nil)
   (authority :initform nil)
   (path :initform nil)
   (query :initform nil)
   (fragment :initform nil)))

(defclass mvn (iri) 
  ((group-id :initform nil)
   (artifact-id :initform nil)))

#+nil
(defmethod find-component ((component iri) path)
  component)

;;; We intercept compilation to ensure that load-op will succeed
(defmethod perform ((op compile-op) (c mvn))
  (maybe-parse-mvn c)
  (abcl-asdf:satisfy c))
     
(defmethod perform ((operation load-op) (c mvn))
  (maybe-parse-mvn c)
  (java:add-to-classpath 
   (abcl-asdf:as-classpath 
    (abcl-asdf:satisfy c))))

;;; A Maven URI has the form "mvn:group-id/artifact-id/version"
;;;
;;; Currently we "stuff" the group-id/artifact-id into the 'name' and
;;; use the component 'version' for the version string.
(defun maybe-parse-mvn (component)
  (with-slots (asdf::name asdf::group-id asdf::artifact-id
               asdf::version asdf::schema asdf::path) component
    (when (null asdf::artifact-id) 
      (let ((slash (search "/" name)))
        (unless (and (integerp slash)
                     asdf::version)
          (error "Failed to construct a mvn reference from name '~A' and version '~A'"
                 asdf::name asdf::version))
        (setf asdf::group-id (subseq asdf::name 0 slash)
              asdf::artifact-id (subseq asdf::name (1+ slash))
              asdf::schema "mvn"
              asdf::version (if (eq asdf::version :latest)
                                "LATEST"
                                asdf::version)
              asdf::path (format nil "~A/~A" asdf::name asdf::version))))))

(defmethod source-file-type ((component iri) (system system))
  nil)

(defmethod component-relative-pathname ((component iri))
  nil)

(in-package #:abcl-asdf)

(defgeneric satisfy (something)
 :documentation "Returns a string in JVM CLASSPATH format as entries delimited by classpath separator string."

(defmethod satisfy ((mvn-component asdf::mvn))
  "Resolve all runtime dependencies of MVN-COMPONENT.

Returns a string in JVM CLASSPATH format as entries delimited by classpath separator string."
  
  (with-slots (asdf::group-id asdf::artifact-id asdf::version) mvn-component
    (resolve-dependencies asdf::group-id asdf::artifact-id asdf::version)))

(defun as-classpath (classpath)
  "Break apart the JVM CLASSPATH string into a list of its consituents."
  (split-string classpath 
                (java:jfield "java.io.File" "pathSeparator")))

(defun split-string (string split-char)
  (loop :for i = 0 :then (1+ j)
     :as j = (position split-char string :test #'string-equal :start i)
     :collect (subseq string i j)
     :while j))
