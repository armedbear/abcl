(in-package :asdf)

(defclass iri (component) 
  ((schema :initform nil)
   (authority :initform nil)
   (path :initform nil)
   (query :initform nil)
   (fragment :initform nil)))

(defclass mvn (iri) 
  ((group-id :initform nil)
   (artifact-id :initform nil)
;; inherited from ASDF:COMPONENT
#+nil   (version :initform nil)))

#+nil
(defmethod find-component ((component iri) path)
  component)

;;; We intercept compilation to ensure that load-op will succeed
(defmethod perform ((op compile-op) (c mvn))
  (maybe-parse-mvn c)
  (abcl-asdf:resolve c))
     
(defmethod perform ((operation load-op) (c mvn))
  (maybe-parse-mvn c)
  (java:add-to-classpath 
   (abcl-asdf:as-classpath 
    (abcl-asdf:resolve c))))

;;; A Maven URI has the form "mvn:group-id/artifact-id/version"
;;;
;;; Currently we "stuff" the group-id/artifact-id into the 'name' and
;;; use the component 'version' for the version.  Parts of ASDF 
;;; *reallY* want ASDF:VERSION to be a triple of intergers, and never
;;; anything more, so that is part of the motivation behind this effort.
;;; ??? rename me to ENSURE-MVN-PARSE ??
(defun maybe-parse-mvn (component)
  (with-slots (name group-id artifact-id
               version schema path) 
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
        ;;; Always normalized path "on the way out" to contain group-id/artifact-id/version
        (setf path (format nil "~A/~A/~A" group-id artifact-id version))))))

(defmethod source-file-type ((component iri) (system system))
  nil)

(defmethod component-relative-pathname ((component iri))
  nil)

(in-package #:abcl-asdf)

(defgeneric resolve (something)
 (:documentation "Returns a string in JVM CLASSPATH format as entries delimited by classpath separator string."))

(defmethod resolve ((mvn-component asdf::mvn))
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
