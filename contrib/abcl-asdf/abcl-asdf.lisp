;;;; The ABCL specific overrides in ASDF.  
;;;;
;;;; Done separate from asdf.lisp for stability.
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
   (repository :initform "http://repo1.maven.org/maven2/") ;;; XXX unimplmented
;; inherited from ASDF:COMPONENT ??? what are the CL semantics on overriding -- ME 2012-04-01
#+nil   (version :initform nil)))

#+nil
(defmethod find-component ((component iri) path)
  component)


;;; We intercept compilation to ensure that load-op will succeed
(defmethod perform ((op compile-op) (c mvn))
  (abcl-asdf:resolve   
   (ensure-parsed-mvn c)))
     
(defmethod perform ((operation load-op) (c mvn))
  (java:add-to-classpath 
   (abcl-asdf:as-classpath 
    (abcl-asdf:resolve 
     (ensure-parsed-mvn c)))))

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
        (pushnew repository *mvn-repositories*)
        ;;; Always normalized path "on the way out" to contain group-id/artifact-id/version
        (setf path (format nil "~A/~A/~A" group-id artifact-id version))))
    component))

(export `(mvn iri ensure-parsed-mvn
              group-id artifact-id version) 'asdf)

(defmethod source-file-type ((component iri) (system system))
  nil)

(defmethod component-relative-pathname ((component iri))
  nil)

(in-package #:abcl-asdf)

(defgeneric resolve (something)
 (:documentation "Returns a string in JVM CLASSPATH format as entries delimited by classpath separator string."))

(defmethod resolve ((mvn-component asdf::mvn))
  "Resolve all runtime dependencies of MVN-COMPONENT.

Returns a string in JVM CLASSPATH format as entries delimited by
classpath separator string.  Can possibly be a single entry denoting a
remote binary artifact."
  (let ((name (asdf::component-name mvn-component)))
    (if (find-mvn)
        (with-slots (asdf::group-id asdf::artifact-id asdf::version) mvn-component
          (resolve-dependencies asdf::group-id asdf::artifact-id asdf::version))
        (cond 
          ((string= name
                    "net.java.dev.jna/jna/3.4.0"
                    (let ((uri #p"http://repo1.maven.org/maven2/net/java/dev/jna/jna/3.4.0/jna-3.4.0.jar"))
                      (values (namestring uri) uri))))
          (t 
           (error "Failed to resolve MVN component name ~A." name))))))

(defun as-classpath (classpath)
  "Break apart the JVM CLASSPATH string into a list of its consituents."
  (split-string classpath 
                (java:jfield "java.io.File" "pathSeparator")))

(defun split-string (string split-char)
  (loop :for i = 0 :then (1+ j)
     :as j = (position split-char string :test #'string-equal :start i)
     :collect (subseq string i j)
     :while j))
