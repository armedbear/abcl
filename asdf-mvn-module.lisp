(in-package :cl-user)
(defpackage mvn-module
  (:use :cl))
(in-package :mvn-module)

(in-package :asdf)
 ;; dependencies: a list of maven artifacts. color or slash separated
 ;;   components groupid:artifactid:versionid

 ;; managed-dependencies: a list of maven artifacts. If an dependency
 ;;   with same groupid and artifactid are encountered, the version
 ;;   specified here overrides.

 ;; exclusions: a list of partial maven artifacts
 ;;   groupid:artifactid. Dependencies with same groupid and artifactid are
 ;;   exluded

(defclass mvn-module (component) 
  ((depends :initarg :dependencies :initform nil :accessor mvn-module-depends)
   (excludes :initarg :exclusions :initform nil :accessor mvn-module-excludes)
   (managed :initarg :managed-dependencies :initform nil :accessor mvn-module-managed)))

(defmethod component-children ((c mvn-module))
  nil)

;;; TODO move to initarg
(defmethod source-file-type ((c mvn-module) (system parent-component))  nil)

(defmethod perform ((op compile-op) (c mvn-module))
  )

(defmethod perform ((op prepare-op) (c mvn-module))
  )

(defmethod perform ((operation load-op) (c mvn-module))
  (loop for path
     in (asdf-mvn-module:resolve-multiple-maven-dependencies
         (mvn-module-depends c)
         (mvn-module-managed c)
         (mvn-module-excludes c))
	do
	   (unless (member path abcl-asdf::*added-to-classpath* :test 'equalp)
	     (jss::add-to-classpath  path))))






