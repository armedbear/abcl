(defpackage #:abcl-asdf
  (:use :cl)
  (:export 
   #:satisfy
   #:as-classpath

   #:resolve-artifact
   #:resolve-dependencies

   #:add-directory-jars-to-class-path
   #:need-to-add-directory-jar?
   
   #:*added-to-classpath*
   #:*inhibit-add-to-classpath*))

(in-package :asdf)
(defclass iri (static-class) 
  (schema authority path query fragment))

(defclass mvn (iri) ())

;;; We interpret compilation to ensure that load-op will succeed
(defmethod perform ((op compile-op) (c mvn))
    (let ((version (component-version c)))
      (abcl-asdf:satisfy (component-name c) 
                         :version (if version version :latest))))

(defmethod perform ((operation load-op) (c mvn))
    (let ((version (component-version c)))
      (java:add-to-classpath 
       (abcl-asdf:as-classpath 
        (abcl-asdf:satisfy (component-name c)
                     :version (if version version :latest))))))

(in-package #:abcl-asdf)

(defun satisfy (name &key (version :latest))
  (declare (ignore version))
  (resolve-dependencies name))
           
(defun as-classpath (classpath)
  "For a given MVN entry, return a list of loadable archives 
 suitable for addition to the classpath."
  (split-string classpath ":"))

(defun split-string (string split-char)
  (loop :for i = 0 :then (1+ j)
     :as j = (position split-char string :test #'string-equal :start i)
     :collect (subseq string i j)
     :while j))