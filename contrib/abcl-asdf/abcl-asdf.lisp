(defpackage #:abcl-asdf
  (:use :cl)
  (:export #:package))

(in-package :asdf-jar)

(in-package :asdf)
(defclass iri (static-class) ())

(defclass mvn (iri) ())

;;; We interpret compilation to ensure that load-op will succeed
(defmethod perform ((operation compile-op) (component mvn))
    (let ((version (component-version mvn)))
      (mvn:satisfy (component-name mvn) 
                   :version (if version version :latest))))

(defmethod perform ((operation load-op) (component mvn))
    (let ((version (component-version mvn)))
      (java:add-to-classpath 
       (as-classpath (mvn:satisfy (component-name mvn)
                                  :version (if version version :latest))))))

(defun decompose (iri) 
  ;;; XXX test 
  `((:scheme :jvm)
    (:authority :mvn)
    (:host "log4j")
    (:version "1.4.10")))

(defun mvn:satisfy (name &key (version :latest))
  ;;; XXX either invoke mvn in the same jvm or fork a process)
