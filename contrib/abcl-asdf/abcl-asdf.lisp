(defpackage #:abcl-asdf
  (:use :cl)
  (:export #:package))

(defpackage #:mvn
  (:use :cl)
  (:export #:satisfy))

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
  (let ((build.xml (make-temp-file)))
    (with-open-file (s build.xml :direction :output)
      (write-string *ant-build-template* s ))
    (run-program 
     (format nil "ant -find ~A" build.xml))))

#|

Ant with Maven tasks would add the following

  <artifact:dependencies pathId="abcl.dynamic.classpath">
    <dependency groupId="junit" artifactId="junit" version="3.8.2"/>
  </artifact:dependencies>
|#

(defvar *ant-build-template*
  (format nil
  "<?xml version='1.0' encoding='UTF-8'?>
<project xmlns='antlib:org.apache.tools.ant'
         name='abcl-mvn-~A' default='install'>

  <artifact:dependencies pathId='abcl.dynamic.classpath'>
    <dependency groupId='~A' artifactId='~A' version='~A'/>
  </artifact:dependencies>

  <path id='abcl.jar'> 
    <pathelement location='/export/home/evenson/work/abcl/dist/abcl.jar'/>
  </path>

  <target name='install'>
    <java classname='org.armedbear.lisp.Main'>
      <classpath refid='abcl.jar'>
      <classpath refid='abcl.dynamic.classpath'>
    </java>
  </target>
</project>
" (symbol-name (gensym)) "junit" "junit" "3.8.2"))




  

