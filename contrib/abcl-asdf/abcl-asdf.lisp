(defpackage #:abcl-asdf
  (:use :cl)
  (:export #:package))


(defpackage #:mvn
  (:use :cl)
  (:export #:satisfy
           #:as-classpath))

(in-package :asdf)
(defclass iri (static-class) ())

(defclass mvn (iri) ())

;;; We interpret compilation to ensure that load-op will succeed
(defmethod perform ((op compile-op) (c mvn))
    (let ((version (component-version c)))
      (mvn:satisfy (component-name c) 
                   :version (if version version :latest))))

(defmethod perform ((operation load-op) (c mvn))
    (let ((version (component-version c)))
      (java:add-to-classpath 
       (mvn:as-classpath 
        (mvn:satisfy (component-name c)
                     :version (if version version :latest))))))

(in-package :abcl-asdf)

(defun decompose (iri) 
  (declare (ignore iri))
  ;;; XXX test 
  `((:scheme :jvm)
    (:authority :mvn)
    (:host "log4j")
    (:version "1.4.10")))

(in-package :mvn)

(defparameter *maven-ant-tasks.jar*
  "/export/home/evenson/src/apache-maven-3.0.3/maven-ant-tasks-2.1.1.jar")

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
         xmlns:artifact='antlib:org.apache.maven.artifact.ant'
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

(defun satisfy (name &key (version :latest))
  (declare (ignore name version))
  (let ((build.xml (ext:make-temp-file)))
    (with-open-file (s build.xml :direction :output)
      (write-string *ant-build-template* s ))
    (ext:run-program 
     (format nil "ant -find ~A -lib ~A" 
             build.xml
             *maven-ant-tasks.jar*))))

(defun as-classpath (mvn)
  "For a given MVN entry, return a list of loadable archives 
 suitable for addition to the classpath."
  (declare (ignore mvn))
  (error "unimplemented"))
                       



