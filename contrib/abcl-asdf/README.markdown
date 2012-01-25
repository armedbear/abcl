ABCL-ASDF
=========

To use:

    CL-USER> (require 'abcl-contrib)

    CL-USER> (require 'abcl-asdf)
    
    
ABCL specific contributions to ASDF system definition mainly concerned
with finding JVM artifacts such as jar archives to be dynamically loaded.

Examples
--------

    ;;;; -*- Mode: LISP -*-
    (in-package :asdf)

    (defsystem :log4j
      :components ((:mvn "log4j/log4j" 
                    :version "1.4.9")))

API
---

We define an API as consisting of the following ASDF classes:

JAR-DIRECTORY, JAR-FILE, and CLASS-FILE-DIRECTORY for JVM artifacts
that have a currently valid pathname representation 

And the MVN and IRI classes descend from ASDF-COMPONENT, but do not
directly have a filesystem location.

For use outside of ASDF, we currently define one method,
RESOLVE-DEPENDENCIES which locates, downloads, caches, and then loads
into the currently executing JVM process all recursive dependencies
annotated in the Maven pom.xml graph.

Example 2
---------

Bypassing ASDF, one can directly issue requests for the Maven
artifacts to be downloaded

    CL-USER> (abcl-asdf:resolve-dependencies "com.google.gwt" "gwt-user")
    WARNING: Using LATEST for unspecified version.
    "/Users/evenson/.m2/repository/com/google/gwt/gwt-user/2.4.0-rc1/gwt-user-2.4.0-rc1.jar:/Users/evenson/.m2/repository/javax/validation/validation-api/1.0.0.GA/validation-api-1.0.0.GA.jar:/Users/evenson/.m2/repository/javax/validation/validation-api/1.0.0.GA/validation-api-1.0.0.GA-sources.jar"

Notice that all recursive dependencies have been located and installed
as well.


Example 3
---------

For a filesystem of jar archives:

    ./lib/ext/flora2-reasoner/XSBFlora.jar
    ./lib/ext/iris-reasoner/iris/iris-0.58.jar
    ./lib/ext/iris-reasoner/jgrapht/jgrapht-jdk1.5-0.7.1.jar
    ./lib/ext/log4j/log4j-1.2.14.jar
    ./lib/ext/mandrax-reasoner/commons-collections-2.1.jar
    ./lib/ext/mandrax-reasoner/jdom-b10.jar
    ./lib/ext/mandrax-reasoner/log4j-1.2.8.jar
    ./lib/ext/mandrax-reasoner/mandarax-3.4.jar
    ./lib/ext/mins-reasoner/mins-v0_3.jar
    ./lib/ext/pellet-reasoner/aterm/1.6/aterm-java-1.6.jar
    ./lib/ext/pellet-reasoner/commons-logging/1.1/commons-logging-1.1.jar
    ./lib/ext/pellet-reasoner/kaon/1.2.9/rdfapi.jar
    ./lib/ext/pellet-reasoner/owl-api/1.4.3/abstractparser.jar
    ./lib/ext/pellet-reasoner/owl-api/1.4.3/io.jar
    ./lib/ext/pellet-reasoner/owl-api/1.4.3/rdfparser.jar
    ./lib/ext/pellet-reasoner/owl-api/1.4.3/validation.jar
    ./lib/ext/pellet-reasoner/owl-api/owl-api-econn/2006-04-27/api.jar
    ./lib/ext/pellet-reasoner/owl-api/owl-api-econn/2006-04-27/impl.jar
    ./lib/ext/pellet-reasoner/pellet/pellet.jar
    ./lib/ext/pellet-reasoner/relaxng/1.0/relaxngDatatype.jar
    ./lib/ext/pellet-reasoner/xsdlib/xsdlib.jar
    ./lib/ext/wsmo/WSML-grammar-20081202.jar
    ./lib/ext/wsmo/wsmo-api-0.6.2.jar
    ./lib/ext/wsmo/wsmo4j-0.6.2.jar
    ./lib/ext/xsb-system/interprolog.jar

The following ASDF defintion loads enough JVM artifacts to use the
[IRIS reasoner][1]:

    (defsystem :wsml2reasoner-jars
      :version "0.6.4"  ;; last sync with SVN
      :depends-on (:abcld) :components 
    ((:module wsml2reasoner 
	    :pathname "lib/" :components
	    ((:jar-file "wsml2reasoner")))
      (:module iris-libs 
  	    :pathname "lib/ext/iris-reasoner/iris/" :components
	    ((:jar-file "iris-0.58")))
      (:module jgrapht-libs 
	    :pathname "lib/ext/iris-reasoner/jgrapht/" :components
	    ((:jar-file "jgrapht-jdk1.5-0.7.1")))
      (:module wsmo-libs
 	    :pathname "lib/ext/wsmo/" :components
	    ((:jar-file "WSML-grammar-20081202")
	     (:jar-file "wsmo-api-0.6.2")
	     (:jar-file "wsmo4j-0.6.2")))
      (:module log4j-libs
	 :pathname "lib/ext/log4j/" :components
	      ((:jar-file "log4j-1.2.14")))))

[1]:  http://www.iris-reasoner.org/


Problems
--------

### 0.5.0 2012-01-22

   o  just bless this as a release to stablize its offered API "as is"
   
   o  definitely failing under MSFT
   
   o  ASDF version has to be a three value integer (i.e. no "-snapshot"
      after version).  Should be fixed with appropiate :AROUND method
      as implementation specific monkeypatch.
       

### 0.4.1 2011-09-06 

    o  locating the proper Maven3 libraries could work in more places
   
    o  untested under Windows
   
    o  more information should be optionally available when downloading
       as this process can potentially take a long time.


#### Colophon

    Mark <evenson.not.org@gmail.com>
    
    Created: 2011-01-01
    Revised: 2012-01-24
    
