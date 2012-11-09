ABCL-ASDF
=========

To use:

    CL-USER> (require 'abcl-contrib)

    CL-USER> (require 'abcl-asdf)
    
    
ABCL specific contributions to ASDF system definition mainly concerned
with finding JVM artifacts such as jar archives to be dynamically loaded.

Example 1
---------

For the following ASDF definition stored in a file named "log4j.asd"
that loadable by ASDF

    ;;;; -*- Mode: LISP -*-
    (in-package :asdf)

    (defsystem log4j
      :components ((:mvn "log4j/log4j/1.2.13")))

After issuing 

    CL-USER> (asdf:load-system :log4j)
    
all the Log4j libraries would be dynamically added to the classpath so
that the following code would

    (let ((logger (#"getLogger" 'log4j.Logger (symbol-name (gensym)))))
      (#"trace" logger "Kilroy wuz here."))
 
output the message "Kilroy wuz here" to the log4j logging system.
      

API
---

We define an API within the ASDF package consisting of the following
ASDF classes derived from ASDF:COMPONENT:

JAR-DIRECTORY, JAR-FILE, and CLASS-FILE-DIRECTORY for JVM artifacts
that have a currently valid pathname representation (i.e. they exist
on the local filesystem).

The MVN and IRI classes descend from ASDF-COMPONENT, but do not
directly have a filesystem location.

The IRI component is currently unused, but serves as a point to base
the inheritance of the MVN component while allowing other forms of
uri-like resources to be encapsulated in the future.

The MVN component should specifiy a [Maven URI][1] as its PATH.  A
Maven URI has the form "GROUP-ID/ARTIFACT-ID/VERSION" which specifies
the dependency to be satisfied for this component by resolution
through the Maven distributed dependency graph.  The scheme (the
initial "mvn://") is implied, usually omitted for brevity.  If a
VERSION is not specified (i.e. by a form like "GROUP-ID/ARTIFACT-ID"),
then the latest available version of the artifact will be retrieved
from the network.

[1]: http://team.ops4j.org/wiki/display/paxurl/Mvn+Protocol

The MVN component may specify a CLASSNAME which if present in the
current jvm, inhibits further loading from the network.  This may be
used to bypass the invocation of Maven.  Since classnames are not
unique to jar archives, this mechanism may not have the desired result
in all cases, but it is surpisingly, like the rest of Java, "good
enough" for everyday use.

The MVN component may specify an ALTERNATE-URI which will be added to
the jvm classpath if Maven cannot be located.  Since a Maven URI may
refer to more than one binary artifact, this may not work in all cases.

For use outside of ASDF, we currently define the generic function
ABCL-ASDF:RESOLVE which locates, downloads, caches, and then loads
into the currently executing JVM process all recursive dependencies
annotated in the ditributed Maven pom.xml graph.

One can muffle the verbosity of the Maven Aether resolver by setting
ABCL-ASDF:*MAVEN-VERBOSE* to NIL.

Example 2
---------

Bypassing ASDF, one can directly issue requests for the Maven
artifacts to be downloaded

    CL-USER> (abcl-asdf:resolve "com.google.gwt:gwt-user")
    WARNING: Using LATEST for unspecified version.
    "/Users/evenson/.m2/repository/com/google/gwt/gwt-user/2.4.0-rc1/gwt-user-2.4.0-rc1.jar:/Users/evenson/.m2/repository/javax/validation/validation-api/1.0.0.GA/validation-api-1.0.0.GA.jar:/Users/evenson/.m2/repository/javax/validation/validation-api/1.0.0.GA/validation-api-1.0.0.GA-sources.jar"

Notice that all recursive dependencies have been located and installed
as well.

ABCL-ASDF:RESOLVE does not added the resolved dependencies to the
current JVM classpath.  Use JAVA:ADD-TO-CLASSPATH as follows to do
that:

    CL-USER> (java:add-to-classpath (abcl-asdf:as-classpath (abcl-asdf:resolve "com.google.gwt:gwt-user")))

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

Releases
--------

### 9.9.2 2012-11-09




### 0.7.0 2012-02-05

Plausibly work under MSFT operating systems.

Working with maven-3.0.4.

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
    Revised: 2012-11-09
    
