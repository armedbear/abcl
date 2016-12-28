ABCL-CONTRIB
============

The contributions to Armed Bear constitute Common Lisp only code that
is potentially useful for system construction and distribution.

quicklisp-abcl

    Loads and installs the Quicklisp library manager from the network
    if not locally present.

abcl-asdf 

    ABCL specific extensions to ASDF, including resolution of binary
    JVM artifacts cached from the network according to Maven metadata
    with the derived transitive relationships.
    

asdf-jar

    Package ASDF system definitions into JVM artifacts for
    distribution
    

jss
    
    A higher-order, more Lisp oriented interface for constructing Lisp
    interfaces to existing binary code libraries available for the JVM
    built on the primitives provided by the JAVA package.  Used in the
    [lsw2][] Semantic Web package for dealing with OWL2 ontologies in
    RDF(S) and other notations.
    
[lsw2]: http://code.google.com/p/lsw2/    
    
jfli

    The "original" higher-order JVM interface descended from Rich
    Hickey's work on the JVM before Clojure.  This implementation
    currently uses a fork of the public [JFLI][] API that uses the
    java interop of the ABCL JAVA package instead of the JNI
    interface.
    
[jfli]: http://sourceforge.net/projects/jfli/
   
mvn

    A collection of various useful JVM artifacts downloaded and cached
    by the Aether Maven connector.  Requires the maven-3.0.3
    executable "mvn" (or "mvn.bat" under MSFT Windows) to be in the
    current processes's path. These artifacts load the binary
    artifacts necessary in the current JVM process, 

    
    mvn currently includes:
    
    jna
        JNA provides an the ability to dynamically link to shared
        executables on the host platform, needed by CFFI.

    log4j
        An example of a dependency without an explicit version.

abcl-introspect

    Advanced introspection of Java and Lisp runtime classes
    representation.

# Colophon

Mark <evenson.not.org@gmail.com>
Created:  2011-09-11
Revised:  2016-12-28
<> abcl:documents <abcl.org/release/1.5.0/#abcl-contrib.jar> .



