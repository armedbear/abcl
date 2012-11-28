ABCL-CONTRIB
============

The contributions to Armed Bear constitute Common Lisp only code that
is potentially useful for system construction and distribution.


abcl-asdf 

    ABCL specific extensions to ASDF, including resolution of binary
    JVM artifacts cached from the network according to Maven metadata
    with the derived transitive relationships.
    

asdf-jar

    Package ASDF system definitions into JVM artifacts for distribution
    

jss
    
    A higher-order, more Lisp oriented interface for constructing Lisp
    interfaces to existing binary code libraries available for the JVM
    built on the primitives provided by the JAVA package.
    
jfli

    The "original" higher-order JVM interop descended from Rich
    Hickey's work on the JVM before Clojure.  This implementation
    currently uses a fork of the public [JFLI][] API that uses the
    java interop of the ABCL JAVA package instead of the JNI
    interface.  
    
[jfli]: http://sourceforge.net/projects/jfli/
    
   
mvn
---
   
A collection of various useful JVM artifacts downloaded and cached by
the Aether Maven connector.  Requires the maven-3.0.3 executable "mvn"
(or "mvn.bat" under MSFT Windows) to be in the current processes's path.
    
jna     
    Cache, from the network if necessary, the jna-3.4.0.jar in
    the current JVM process, allowing the bootstrapping of
    dynamically linking to shared executables on the host platform.


Deprecated
----------

asdf-install
    
    Install ASDF system definitions from the network.  
    
    Deprecated, use Quicklisp from the REPL via
    
        CL-USER> (load "https://beta.quicklisp.org/quicklisp.lisp")
       
    instead.

# Colophon

Mark <evenson.not.org@gmail.com>
Created:  2011-09-11
Revised:  2012-11-28



