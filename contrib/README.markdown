ABCL-CONTRIB
============

The contributions to Armed Bear constitute Common Lisp only code that is
potentially useful for system construction and distribution.


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
   
   
Deprecated
----------

asdf-install
    
    Install ASDF system definitions from the network.  
    
    Deprecated, use Quicklisp from the REPL via
    
        CL-USER> (load "http://beta.quicklisp.org/quicklisp.lisp")
       
    instead.


