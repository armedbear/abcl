ABCL-CONTRIB
============

The contributions to Armed Bear consitute Common Lisp only code that is
potentially useful for system construction and distribution.


abcl-asdf 

    ABCL specific extensions to ASDF, including resolution of binary
    JVM artifacts cached from the network according to Maven metadata
    with the derived transitory relationships.
    

asdf-jar

    Package ASDF system defintions into JVM artifacts for distribution
    

jss
    
   High-order, more Lisp oriented interface to dealing with
   constructing Lisp interfaces to existing binary code libraries
   available for the JVM
   

asdf-install
    
    Install ASDF system definitions from the network.  
    
    Deprecated, use
    
        CL-USER> (load "https://quicklisp.org/quicklisp.lisp")
       
    instead.


