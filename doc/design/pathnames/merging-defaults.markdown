If *DEFAULT-PATHNAME-DEFAULTS* is a JAR-PATHNAME, commands like 

    CL-USER: (probe-file #"/") 
    
will fail.  

## Other implementations

### SBCL

A host nonce which appears in the reader as
#<SB-IMPL::UNIX-HOST {1000290443}>.  (Is there a different one under
Windows?)

### CLISP

HOST is NIL.


### CCL

HOST is :UNSPECIFIC. 


### ECL

HOST is NIL.




