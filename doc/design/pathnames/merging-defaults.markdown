# ISSUE MERGE-PATHNAMES with specialization of JAR-PATHNAME and URL-PATHNAME

## UC0 Loading jna.jar for CFFI via Quicklisp

This happens in loading systems via ASDF recursively in jars (abcl-contrib.jar)

## UC1
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




### Colophon

Mark <evenson@panix.com>
Created:  01-SEP-2012
Revised:  30-SEP-2012

