# ISSUE MERGE-PATHNAMES with specialization of JAR-PATHNAME and URL-PATHNAME

## UC0 Loading jna.jar for CFFI via Quicklisp

This happens in loading systems via ASDF recursively in jars (abcl-contrib.jar)

## UC1
If *DEFAULT-PATHNAME-DEFAULTS* is a JAR-PATHNAME, commands like 

    CL-USER: (probe-file #"/") 
    
will fail.  


## Questions

###  19.2.3 Merging Pathnames

Except as explicitly specified otherwise, for functions that
manipulate or inquire about files in the file system, the pathname
argument to such a function is merged with *default-pathname-defaults*
before accessing the file system (as if by merge-pathnames).

Q: Does this mean that the arguments to PARSE-NAMESTRING should not be
merged? 

## Other implementations

What is the "default" host for #p"/"?

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
Revised:  01-OCT-2012

