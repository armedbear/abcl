# ISSUE MERGE-PATHNAMES with specialization of JAR-PATHNAME 

We wish to resolve the following issues.

## UC0 Loading jna.jar for CFFI via Quicklisp

Currently we cannount load systems via ASDF recursively in jars, most
importantly for those packaged in 'abcl-contrib.jar', which prevents
CFFI from loading the necessary JNA code.

## UC1.1
If *DEFAULT-PATHNAME-DEFAULTS* is a JAR-PATHNAME, commands that would
normally be expected to work such as 

    CL-USER> (probe-file #p"/") 
    
will fail.  

### UC1.2

If *DEFAULT-PATHNAME-DEFAULTS* is a JAR-PATHNAME, COMPILE-FILE
operations specifying an OUTPUT-FILE with a NIL DEVICE will fail, as
COMPILE-FILE-PATHNAME is required to merge its arguments with the
defaults.

## CLHS Citations

Some especially relevant portions of the CLHS for consideration.

###  19.2.3 Merging Pathnames
http://www.lispworks.com/documentation/HyperSpec/Body/19_bc.htm

"Except as explicitly specified otherwise, for functions that
manipulate or inquire about files in the file system, the pathname
argument to such a function is merged with *default-pathname-defaults*
before accessing the file system (as if by merge-pathnames)."

Note that this implies that the arguments to PARSE-NAMESTRING--which
is what the SHARSIGN-P reader macro correpsponds to--should not be
merged.


## 19.2.2.2.3 :UNSPECIFIC as a Component Value
http://www.lispworks.com/documentation/HyperSpec/Body/19_bbbc.htm

"If :unspecific is the value of a pathname component, the component is
considered to be ``absent'' or to ``have no meaning'' in the filename
being represented by the pathname."

Having an :UNSPECIFIC DEVICE when a PATHNAME refers to a file would
address problems when merging when the defaults contains a JAR-PATHNAME.

### MERGE-PATHNAMES 
http://www.lispworks.com/documentation/HyperSpec/Body/f_merge_.htm

"If pathname explicitly specifies a host and not a device, and if the
host component of default-pathname matches the host component of
pathname, then the device is taken from the default-pathname;
otherwise the device will be the default file device for that host. If
pathname does not specify a host, device, directory, name, or type,
each such component is copied from default-pathname."

This suggests that the contents HOST should be considered as an
additional axis of type for PATHNAME, so that when PATHNAMES of
differing types get merged, a DEVICE which has no meaning for a given
type does not get inserted. 

## Other implementations

A survey of the the "default" host for #p"/" on startup.

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

## Implementation

Since Windows systems do have a default DEVICE for a normal file
PATHNAME, namely the current "drive letter" of the process, the
implementation changes will be mostly wrapped in runtime conditionals
for non-Windows systems.

### TRUENAME sets DEVICE to :UNSPECIFIC

TRUENAME sets DEVICE to :UNSPECIFIC running on non-Windows when
resolving a path to a plain file.


### DIRECTORY sets DEVICE to :UNSPECIFIC

When the default for the :RESOLVE-SYMLINKS argument to DIRECTORY was
changed to nil, DIRECTORY was changed not to always resolve its
results via TRUENAME.  As a result

    (equal (truename "~/.emacs")
           (first (directory "~/.emacs")) )

forms would return nil.  This is a bit counter to expectations set by
CLHS that DIRECTORY "returns a list of pathnames corresponding to the
truenames".  In particular, this breaks the ANSI CL DIRECTORY.[67]
tests.  Thus, under non-Windows we now explicitly normalize DEVICE
components which are nil to :UNSPECIFIC for the results of DIRECTORY
calls.

### Use an implicit type for merging 

In the case for which a merge occurs when DEFAULT-PATHNAME
is a JAR-PATHNAME and the following conditions hold:

    1.  HOST and DEVICE of the PATHNAME are NIL

    2.  The DIRECTORY of the PATHNAME represents an absolute path.

    3.  We are not running under Windows.

we set the DEVICE to be :UNSPECIFIC.

### COLOPHON

Mark <evenson@panix.com>
Created:  01-SEP-2012
Revised:  06-FEB-2014

