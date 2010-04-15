URL Pathnames ABCL
==================

    Mark Evenson
    Created:  25 MAR 2010
    Modified: 11 APR 2010

Notes towards an implementation of URL references to be contained in
Common Lisp `PATHNAME` objects within ABCL.


References
----------

RFC3986   Uniform Resource Identifier (URI): Generic Syntax


URL vs URI
----------

We use the term URL to describe the URL Pathnames, even though RFC3986
notes that its use should be obsolete because in the context of Common
Lisp Pathnames all need a lookup mechanism to be resolved or they
wouldn't be of much use.

Goals
-----

1.  Use Common Lisp pathnames to refer to representations referenced
by a URL.

2.  The URL schemes supported shall include at least "http", and those
enabled by the URLStreamHandler extension mechanism.

3.  Use URL schemes that are understood by the java.net.URL object.

    A file specified by URL
    
        #p"http://example.org/org/armedbear/systems/pgp.asd"
    
4.  MERGE-PATHNAMES 

        (merge-pathnames "url.asd"
            "http://example/org/armedbear/systems/pgp.asd")
        ==> "http://example/org/armedbear/systems/url.asd"

5.  PROBE-FILE returning the state of URL accesibility.

6.  TRUENAME "aliased" to PROBE-FILE signalling an error if the URL is
not accessible (see "Non-goal 1").

7.  DIRECTORY for non-wildcards

8.  URL pathname work as a valid argument for OPEN with :DIRECTION :INPUT.

9.  Enable the loading of ASDF2 systems referenced by a URL pathname.

10.  The reserved URL characters (`~`, `/`, `?`, etc.) shall be
encoded in the proper manner on construction of the Pathname.

11.  The "file" scheme will continue to be represented by an
"ordinary" Pathname.

12.  The "jar" scheme will continue to be represented by a jar
Pathname.


Non-goals 
---------

1.  We will not implement canonicalization of URL schemas (such as following
"http" redirects).

2.  DIRECTORY working for URL pathnames containing wildcards.


Implementation
--------------

A PATHNAME refering to a resource referenced by a URL is known as a
URL PATHNAME.

A URL PATHNAME always has a HOST component which is a proper list.
This list will be an property list (plist).  The property list
values must be character strings.

    :SCHEME
        Scheme of URI ("http", "ftp", "bundle", etc.)
    :AUTHORITY   
        Valid authority according to the URI scheme.  For "http" this
        could be "example.org:8080".
    :QUERY
        The query of the URI
    :FRAGMENT
        The fragment portion of the URI
        
The DIRECTORY, NAME and TYPE fields of the PATHNAME are used to form
the URI `path` according to the conventions of the UNIX filesystem
(i.e. '/' is the directory separator).  In a sense the HOST contains
the base URL, to which the `path` is a relative URL (although this
abstraction is violated somwhat by the storing of the QUERY and
FRAGMENT portions of the URI in the HOST component).

For the purposes of PATHNAME-MATCH-P, two URL pathnames may be said to
match if their HOST compoments are EQUAL, and all other components are
considered to match according to the existing rules for Pathnames.

A URL pathname must have a DEVICE whose value is NIL.

Upon creation, the presence of ".." and "." components in the
DIRECTORY are removed.  The DIRECTORY component, if present, is always
absolute.

The namestring of a URL pathname shall be formed by the usual
conventions of a URL.

A URL Pathname has type URL-PATHNAME, derived from PATHNAME.

Status
------

This design is a proposal.
