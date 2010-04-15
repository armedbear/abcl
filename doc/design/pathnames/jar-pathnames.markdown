JARs and JAR entries in ABCL
============================

    Mark Evenson
    Created:  09 JAN 2010
    Modified: 10 APR 2010

Notes towards an implementation of "jar:" references to be contained
in Common Lisp `PATHNAME`s within ABCL.

Goals
-----

1.  Use Common Lisp pathnames to refer to entries in a jar file.

    
2.  Use `'jar:'` schema as documented in [`java.net.JarURLConnection`][jarURLConnection] for
    namestring representation.

    An entry in a JAR file:

         #p"jar:file:baz.jar!/foo"
    
    A JAR file:

         #p"jar:file:baz.jar!/"

    A JAR file accessible via URL

         #p"jar:http://example.org/abcl.jar!/"

    An entry in a ABCL FASL in a URL accessible JAR file

         #p"jar:jar:http://example.org/abcl.jar!/foo.abcl!/foo-1.cls"
         
[jarUrlConnection]: http://java.sun.com/javase/6/docs/api/java/net/JarURLConnection.html

3.  `MERGE-PATHNAMES` working for jar entries in the following use cases:

        (merge-pathnames "foo-1.cls" "jar:jar:file:baz.jar!/foo.abcl!/foo._")
        ==> "jar:jar:file:baz.jar!/foo.abcl!/foo-1.cls"

        (merge-pathnames "foo-1.cls" "jar:file:foo.abcl!/")
        ==> "jar:file:foo.abcl!/foo-1.cls"

4.  TRUENAME and PROBE-FILE working with "jar:" with TRUENAME
    cannonicalizing the JAR reference.

5.  DIRECTORY working within JAR files (and within JAR in JAR).

6.  References "jar:<URL>" for all strings <URL> that java.net.URL can
    resolve works.

7.  Make jar pathnames work as a valid argument for OPEN with
:DIRECTION :INPUT.

8.  Enable the loading of ASDF systems packaged within jar files.

9.  Enable the matching of jar pathnames with PATHNAME-MATCH-P

        (pathname-match-p 
          "jar:file:/a/b/some.jar!/a/system/def.asd"
          "jar:file:/**/*.jar!/**/*.asd")      
        ==> t

Status
------

As of svn r125??, all the above goals have been implemented and
tested.


Implementation
--------------

A PATHNAME refering to a file within a JAR is known as a JAR PATHNAME.
It can either refer to the entire JAR file or an entry within the JAR
file.

A JAR PATHNAME always has a DEVICE which is a proper list.  This
distinguishes it from other uses of Pathname.

The DEVICE of a JAR PATHNAME will be a list with either one or two
elements.  The first element of the JAR PATHNAME can be either a
PATHNAME representing a JAR on the filesystem, or a URL PATHNAME.

A PATHNAME occuring in the list in the DEVICE of a JAR PATHNAME is
known as a DEVICE PATHNAME.

Only the first entry in the the DEVICE list may be a URL PATHNAME.

Otherwise the the DEVICE PATHAME denotes the PATHNAME of the JAR file.

The DEVICE PATHNAME list of enclosing JARs runs from outermost to
innermost.
    
The DIRECTORY component of a JAR PATHNAME should be a list starting
with the :ABSOLUTE keyword.  Even though hierarchial entries in jar
files are stored in the form "foo/bar/a.lisp" not "/foo/bar/a.lisp",
the meaning of DIRECTORY component is better represented as an
absolute path.

A jar Pathname has type JAR-PATHNAME, derived from PATHNAME.


BNF
---

An incomplete BNF of the syntax of JAR PATHNAME would be:

      JAR-PATHNAME ::= "jar:" URL "!/" [ ENTRY ]

      URL ::= <URL parsable via java.net.URL.URL()>
            | JAR-FILE-PATHNAME

      JAR-FILE-PATHNAME ::= "jar:" "file:" JAR-NAMESTRING "!/" [ ENTRY ]

      JAR-NAMESTRING  ::=  ABSOLUTE-FILE-NAMESTRING
                         | RELATIVE-FILE-NAMESTRING

      ENTRY ::= [ DIRECTORY "/"]* FILE


### Notes

1.  `ABSOLUTE-FILE-NAMESTRING` and `RELATIVE-FILE-NAMESTRING` use the
local filesystem conventions, meaning that on Windows this could
contain '\' as the directory separator, while an `ENTRY` always uses '/'
to separate directories within the jar proper.


Use Cases
---------

    // UC1 -- JAR
    pathname: {
      namestring: "jar:file:foo/baz.jar!/"
      device: ( 
        pathname: {  
          device: "jar:file:"
          directory: (:RELATIVE "foo")
          name: "baz"
          type: "jar"
        }
      )
    }


    // UC2 -- JAR entry 
    pathname: {
      namestring: "jar:file:baz.jar!/foo.abcl"
      device: ( pathname: {
        device: "jar:file:"
        name: "baz"
        type: "jar"
      }) 
      name: "foo"
      type: "abcl"
    }


    // UC3 -- JAR file in a JAR entry
    pathname: {
      namestring: "jar:jar:file:baz.jar!/foo.abcl!/"
      device: ( 
        pathname: {
          name: "baz"
          type: "jar"
        }
        pathname: {
          name: "foo"
          type: "abcl"
        } 
      )
    }

    // UC4 -- JAR entry in a JAR entry with directories
    pathname: {
      namestring: "jar:jar:file:a/baz.jar!/b/c/foo.abcl!/this/that/foo-20.cls"
      device: ( 
        pathname {
          directory: (:RELATIVE "a")      
          name: "bar"
          type: "jar"
        }
        pathname {
          directory: (:RELATIVE "b" "c")
          name: "foo"
          type: "abcl"
        }
      )
      directory: (:RELATIVE "this" "that")
      name: "foo-20"
      type: "cls" 
    }

    // UC5 -- JAR Entry in a JAR Entry
    pathname: {
      namestring: "jar:jar:file:a/foo/baz.jar!/c/d/foo.abcl!/a/b/bar-1.cls"
      device: (
        pathname: {
          directory: (:RELATIVE "a" "foo")
          name: "baz"
          type: "jar"
        }
        pathname: {
          directory: (:RELATIVE "c" "d")
          name: "foo"
          type: "abcl"
        }
      )
      directory: (:ABSOLUTE "a" "b")
      name: "bar-1"
      type: "cls"
    }

    // UC6 -- JAR entry in a http: accessible JAR file
    pathname: {
      namestring: "jar:http://example.org/abcl.jar!/org/armedbear/lisp/Version.class",
      device: ( 
        pathname: {
          namestring: "http://example.org/abcl.jar"
        }
        pathname: {
          directory: (:RELATIVE "org" "armedbear" "lisp")
          name: "Version"
          type: "class"
       }
    }

    // UC7 -- JAR Entry in a JAR Entry in a URL accessible JAR FILE
    pathname: {
       namestring  "jar:jar:http://example.org/abcl.jar!/foo.abcl!/foo-1.cls"
       device: (
         pathname: {
           namestring: "http://example.org/abcl.jar"
         }
         pathname: { 
           name: "foo"
           type: "abcl"
         }
      )
      name: "foo-1"
      type: "cls"
    }

    // UC8 -- JAR in an absolute directory

    pathame: {
       namestring: "jar:file:/a/b/foo.jar!/"
       device: (
         pathname: {
           directory: (:ABSOLUTE "a" "b")
           name: "foo"
           type: "jar"
         }
       )
    }

    // UC9 -- JAR in an relative directory with entry
    pathname: {
       namestring: "jar:file:a/b/foo.jar!/c/d/foo.lisp"
       device: (
         directory: (:RELATIVE "a" "b")
         name: "foo"
         type: "jar"
       )
       directory: (:ABSOLUTE "c" "d")
       name: "foo"
       type: "lisp
    }


History
-------

Previously, ABCL did have some support for jar pathnames. This support
used the convention that the if the device field was itself a
pathname, the device pathname contained the location of the jar.

In the analysis of the desire to treat jar pathnames as valid
locations for `LOAD`, we determined that we needed a "double" pathname
so we could refer to the components of a packed FASL in jar.  At first
we thought we could support such a syntax by having the device
pathname's device refer to the inner jar.  But with in this use of
`PATHNAME`s linked by the `DEVICE` field, we found the problem that UNC
path support uses the `DEVICE` field so JARs located on UNC mounts can't
be referenced. via '\\', i.e.  

    jar:jar:file:\\server\share\a\b\foo.jar!/this\that!/foo.java

would not have a valid representation.

So instead of having `DEVICE` point to a `PATHNAME`, we decided that the
`DEVICE` shall be a list of `PATHNAME`, so we would have:

    pathname: {
      namestring: "jar:jar:file:\\server\share\foo.jar!/foo.abcl!/"
      device: ( 
                pathname: {
                  host: "server"
                  device: "share"
                  name: "foo"
                  type: "jar"
                }
                pathname: {
                  name: "foo"
                  type: "abcl"
                }
              )
    }

Although there is a fair amount of special logic inside `Pathname.java`
itself in the resulting implementation, the logic in `Load.java` seems
to have been considerably simplified.

When we implemented URL Pathnames, the special syntax for URL as an
abstract string in the first position of the device list was naturally
replaced with a URL pathname.


