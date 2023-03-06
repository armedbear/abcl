ASDF-JAR
========

ASDF-JAR provides a system for packaging ASDF systems into jar
archives for ABCL.  Given a running ABCL image with loadable ASDF
systems the code in this package will recursively package all the
required source and fasls in a jar archive .

To install ASDF systems, [Quicklisp]() is probably the best
contemporary solution.  The QUICKLISP-ABCL <file:../quicklisp-abcl>
may be used to install Quicklisp at runtime from within ABCL.

[Quicklisp]: http://www.quicklisp.org

Once the requisite ASDF systems have been installed, ensure that this
contrib is loaded via

    CL-USER) (require :abcl-contrib)
    CL-USER> (asdf:make :asdf-jar)

Then, one may package any locally loadable ASDF system via 
ASDF-JAR:PACKAGE as follows:

     CL-USER> (asdf-jar:package :chunga :verbose t)
     Packaging ASDF definition of #<ASDF/SYSTEM:SYSTEM "chunga">
     Performing unforced compilation of /var/tmp/chunga-all-1.1.7.jar.
     Packaging contents in '/var/tmp/chunga-all-1.1.7.jar'.
     Packaging with recursive dependencies #<ASDF/SYSTEM:SYSTEM "trivial-gray-streams">.
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/streams.lisp
      =>chunga/streams.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/input.lisp
      =>chunga/input.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/specials.lisp
      =>chunga/specials.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/known-words.lisp
      =>chunga/known-words.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/util.lisp
      =>chunga/util.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/read.lisp
      =>chunga/read.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/output.lisp
      =>chunga/output.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/conditions.lisp
      =>chunga/conditions.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/chunga-20221106-git/packages.lisp
      =>chunga/packages.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/trivial-gray-streams-20210124-git/streams.lisp
      =>trivial-gray-streams/streams.lisp
     /Users/evenson/quicklisp/dists/quicklisp/software/trivial-gray-streams-20210124-git/package.lisp
      =>trivial-gray-streams/package.lisp
     #P"/var/tmp/chunga-all-1.1.7.jar"
     #<EQUAL HASH-TABLE 13 entries, 22 buckets {5368E7A9}>

    
The resulting jar contains the source required to run the ASDF system
including any transitive ASDF dependencies.  Each such system is
packaged under its own top level directory within the jar archive.

To load the system from the jar one needs to add the ASDF file
locations to the ASDF source registry, conveniently abstracted as the
ASDF-JAR:ADD-TO-JAR function:

    CL-USER> (asdf-jar:add-to-asdf "/var/tmp/chunga-all-1.1.7.jar)

a subsequent

    CL-USER> (asdf:load-system :chunga)

should load the ASDF system from the jar.

Setting CL:*LOAD-VERBOSE* will allow one to verify that the subsequent
load is indeed coming from the jar.

# Colophon

     Mark Evenson
     Created: 20-JUN-2011
     Revised: 01-APR-2023
     
     
