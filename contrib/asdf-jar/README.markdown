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
    CL-USER> (require :asdf-jar)

Then, to say package the Perl regular expression system ("CL-PPCRE"),
one uses the ASDF-JAR:PACKAGE as follows:

    CL-USER> (asdf-jar:package :cl-ppcre)
    ;  Loading #P"/home/evenson/quicklisp/dists/quicklisp/software/cl-ppcre-2.0.3/cl-ppcre.asd" ...
    ;  Loaded #P"/home/evenson/quicklisp/dists/quicklisp/software/cl-ppcre-2.0.3/cl-ppcre.asd" (0.029 seconds)
    Packaging ASDF definition of #<ASDF:SYSTEM "cl-ppcre">
     as /var/tmp/cl-ppcre-all-2.0.3.jar.
    Packaging contents in /var/tmp/cl-ppcre-all-2.0.3.jar
     with recursive dependencies.
    #P"/var/tmp/cl-ppcre-all-2.0.3.jar"

The resulting jar contains all source and fasls required to run the
ASDF system including any transitive ASDF dependencies.  Each asdf
system is packaged under its own top level directory within the jar
archive.  The jar archive itself is numbered with the version of the
system that was specified in the packaging.

To load the system from the jar one needs to add the ASDF file
locations to the ASDF *CENTRAL-REGISTRY*.  If one wishes to load the
fasls from the jar alone, one needs to tell ASDF not to override its
output translation rules.  The function ASDF-JAR:ADD-TO-JAR does both
of these options serving as the basis for customized load strategies
tailored to end-user deployment needs.  So, after

    CL-USER> (asdf-jar:add-to-asdf "/var/tmp/cl-ppcre-all-2.0.3.jar")

a subsequent

    CL-USER> (asdf:load-system :cl-ppcre)

should load the ASDF system from the jar.

Setting CL:*LOAD-VERBOSE* will allow one to verify that the subsequent
load is indeed coming from the jar.

# Colophon

     Mark Evenson
     Created: 20-JUN-2011
     Revised: 11-JUN-2017
