Armed Bear Common Lisp (ABCL) README
====================================

Armed Bear Common Lisp is a conforming implementation of ANSI X3J13
Common Lisp that runs in a Java virtual machine.  It compiles Lisp
code directly to Java byte code for execution.


LICENSE
-------

Armed Bear Common Lisp is distributed under the GNU General Public
License with a classpath exception (see "Classpath Exception" below).

A copy of GNU General Public License (GPLv2) is included in this
distribution, in <file:COPYING>.

We have modified our GPLv2 license section 13 to read:

     13. Linking this library statically or dynamically with other
     modules is making a combined work based on this library. Thus, the
     terms and conditions of the GNU General Public License cover the
     whole combination.

     The following paragraph details the "classpath exception" which ABCL
     allows as an exception to the statement about linking libraries.

     As a special exception, the copyright holders of this software give
     you permission to link this software with independent modules to
     produce an executable, regardless of the license terms of these
     independent modules, and to copy and distribute the resulting
     executable under terms of your choice, provided that you also meet,
     for each linked independent module, the terms and conditions of the
     license of that module. An independent module is a module which is
     not derived from or based on this software. If you modify this
     software, you may extend this exception to your version of the
     software, but you are not obligated to do so. If you do not wish to
     do so, delete this exception statement from your version.

CONTAINERIZATION
----------------

We recommend using podman over docker for political reasons, but the
surface syntax is identical so if you must, just subsitute `docker`
for `podman` in the following examples.

With [podman][] installed, one may execute:

    podman build -t YOURID/abcl .
    podman run -it YOURID/abcl 

to get something like

    illin:~/work/abcl$ podman run -it YOURID/abcl
    VM settings:
    Max. Heap Size (Estimated): 498.00M
    Using VM: OpenJDK 64-Bit Server VM

    Armed Bear Common Lisp 1.9.0
    Java 17.0.2 Oracle Corporation
    OpenJDK 64-Bit Server VM
    Low-level initialization completed in 0.952 seconds.
    Startup completed in 4.248 seconds.
    Type ":help" for a list of available commands.
    CL-USER(1):

To install Quicklisp for ABCL in the Docker container run:

    podman run -t YOURID/abcl abcl \
      --batch --load /home/abcl/work/abcl/ci/install-quicklisp.lisp

See <file:Dockerfile> for the build instructions.

[podman]: https://podman.io/releases/2022/02/22/podman-release-v4.0.0.html
[Docker Engine]: https://www.docker.com/products/docker-engine


RUNNING FROM BINARY RELEASE
---------------------------

After you have downloaded a binary release from either [the
distributed Maven POM graph][maven-abcl] or from
[abcl.org][abcl.org-release] archive unpack it into its own
directory. To run ABCL directly from this directory, make sure the
Java executable (`java`) (Java 6, 7, 8, 11, 13, 14, 15, 16, 17, and 18
are supported by ABCL 1.9.0) is in your shell's path.

[maven-abcl]:          <https://mvnrepository.com/artifact/org.abcl/abcl/1.9.0>
[maven-abcl-contrib]:  <https://mvnrepository.com/artifact/org.abcl/abcl-contrib/1.9.0>
[abcl.org-release]:    <http://abcl.org/releases/1.9.0/>

To start ABCL, simply issue the following command:

    cmd$ java -jar abcl.jar

which should result in output like the following

    Armed Bear Common Lisp 1.9.0
    Java 17.0.2 OpenJDK Porters Group
    OpenJDK 64-Bit Server VM
    Low-level initialization completed in 0.107 seconds.
    Startup completed in 0.493 seconds.
    CL-USER(1):

Yer now at the interactive ABCL "Read Eval Print Loop" (REPL): hacks
'n glory await.

See the section headed "SLIME" for instructions to connect to this
repl from Emacs.


BUILDING FROM SOURCE RELEASE
----------------------------

ABCL may be built from its source code by executing the build
instructions <file:build.xml> expressed by the venerable Apache Ant
tool.

To build, one must have a Java 6, 7, 8, 11, 13, 14, 15, 16 or 17 Java
Development Kit (openjdk) installed locally. Just the Java Runtime
Environment (JRE) isn't enough, as you need the Java compiler
('javac') to compile the Java source of the ABCL implementation.

Download a binary distribution [Ant version 1.7.1 or greater][ant].
Unpack the files somewhere convenient, ensuring that the 'ant' (or
'ant.bat' under Windows) executable is in your path and executable.

[ant]: http://ant.apache.org/bindownload.cgi

Then simply executing 

    cmd$ ant

from the directory containing the <file:build.xml> instructions will
create an executable wrapper ('abcl' under UNIX, 'abcl.bat' under
Windows).  Use this wrapper to start ABCL.

The build may be customized by copying <file:abcl.properties.in> to
<file:abcl.properties>, which will cause Ant to attempt to build
incrementally as well as optimizing the runtime for a contemporary
64bit desktop/server machine running Java 8, 11, and/or 17.  The file
contains incomplete documentation on how it may be edited for
subsequent customization.  As an alternative to copying the prototype,
if one has a version of bash locally, one may issue via Ant

    ant abcl.properties.autoconfigure.openjdk.17

or from the shell as

    bash ci/create-abcl-properties.bash openjdk17

Currently supported platforms are 'openjdk6', 'openjdk7', 'openjdk8',
'openjdk11', 'openjdk13', 'openjdk14', 'openjdk15', 'openjd16', and
'openjdk17'.


USING APACHE NETBEANS
---------------------

Alternatively, one may install the [Netbeans visual integrated
development environment][netbeans], which contains both the Java
Development Kit as well as the Ant build tool.  The source
distribution contains Netbeans-specific project artifacts under
<file:nbproject> for loading ABCL as a Netbeans project.

With Netbeans, one should be able to open the ABCL directory as a
project whereupon the usual build, run, and debug targets as invoked
in the GUI are available.  Use the 'slime' config with a suitably
linked SLIME `swank.asd` ASDF configuration
<file:~/.config/common-lisp/source-registry.conf.d/> to connect a REPL
to the NetBeans debug process.

[netbeans]: http://netbeans.org/downloads/


SLIME
-----

For usage of ABCL with the [Superior Lisp Interaction Mode for
Emacs][slime], one may easily start a Swank listener via:

    (require :asdf)
    (require :abcl-contrib)
    (asdf:load-system :quicklisp-abcl)
    (or
       (asdf:make :swank)
       (ql:quickload :swank))
    (swank:create-server :dont-close t)

[slime]: https://common-lisp.net/project/slime/



BUGS
----

Armed Bear Common Lisp strives to be a conforming ANSI X3J13 Common
Lisp implementation.  Any other behavior should be reported as a bug.

ABCL has a [User Manual][manual] stating its conformance to the ANSI
standard, providing a compliant and practical Common Lisp
implementation.

[manual]: https://abcl.org/releases/1.9.0/abcl-1.9.0.pdf


TESTS
-----

    | Version | Failures | Total |
    |---------+----------+-------|
    |   1.9.0 |       61 | 21870 |
    |   1.8.0 |       49 | 21848 |
    |   1.5.0 |       48 | 21708 |

ABCL 1.9.0 currently fails ~49(!!) out of 21848(!!) the current ANSI
test suite derived from the tests originally written for GCL.

[ansi-test]: git+https://gitlab.common-lisp.net/ansi-test/ansi-test.git

Maxima's test suite runs without failures.

ABCL comes with a test suite.  Consult the output of `ant help.test`
for more information.


SUPPORT
-------

ABCL has many deficiencies, both known and unknown.  Descriptions,
tests, and even patches to address them will be gladly accepted.

Please report problems to the [development mailing list][mailing-list]
or via opening an issue on either the [ABCL trac instance][trac] or
[github][].

[mailing-list]: https://mailman.common-lisp.net/pipermail/armedbear-devel/
[github]: https://github.com/armedbear/abcl/issues
[trac]: https://abcl.org/trac/


AUTHORS
-------

On behalf of all ABCL development team and contributors,

    Mark Evenson
    Erik Hülsmann
    Rudolf Schlatte
    Alessio Stalla
    Ville Voutilainen

    alan
    dmiles
    Dmitry Nadezhin
    olof ferada
    pipping
    slyrus
    vibhu

    Jonathan Cunningham
    Uthar
    alejandrozf
    phoe
    jackdaniel
    Robert Munyer
    Eric Timmons (daewok)
    contrapunctus
    Scott Burson
    Samuel Hunter
    Phil Eaton

    András Simon
    Peter Graves

Have fun!

May 2022
