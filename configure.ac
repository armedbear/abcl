dnl  Process this file with autoconf to produce a configure script.
dnl
dnl  Copyright (C) 1998-2004 Peter Graves
dnl  $Id: configure.ac,v 1.77 2005-12-19 15:57:07 piso Exp $
dnl
dnl  This program is free software; you can redistribute it and/or
dnl  modify it under the terms of the GNU General Public License
dnl  as published by the Free Software Foundation; either version 2
dnl  of the License, or (at your option) any later version.
dnl
dnl  This program is distributed in the hope that it will be useful,
dnl  but WITHOUT ANY WARRANTY; without even the implied warranty of
dnl  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
dnl  GNU General Public License for more details.
dnl
dnl  You should have received a copy of the GNU General Public License
dnl  along with this program; if not, write to the Free Software
dnl  Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.

AC_REVISION($Revision: 1.77 $)

BOOTFILE=./configure-options

if test -f "${BOOTFILE}" && test -z "${IN_BOOT}" ; then
    echo "reading options from ${BOOTFILE}"
    IN_BOOT=1 $0 `cat ${BOOTFILE}` $*
    exit $?
fi

AC_INIT(j.in)

AC_PROG_INSTALL

version=0.21.0.5+
AC_SUBST(version)

if test $OSTYPE = cygwin32 || test $OSTYPE = cygwin ; then
    CYGWIN=yes
    PATHSEP=";"
else
    PATHSEP=":"
fi
AC_SUBST(PATHSEP)


dnl  Make srcdir absolute.

case "${srcdir}" in
    /*)
        ;;
    .)
        unset CDPATH
        if test "${PWD}" != "" && test "`(cd ${PWD}; sh -c pwd)`" = "`pwd`"; then
            srcdir="$PWD"
        else
            srcdir="`(cd ${srcdir}; pwd)`"
        fi
        ;;
    *)
        srcdir="`(cd ${srcdir}; pwd)`"
        ;;
esac


dnl  User -must- specify location of JDK.
AC_ARG_WITH(jdk,
[  --with-jdk=DIR          JDK you want to use ],
[
    JDK=${with_jdk}
],
[
    AC_MSG_ERROR([no jdk specified; use --with-jdk])
])

dnl  Remove trailing '/' (if any).
JDK=`echo $JDK | sed -e 's/\/$//'`

if test -x $JDK/bin/java; then
    JAVA=$JDK/bin/java;
elif test -x $JDK/jre/bin/java; then
    JAVA=$JDK/jre/bin/java;
elif test -x $JDK/Home/bin/java; then
    JAVA=$JDK/Home/bin/java;
else
    AC_MSG_ERROR(Java not found)
fi
AC_SUBST(JDK)
AC_SUBST(JAVA)


dnl  Java version.

AC_MSG_CHECKING(Java version)
JAVA_VERSION=`$JAVA -version 2>&1`

case "$JAVA_VERSION" in
    java*1.5*)
        AC_MSG_RESULT($JAVA_VERSION)
        ;;
    java*1.4*)
        AC_MSG_RESULT($JAVA_VERSION)
        ;;
    *)
        AC_MSG_ERROR(Java 1.4 or later is required)
        ;;
esac


dnl  jikes/javac

test -z "$JAVAC" && test -x $JDK/Home/bin/javac && JAVAC=$JDK/Home/bin/javac
test -z "$JAVAC" && JAVAC=$JDK/bin/javac
AC_SUBST(JAVAC)
test "$JAVAC" = jikes && JAVACFLAGS="+D +F"
AC_SUBST(JAVACFLAGS)

dnl javah

JAVAH=$JDK/bin/javah
AC_SUBST(JAVAH)


dnl  fastjar/jar

test -z "$JAR" && AC_CHECK_PROGS(JAR, fastjar)
test -z "$JAR" && test -x $JDK/Home/bin/jar && JAR=$JDK/Home/bin/jar
test -z "$JAR" && JAR=$JDK/bin/jar
AC_SUBST(JAR)


dnl  Location of Lisp source files.
LISP_HOME=$srcdir/src/org/armedbear/lisp
AC_SUBST(LISP_HOME)


dnl  Make prefix explicit.
test "x$prefix" = xNONE && prefix=/usr/local

dnl  Make prefix explicit.
test "x$exec_prefix" = xNONE && exec_prefix="${prefix}"

dnl  Make datadir and libdir explicit.
eval "datadir=$datadir"
eval "libdir=$libdir"

dnl Library path for libabcl.so.
ABCL_LIBPATH=$LISP_HOME:$libdir/abcl
AC_SUBST(ABCL_LIBPATH)

dnl  Java runtime classes and options
if test -f $JDK/jre/lib/rt.jar ; then
    # SUN
    JAVA_RUNTIME=$JDK/jre/lib/rt.jar
    ABCL_JAVA_OPTIONS="-server -Xmx256M"
elif test -f $JDK/jre/lib/core.jar ; then
    # IBM
    JAVA_RUNTIME=""
    for i in $JDK/jre/lib/*.jar ; do JAVA_RUNTIME=$JAVA_RUNTIME:$i ; done
    ABCL_JAVA_OPTIONS="-Xss512K -Xmx256M"
elif test -d $JDK/Classes ; then
    # Mac OS X
    JAVA_RUNTIME=""
    for i in $JDK/Classes/*.jar ; do JAVA_RUNTIME=$JAVA_RUNTIME:$i ; done
    ABCL_JAVA_OPTIONS="-server -Xmx256M"
else
    AC_MSG_ERROR(unable to find Java runtime)
fi
if test -f $JDK/lib/tools.jar ; then
    JAVA_RUNTIME=$JAVA_RUNTIME:$JDK/lib/tools.jar
fi
AC_SUBST(JAVA_RUNTIME)
AC_SUBST(ABCL_JAVA_OPTIONS)


dnl  Construct command line for shell script.

dnl  Places to look for j.
JPATH=$srcdir/src:$datadir/j/j.jar

CP="-cp"
CLASSPATH="$JPATH:$JDK/lib/tools.jar"
ABCL_CLASSPATH=$JPATH
AC_SUBST(CP)
AC_SUBST(CLASSPATH)
AC_SUBST(ABCL_CLASSPATH)

COMPILER_CLASSPATH="$srcdir/src:$JAVA_RUNTIME"
AC_SUBST(COMPILER_CLASSPATH)

dnl  Runtime options to be passed to java
AC_ARG_WITH(options,
[  --with-options=OPTIONS  runtime options to be passed to java (e.g. -server) ],
[
    JAVA_OPTIONS=${with_options}
])
AC_SUBST(JAVA_OPTIONS)

dnl  Extensions (JIMI etc.)
AC_ARG_WITH(extensions,
[  --with-extensions=PATH  extensions to CLASSPATH (JIMI, Xerces, etc.) ],
[
    EXTENSIONS=${with_extensions}
])

if test $EXTENSIONS; then
    CLASSPATH="$CLASSPATH:$EXTENSIONS"
    COMPILER_CLASSPATH="$COMPILER_CLASSPATH:$EXTENSIONS"
fi

if test $CYGWIN; then
    JAVA_RUNTIME=`cygpath -p -w $JAVA_RUNTIME`
    CLASSPATH=`cygpath -p -w $CLASSPATH`
    COMPILER_CLASSPATH=`cygpath -p -w $COMPILER_CLASSPATH`
fi

dnl jpty

AC_ARG_ENABLE(jpty,
[  --enable-jpty           build and install jpty
  --disable-jpty          do not build or install jpty (default)])

if test "$enable_jpty" = "yes"; then
    if test $CYGWIN; then
        JPTY=jpty.exe
    else
        JPTY=jpty
    fi
fi

AC_SUBST(JPTY)

dnl libabcl

AC_ARG_ENABLE(libabcl,
[  --enable-libabcl        build and install libabcl.so
  --disable-libabcl       do not build or install libabcl.so (default)])

if test "$enable_libabcl" = "yes"; then
    LIBABCL=libabcl.so
fi

AC_SUBST(LIBABCL)

dnl debug

AC_ARG_ENABLE(debug,
[  --enable-debug          enable run-time debugging
  --disable-debug         disable run-time debugging (default)])

if test "$enable_debug" = "yes"; then
    JAVACFLAGS="$JAVACFLAGS -g"
else
    JAVACFLAGS="$JAVACFLAGS -g:none"
fi


AC_CONFIG_FILES([
Makefile
src/Makefile
src/jpty/Makefile
src/gnu/regexp/Makefile
src/org/armedbear/j/Makefile
src/org/armedbear/j/version
src/org/armedbear/j/mail/Makefile
src/org/armedbear/j/jdb/Makefile
src/org/armedbear/lisp/Makefile
src/org/armedbear/lisp/java/awt/Makefile
j
abcl
])

AC_CONFIG_COMMANDS([chmod], [chmod 755 j; chmod 755 abcl])

AC_OUTPUT
