/*
 * Keyword.java
 *
 * Copyright (C) 2002-2007 Peter Graves
 * $Id$
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

import static org.armedbear.lisp.Lisp.*;

public final class Keyword
{
    public static final Symbol
        ABCL                = internKeyword("ABCL"),
        ABORT               = internKeyword("ABORT"),
        ABSOLUTE            = internKeyword("ABSOLUTE"),
        ADJUSTABLE          = internKeyword("ADJUSTABLE"),
        ALLOW_OTHER_KEYS    = internKeyword("ALLOW-OTHER-KEYS"),
        ANSI_CL             = internKeyword("ANSI-CL"),
        APPEND              = internKeyword("APPEND"),
        ARMEDBEAR           = internKeyword("ARMEDBEAR"),
        BACK                = internKeyword("BACK"),
        BOOLEAN             = internKeyword("BOOLEAN"),
        CAPITALIZE          = internKeyword("CAPITALIZE"),
        CAPITALIZE_FIRST    = internKeyword("CAPITALIZE-FIRST"),
        CASE                = internKeyword("CASE"),
        CAUSE               = internKeyword("CAUSE"),
        CHAR                = internKeyword("CHAR"),
        COMMON              = internKeyword("COMMON"),
        COMMON_LISP         = internKeyword("COMMON-LISP"),
        COMPILE_TOPLEVEL    = internKeyword("COMPILE-TOPLEVEL"),
        COUNT_ONLY          = internKeyword("COUNT-ONLY"),
        CREATE              = internKeyword("CREATE"),
        DARWIN              = internKeyword("DARWIN"),
        DATUM               = internKeyword("DATUM"),
        DECLARED            = internKeyword("DECLARED"),
        DEFAULT             = internKeyword("DEFAULT"),
        DEFAULTS            = internKeyword("DEFAULTS"),
        DEVICE              = internKeyword("DEVICE"),
        DIRECTION           = internKeyword("DIRECTION"),
        DIRECTORY           = internKeyword("DIRECTORY"),
        DIRECT_SUPERCLASSES = internKeyword("DIRECT-SUPERCLASSES"),
        DOWNCASE            = internKeyword("DOWNCASE"),
        ELEMENT_TYPE        = internKeyword("ELEMENT-TYPE"),
        END                 = internKeyword("END"),
        ERROR               = internKeyword("ERROR"),
        EXECUTE             = internKeyword("EXECUTE"),
        EXPECTED_TYPE       = internKeyword("EXPECTED-TYPE"),
        EXTERNAL            = internKeyword("EXTERNAL"),
        EXTERNAL_FORMAT     = internKeyword("EXTERNAL-FORMAT"),
        FILL_POINTER        = internKeyword("FILL-POINTER"),
        FORMAT_ARGUMENTS    = internKeyword("FORMAT-ARGUMENTS"),
        FORMAT_CONTROL      = internKeyword("FORMAT-CONTROL"),
        FROM_END            = internKeyword("FROM-END"),
        FREEBSD             = internKeyword("FREEBSD"),
        HOST                = internKeyword("HOST"),
        IF_DOES_NOT_EXIST   = internKeyword("IF-DOES-NOT-EXIST"),
        IF_EXISTS           = internKeyword("IF-EXISTS"),
        INHERITED           = internKeyword("INHERITED"),
        INITIAL_CONTENTS    = internKeyword("INITIAL-CONTENTS"),
        INITIAL_ELEMENT     = internKeyword("INITIAL-ELEMENT"),
        INPUT               = internKeyword("INPUT"),
        INSTANCE            = internKeyword("INSTANCE"),
        INT                 = internKeyword("INT"),
        INTERNAL            = internKeyword("INTERNAL"),
        INVERT              = internKeyword("INVERT"),
        IO                  = internKeyword("IO"),
        J                   = internKeyword("J"),
        JAVA_1_4            = internKeyword("JAVA-1.4"),
        JAVA_1_5            = internKeyword("JAVA-1.5"),
        JAVA_1_6            = internKeyword("JAVA-1.6"),
        JAVA_1_7            = internKeyword("JAVA-1.7"),
        KEY                 = internKeyword("KEY"),
        KEY_AND_VALUE       = internKeyword("KEY-AND-VALUE"),
        KEY_OR_VALUE        = internKeyword("KEY-OR-VALUE"),
        LINUX               = internKeyword("LINUX"),
        LOAD_TOPLEVEL       = internKeyword("LOAD-TOPLEVEL"),
        LOCAL               = internKeyword("LOCAL"),
        LONG                = internKeyword("LONG"),
        MOP                 = internKeyword("MOP"),
        NAME                = internKeyword("NAME"),
        NETBSD              = internKeyword("NETBSD"),
        NEW_VERSION         = internKeyword("NEW"),
        NEWEST              = internKeyword("NEWEST"),
        NICKNAMES           = internKeyword("NICKNAMES"),
        NONE                = internKeyword("NONE"),
        NO_ERROR            = internKeyword("NO-ERROR"),
        OBJECT              = internKeyword("OBJECT"),
        OPENBSD             = internKeyword("OPENBSD"),
        OPERANDS            = internKeyword("OPERANDS"),
        OPERATION           = internKeyword("OPERATION"),
        OUTPUT              = internKeyword("OUTPUT"),
        OVERFLOW            = internKeyword("OVERFLOW"),
        OVERWRITE           = internKeyword("OVERWRITE"),
        PACKAGE             = internKeyword("PACKAGE"),
        PATHNAME            = internKeyword("PATHNAME"),
        PROBE               = internKeyword("PROBE"),
        PREFIX              = internKeyword("PREFIX"), // EXT:MAKE-TEMP-FILE
        PUBLIC              = internKeyword("PUBLIC"),
        PRESERVE            = internKeyword("PRESERVE"),
        REF                 = internKeyword("REF"),
        RELATIVE            = internKeyword("RELATIVE"),
        RENAME              = internKeyword("RENAME"),
        RENAME_AND_DELETE   = internKeyword("RENAME-AND-DELETE"),
        SIZE                = internKeyword("SIZE"),
        SOLARIS             = internKeyword("SOLARIS"),
        START               = internKeyword("START"),
        STATUS              = internKeyword("STATUS"),
        STREAM              = internKeyword("STREAM"),
        SUNOS               = internKeyword("SUNOS"),
        SUFFIX              = internKeyword("SUFFIX"),  // EXT:MAKE-TEMP-FILE
        SUPERSEDE           = internKeyword("SUPERSEDE"),
        TEST                = internKeyword("TEST"),
        TEST_NOT            = internKeyword("TEST-NOT"),
        TIME                = internKeyword("TIME"),
        TOP_LEVEL           = internKeyword("TOP-LEVEL"),
        TRAPS               = internKeyword("TRAPS"),
        TYPE                = internKeyword("TYPE"),
        UNDERFLOW           = internKeyword("UNDERFLOW"),
        UNIX                = internKeyword("UNIX"),
        UNSPECIFIC          = internKeyword("UNSPECIFIC"),
        UP                  = internKeyword("UP"),
        UPCASE              = internKeyword("UPCASE"),
        USE                 = internKeyword("USE"),
        VALUE               = internKeyword("VALUE"),
        VERSION             = internKeyword("VERSION"),
        WILD                = internKeyword("WILD"),
        WILD_ERROR_P        = internKeyword("WILD-ERROR-P"),
        WILD_INFERIORS      = internKeyword("WILD-INFERIORS"),
        WINDOWS             = internKeyword("WINDOWS"),
        X86                 = internKeyword("X86"),
        X86_64              = internKeyword("X86-64"),
        CDR6                = internKeyword("CDR6");
}
