/*
 * AutoconfFormatter.java
 *
 * Copyright (C) 2000-2003 Peter Graves
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
 * Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
 */

package org.armedbear.j;

public final class AutoconfFormatter extends Formatter
{
    private static final int STATE_BACKQUOTE = STATE_LAST + 1;

    // Formats.
    private static final int AUTOCONF_FORMAT_TEXT     = 0;
    private static final int AUTOCONF_FORMAT_COMMENT  = 1;
    private static final int AUTOCONF_FORMAT_STRING   = 2;
    private static final int AUTOCONF_FORMAT_KEYWORD  = 3;
    private static final int AUTOCONF_FORMAT_FUNCTION = 4;

    private static StringSet keywords;
    private static StringSet functions;

    private FastStringBuffer sb = new FastStringBuffer();
    private String token;

    public AutoconfFormatter(Buffer buffer)
    {
        this.buffer = buffer;
        if (functions == null)
            functions = new StringSet(autoconfFunctions);
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null || line.length() == 0) {
            addSegment("", AUTOCONF_FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line);
        for (int i = 0; i < segmentList.size(); i++) {
            LineSegment segment = segmentList.getSegment(i);
            if (segment.getFormat() >= 0)
                continue;
            String s = segment.getText();
            if (isKeyword(s))
                segment.setFormat(AUTOCONF_FORMAT_KEYWORD);
            else if (isFunction(s))
                segment.setFormat(AUTOCONF_FORMAT_FUNCTION);
            else
                segment.setFormat(AUTOCONF_FORMAT_TEXT);
        }
        return segmentList;
    }

    private void endToken(int state)
    {
        if (sb.length() > 0) {
            int format = -1;
            switch (state) {
                case STATE_NEUTRAL:
                    break;
                case STATE_QUOTE:
                case STATE_BACKQUOTE:
                    format = AUTOCONF_FORMAT_STRING;
                    break;
                case STATE_IDENTIFIER:
                    break;
                case STATE_COMMENT:
                    format = AUTOCONF_FORMAT_COMMENT;
                    break;
            }
            token = sb.toString();
            addSegment(token, format);
            sb.setLength(0);
        }
    }

    private void parseLine(Line line)
    {
        String text = line.getText();
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(text, buffer.getTabWidth());
        else
            text = Utilities.detab(text, buffer.getTabWidth());
        sb.setLength(0);
        int i = 0;
        int state = STATE_NEUTRAL;
        final int limit = text.length();
        // Skip whitespace at start of line.
        while (i < limit) {
            char c = text.charAt(i);
            if (Character.isWhitespace(c)) {
                sb.append(c);
                ++i;
            } else {
                endToken(state);
                break;
            }
        }
        while (i < limit) {
            char c = text.charAt(i);
            if (state == STATE_QUOTE) {
                if (c == '"') {
                    sb.append(c);
                    endToken(state);
                    state = STATE_NEUTRAL;
                } else {
                    sb.append(c);

                    if (c == '\\' && i < limit - 1)
                    {
                        // Escape char.
                        sb.append(text.charAt(++i));
                    }
                }
                ++i;
                continue;
            }
            if (state == STATE_BACKQUOTE) {
                sb.append(c);
                if (c == '`') {
                    endToken(state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            // Reaching here, we're not in a quoted string.
            if (c == '\\') {
                // Escape.
                sb.append(c);
                if (++i < limit) {
                    sb.append(text.charAt(i));
                    ++i;
                }
                continue;
            }
            if (c == '"') {
                endToken(state);
                sb.append(c);
                state = STATE_QUOTE;
                ++i;
                continue;
            }
            if (c == '`') {
                endToken(state);
                sb.append(c);
                state = STATE_BACKQUOTE;
                ++i;
                continue;
            }
            if (state == STATE_IDENTIFIER) {
                if (buffer.mode.isIdentifierPart(c))
                    sb.append(c);
                else {
                    // End of identifier.
                    endToken(state);
                    if (token.equals("dnl")) {
                        state = STATE_COMMENT;
                        sb.append(text.substring(i));
                        endToken(state);
                        return;
                    }
                    state = STATE_NEUTRAL;
                    sb.append(c);
                }
                ++i;
                continue;
            }
            if (state == STATE_NEUTRAL) {
                if (c == '#') {
                    boolean isComment = false;
                    if (i == limit-1) {
                        // '#' is last or only character.
                        isComment = true;
                    } else {
                        // Ignore "#include", "#define", etc. (AC_TRY_COMPILE)
                        char nextChar = text.charAt(i + 1);
                        if (nextChar == ' ' || nextChar == '\t' ||
                            nextChar == '#' || nextChar == '!')
                            isComment = true;
                    }
                    if (isComment) {
                        state = STATE_COMMENT;
                        sb.append(text.substring(i));
                        endToken(state);
                        return; // We're finished with this line.
                    }
                    // Not really the start of a comment.
                    sb.append(c);
                } else if (buffer.mode.isIdentifierStart(c)) {
                    endToken(state);
                    sb.append(c);
                    state = STATE_IDENTIFIER;
                } else // Still neutral...
                    sb.append(c);
            }
            ++i;
        }
        endToken(state);
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("AutoconfMode");
            formatTable.addEntryFromPrefs(AUTOCONF_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(AUTOCONF_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(AUTOCONF_FORMAT_STRING, "string" );
            formatTable.addEntryFromPrefs(AUTOCONF_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(AUTOCONF_FORMAT_FUNCTION, "function");
        }
        return formatTable;
    }

    private final boolean isFunction(String s)
    {
        if (functions == null)
            return false;
        return functions.contains(s);
    }

    private static final String[] autoconfFunctions = {
        "AC_AIX",
        "AC_ALLOCA",
        "AC_ARG_ARRAY",
        "AC_ARG_ENABLE",
        "AC_ARG_PROGRAM",
        "AC_ARG_WITH",
        "AC_BEFORE",
        "AC_C_BIGENDIAN",
        "AC_C_CHAR_UNSIGNED",
        "AC_C_CONST",
        "AC_C_CROSS",
        "AC_C_INLINE",
        "AC_C_LONG_DOUBLE",
        "AC_C_STRINGIZE",
        "AC_CACHE_CHECK",
        "AC_CACHE_LOAD",
        "AC_CACHE_SAVE",
        "AC_CACHE_VAL",
        "AC_CANONICAL_HOST",
        "AC_CANONICAL_SYSTEM",
        "AC_CHAR_UNSIGNED",
        "AC_CHECK_FILE",
        "AC_CHECK_FILES",
        "AC_CHECK_FUNC",
        "AC_CHECK_FUNCS",
        "AC_CHECK_HEADER",
        "AC_CHECK_HEADERS",
        "AC_CHECK_LIB",
        "AC_CHECK_PROG",
        "AC_CHECK_PROGS",
        "AC_CHECK_SIZEOF",
        "AC_CHECK_TOOL",
        "AC_CHECK_TYPE",
        "AC_CHECKING",
        "AC_COMPILE_CHECK",
        "AC_CONFIG_AUX_DIR",
        "AC_CONFIG_COMMANDS",
        "AC_CONFIG_FILES",
        "AC_CONFIG_HEADER",
        "AC_CONFIG_SUBDIRS",
        "AC_CONST",
        "AC_CROSS_CHECK",
        "AC_CYGWIN",
        "AC_DECL_SYS_SIGLIST",
        "AC_DECL_YYTEXT",
        "AC_DEFINE",
        "AC_DEFINE_UNQUOTED",
        "AC_DEFUN",
        "AC_DIR_HEADER",
        "AC_DYNIX_SEQ",
        "AC_EGREP_CPP",
        "AC_EGREP_HEADER",
        "AC_ENABLE",
        "AC_ERROR",
        "AC_EXEEXT",
        "AC_F77_LIBRARY_LDFLAGS",
        "AC_FIND_X",
        "AC_FIND_XTRA",
        "AC_FUNC_ALLOCA",
        "AC_FUNC_CHECK",
        "AC_FUNC_CLOSEDIR_VOID",
        "AC_FUNC_FNMATCH",
        "AC_FUNC_GETLOADAVG",
        "AC_FUNC_GETMNTENT",
        "AC_FUNC_GETPGRP",
        "AC_FUNC_MEMCMP",
        "AC_FUNC_MMAP",
        "AC_FUNC_SELECT_ARGTYPES",
        "AC_FUNC_SETPGRP",
        "AC_FUNC_SETVBUF_REVERSED",
        "AC_FUNC_STRCOLL",
        "AC_FUNC_STRFTIME",
        "AC_FUNC_UTIME_NULL",
        "AC_FUNC_VFORK",
        "AC_FUNC_VPRINTF",
        "AC_FUNC_WAIT3",
        "AC_GCC_TRADITIONAL",
        "AC_GETGROUPS_T",
        "AC_GETLOADAVG",
        "AC_HAVE_FUNCS",
        "AC_HAVE_HEADERS",
        "AC_HAVE_LIBRARY",
        "AC_HAVE_POUNDBANG",
        "AC_HEADER_CHECK",
        "AC_HEADER_DIRENT",
        "AC_HEADER_EGREP",
        "AC_HEADER_MAJOR",
        "AC_HEADER_STAT",
        "AC_HEADER_STDC",
        "AC_HEADER_SYS_WAIT",
        "AC_HEADER_TIME",
        "AC_INIT",
        "AC_INLINE",
        "AC_INT_16_BITS",
        "AC_IRIX_SUN",
        "AC_ISC_POSIX",
        "AC_LANG_C",
        "AC_LANG_CPLUSPLUS",
        "AC_LANG_FORTRAN77",
        "AC_LANG_RESTORE",
        "AC_LANG_SAVE",
        "AC_LINK_FILES",
        "AC_LN_S",
        "AC_LONG_64_BITS",
        "AC_LONG_DOUBLE",
        "AC_LONG_FILE_NAMES",
        "AC_MAJOR_HEADER",
        "AC_MEMORY_H",
        "AC_MINGW32",
        "AC_MINIX",
        "AC_MINUS_C_MINUS_O",
        "AC_MMAP",
        "AC_MODE_T",
        "AC_MSG_CHECKING",
        "AC_MSG_ERROR",
        "AC_MSG_RESULT",
        "AC_MSG_WARN",
        "AC_OBJEXT",
        "AC_OBSOLETE",
        "AC_OFF_T",
        "AC_OUTPUT",
        "AC_OUTPUT_COMMANDS",
        "AC_PATH_PROG",
        "AC_PATH_PROGS",
        "AC_PATH_X",
        "AC_PATH_XTRA",
        "AC_PID_T",
        "AC_PREFIX",
        "AC_PREFIX_PROGRAM",
        "AC_PREREQ",
        "AC_PROG_AWK",
        "AC_PROG_CC",
        "AC_PROG_CC_C_O",
        "AC_PROG_CPP",
        "AC_PROG_CXX",
        "AC_PROG_CXXCPP",
        "AC_PROG_F77_C_O",
        "AC_PROG_FORTRAN",
        "AC_PROG_GCC_TRADITIONAL",
        "AC_PROG_INSTALL",
        "AC_PROG_LEX",
        "AC_PROG_LN_S",
        "AC_PROG_MAKE_SET",
        "AC_PROG_RANLIB",
        "AC_PROG_YACC",
        "AC_PROGRAM_CHECK",
        "AC_PROGRAM_EGREP",
        "AC_PROGRAM_PATH",
        "AC_PROGRAMS_CHECK",
        "AC_PROGRAMS_PATH",
        "AC_PROVIDE",
        "AC_REMOTE_TAPE",
        "AC_REPLACE_FUNCS",
        "AC_REQUIRE",
        "AC_REQUIRE_CPP",
        "AC_RESTARTABLE_SYSCALLS",
        "AC_RETSIGTYPE",
        "AC_REVISION",
        "AC_RSH",
        "AC_SCO_INTL",
        "AC_SEARCH_LIBS",
        "AC_SET_MAKE",
        "AC_SETVBUF_REVERSED",
        "AC_SIZE_T",
        "AC_SIZEOF_TYPE",
        "AC_ST_BLKSIZE",
        "AC_ST_BLOCKS",
        "AC_ST_RDEV",
        "AC_STAT_MACROS_BROKEN",
        "AC_STDC_HEADERS",
        "AC_STRCOLL",
        "AC_STRUCT_ST_BLKSIZE",
        "AC_STRUCT_ST_BLOCKS",
        "AC_STRUCT_ST_RDEV",
        "AC_STRUCT_TIMEZONE",
        "AC_STRUCT_TM",
        "AC_SUBST",
        "AC_SUBST_FILE",
        "AC_SYS_INTERPRETER",
        "AC_SYS_LONG_FILE_NAMES",
        "AC_SYS_RESTARTABLE_SYSCALLS",
        "AC_SYS_SIGLIST_DECLARED",
        "AC_TEST_CPP",
        "AC_TEST_PROGRAM",
        "AC_TIME_WITH_SYS_TIME",
        "AC_TIMEZONE",
        "AC_TRY_COMPILE",
        "AC_TRY_CPP",
        "AC_TRY_LINK",
        "AC_TRY_LINK_FUNC",
        "AC_TRY_RUN",
        "AC_TYPE_GETGROUPS",
        "AC_TYPE_MODE_T",
        "AC_TYPE_OFF_T",
        "AC_TYPE_PID_T",
        "AC_TYPE_SIGNAL",
        "AC_TYPE_SIZE_T",
        "AC_TYPE_UID_T",
        "AC_UID_T",
        "AC_UNISTD_H",
        "AC_USG",
        "AC_UTIME_NULL",
        "AC_VALIDATE_CACHED_SYSTEM_TUPLE",
        "AC_VERBOSE",
        "AC_VFORK",
        "AC_VPRINTF",
        "AC_WAIT3",
        "AC_WARN",
        "AC_WITH",
        "AC_WORDS_BIGENDIAN",
        "AC_XENIX_DIR",
        "AC_YYTEXT_POINTER",

        // Automake.
        "AM_CONDITIONAL",
        "AM_CONFIG_HEADER",
        "AM_INIT_AUTOMAKE",
        "AM_MAINTAINER_MODE",
    };
}
