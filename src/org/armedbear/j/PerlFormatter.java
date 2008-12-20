/*
 * PerlFormatter.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;

public final class PerlFormatter extends Formatter
{
    private static final int STATE_VARIABLE         = STATE_LAST + 1;
    private static final int STATE_HERE_DOCUMENT    = STATE_LAST + 2;
    private static final int STATE_POD              = STATE_LAST + 3;
    private static final int STATE_REGEXP_DELIMITER = STATE_LAST + 4;
    private static final int STATE_REGEXP           = STATE_LAST + 5;
    private static final int STATE_SUBST            = STATE_LAST + 6;

    private static final String punctuation = "&`^:+#-%'\"/~_";

    // Formats.
    private static final int PERL_FORMAT_TEXT     = 0;
    private static final int PERL_FORMAT_COMMENT  = 1;
    private static final int PERL_FORMAT_STRING   = 2;
    private static final int PERL_FORMAT_KEYWORD  = 3;
    private static final int PERL_FORMAT_FUNCTION = 4;
    private static final int PERL_FORMAT_BRACE    = 5;
    private static final int PERL_FORMAT_NUMBER   = 6;
    private static final int PERL_FORMAT_SCALAR   = 7;
    private static final int PERL_FORMAT_LIST     = 8;

    private static StringSet functions;

    private FastStringBuffer sb = new FastStringBuffer();

    private String endOfText;

    private static RE matchRE = new UncheckedRE("(=~|!~)[ \t]+m[^a-zA-Z0-9]");

    public PerlFormatter(Buffer buffer)
    {
        this.buffer = buffer;
        if (functions == null)
            functions = new StringSet(perlFunctions);
    }

    private void endToken(int state)
    {
        if (sb.length() > 0) {
            int format = -1;
            switch (state) {
                case STATE_NEUTRAL:
                    break;
                case STATE_QUOTE:
                case STATE_SINGLEQUOTE:
                case STATE_HERE_DOCUMENT:
                case STATE_REGEXP:
                case STATE_SUBST:
                    format = PERL_FORMAT_STRING;
                    break;
                case STATE_REGEXP_DELIMITER:
                    format = PERL_FORMAT_FUNCTION;
                    break;
                case STATE_IDENTIFIER:
                    break;
                case STATE_COMMENT:
                case STATE_POD:
                    format = PERL_FORMAT_COMMENT;
                    break;
                case STATE_BRACE:
                    format = PERL_FORMAT_BRACE;
                    break;
                case STATE_NUMBER:
                case STATE_HEXNUMBER:
                    format = PERL_FORMAT_NUMBER;
                    break;
            }
            addSegment(sb.toString(), format);
            sb.setLength(0);
        }
    }

    private void parseLine(String text, int state)
    {
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(text, buffer.getTabWidth());
        else
            text = Utilities.detab(text, buffer.getTabWidth());
        clearSegmentList();
        sb.setLength(0);
        int i = 0;
        if (state == STATE_HERE_DOCUMENT) {
            if (text.startsWith(endOfText))
                state = STATE_NEUTRAL;
            else {
                sb.append(text);
                endToken(state);
                return;
            }
        }
        if (state == STATE_POD) {
            sb.append(text);
            endToken(state);
            return;
        }
        final int limit = text.length();
        char c;
        // Skip whitespace at start of line.
        while (i < limit) {
            c = text.charAt(i);
            if (Character.isWhitespace(c)) {
                sb.append(c);
                ++i;
            } else {
                endToken(state);
                break;
            }
        }
        char delimiter = 0;
        while (i < limit) {
            c = text.charAt(i);
            if (c == '\\') {
                // Escape.
                sb.append(c);
                if (i < limit-1)
                    sb.append(text.charAt(++i));
                ++i;
                continue;
            }
            if (state == STATE_QUOTE) {
                sb.append(c);
                if (c == '"') {
                    endToken(state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_SINGLEQUOTE) {
                sb.append(c);
                if (c == '\'') {
                    endToken(state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_REGEXP) {
                if (c == delimiter) {
                    endToken(state);
                    sb.append(c);
                    endToken(STATE_REGEXP_DELIMITER);
                    state = STATE_NEUTRAL;
                } else
                    sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_SUBST) {
                if (c == delimiter) {
                    endToken(state);
                    sb.append(c);
                    endToken(STATE_REGEXP_DELIMITER);
                    state = STATE_REGEXP;
                } else
                    sb.append(c);
                ++i;
                continue;
            }
            // Reaching here, we're not in a quoted string or regexp.
            if (c == '{' || c == '}') {
                endToken(state);
                sb.append(c);
                endToken(STATE_BRACE);
                state = STATE_NEUTRAL;
                ++i;
                continue;
            }
            if (state == STATE_VARIABLE) {
                boolean ok = false;
                if (PerlMode.isIdentifierChar(c))
                    ok = true;
                else if (sb.length() == 1 && punctuation.indexOf(c) >= 0)
                    ok = true;
                if (ok)
                    sb.append(c);
                else {
                    endToken(state);
                    sb.append(c);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (c == '"') {
                endToken(state);
                sb.append(c);
                state = STATE_QUOTE;
                ++i;
                continue;
            }
            if (c == '\'') {
                endToken(state);
                sb.append(c);
                state = STATE_SINGLEQUOTE;
                ++i;
                continue;
            }
            if (c == '=' || c == '!') {
                REMatch match = matchRE.getMatch(text.substring(i));
                if (match != null && match.getStartIndex() == 0) {
                    final String s = match.toString();
                    final int length = s.length();
                    // End the previous token.
                    endToken(state);
                    sb.append(s.substring(0, 2));
                    endToken(STATE_NEUTRAL);
                    i += 2;
                    sb.append(s.substring(2));
                    endToken(STATE_REGEXP_DELIMITER);
                    i += length - 2;
                    delimiter = s.charAt(length - 1);
                    if (delimiter == '{')
                        delimiter = '}';
                    state = STATE_REGEXP;
                } else {
                    sb.append(c);
                    ++i;
                }
                continue;
            }
            if (c == '/') {
                if (isSubst(text, i)) {
                    delimiter = '/';
                    sb.append(c);
                    endToken(STATE_REGEXP_DELIMITER);
                    state = STATE_SUBST;
                } else if (isRegExp(text, i)) {
                    delimiter = '/';
                    // End the previous token unless we've got "m/".
                    if (i > 0 && text.charAt(i-1) != 'm')
                        endToken(state);
                    sb.append(c);
                    endToken(STATE_REGEXP_DELIMITER);
                    state = STATE_REGEXP;
                } else {
                    // It's the division operator.
                    sb.append(c);
                }
                ++i;
                continue;
            }
            if (c == '#') {
                endToken(state);
                state = STATE_COMMENT;
                sb.append(text.substring(i));
                endToken(state);
                return;
            }
            if (state == STATE_IDENTIFIER) {
                if (PerlMode.isIdentifierChar(c))
                    sb.append(c);
                else {
                    endToken(state);
                    sb.append(c);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_NUMBER) {
                if (Character.isDigit(c))
                    sb.append(c);
                else if (sb.length() == 1 && c == 'x' || c == 'X') {
                    sb.append(c);
                    state = STATE_HEXNUMBER;
                } else {
                    endToken(state);
                    sb.append(c);
                    if (PerlMode.isIdentifierChar(c))
                        state = STATE_IDENTIFIER;
                    else
                        state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_HEXNUMBER) {
                if (Character.isDigit(c))
                    sb.append(c);
                else if ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
                    sb.append(c);
                else {
                    endToken(state);
                    sb.append(c);
                    if (PerlMode.isIdentifierChar(c))
                        state = STATE_IDENTIFIER;
                    else
                        state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_NEUTRAL) {
                if (c == '$') {
                    endToken(state);
                    sb.append(c);
                    state = STATE_VARIABLE;
                } else if (PerlMode.isIdentifierChar(c)) {
                    endToken(state);
                    sb.append(c);
                    state = STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endToken(state);
                    sb.append(c);
                    state = STATE_NUMBER;
                } else // Still neutral...
                    sb.append(c);
            }
            ++i;
        }
        endToken(state);
    }

    // i is the index of '/'.
    public static boolean isSubst(String text, int i)
    {
        Debug.assertTrue(text.charAt(i) == '/');
        if (text.regionMatches(i-2, "tr/", 0, 3)) {
            if (i < 3)
                return true;
            char c = text.charAt(i-3);
            if (PerlMode.getMode().isIdentifierPart(c))
                return false;
            else
                return true;
        }
        if (text.regionMatches(i-1, "s/", 0, 2)) {
            if (i < 2)
                return true;
            char c = text.charAt(i-2);
            if (PerlMode.getMode().isIdentifierPart(c))
                return false;
            else
                return true;
        }
        if (text.regionMatches(i-1, "y/", 0, 2)) {
            if (i < 2)
                return true;
            char c = text.charAt(i-2);
            if (PerlMode.getMode().isIdentifierPart(c))
                return false;
            else
                return true;
        }
        return false;
    }

    // Make sure the '/' at i is not the division operator.
    public static boolean isRegExp(String text, int i)
    {
        Debug.assertTrue(text.charAt(i) == '/');
        if (i == 0) {
            // It's the first character on the line.
            return true;
        }
        // Consider the previous character.
        char c = text.charAt(i-1);
        if (c == '(')
            return true;
        if (c == 'm') {
            if (i-2 < 0)
                return true;
            c = text.charAt(i-2);
            if (c == '(' || Character.isWhitespace(c))
                return true;
             return false;
        }
        // If it's an identifier character, we're not looking at a regexp,
        // since we've already tested for substitution and translation
        // patterns and "m/".
        if (PerlMode.isIdentifierChar(c))
            return false;

        if (!Character.isWhitespace(c))
            return false;

        // The immediately previous character is whitespace.
        final String s = text.substring(0, i-1).trim();
        final int length = s.length();
        if (length == 0) {
            // The '/' is the first non-whitespace character on the line.
            return true;
        }
        c = s.charAt(length-1);
        if (c == ')')
            return false; // "(a + b) / c"
        if (c == '}')
            return false;
        if (!PerlMode.isIdentifierChar(c))
            return true;

        // Last non-whitespace character is an identifier character.
        if (s.endsWith("and")) {
            if (length == 3 || Character.isWhitespace(s.charAt(length-4)))
                return true;
        } else if (s.endsWith("or")) {
            if (length == 2 || Character.isWhitespace(s.charAt(length-3)))
                return true;
        } else if (s.endsWith("not")) {
            if (length == 3 || Character.isWhitespace(s.charAt(length-4)))
                return true;
        }

        return false;
    }

    public LineSegmentList formatLine(Line line)
    {
        if (line == null) {
            clearSegmentList();
            addSegment("", PERL_FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line.getText(), line.flags());
        final int tokenCount = segmentList.size();
        for (int i = 0; i < tokenCount; i++) {
            LineSegment segment = segmentList.getSegment(i);
            if (segment.getFormat() >= 0)
                continue;
            String s = segment.getText();
            if (isKeyword(s)) {
                segment.setFormat(PERL_FORMAT_KEYWORD);
                continue;
            }
            char c = s.charAt(0);
            if (c == '$') {
                segment.setFormat(PERL_FORMAT_SCALAR);
                continue;
            }
            if (c == '%' || c == '@') {
                segment.setFormat(PERL_FORMAT_LIST);
                continue;
            }
            boolean isFunction = false;
            if (PerlMode.isIdentifierChar(c)) {
                boolean maybeFunction = true;
                final int length = s.length();
                for (int j = 1; j < length; j++) {
                    if (!PerlMode.isIdentifierChar(s.charAt(j))) {
                        maybeFunction = false;
                        break;
                    }
                }
                if (maybeFunction) {
                    if (isFunction(s))
                        isFunction = true;
                    else if (i > 1) {
                        // See if "sub" is two segments back (one segment back
                        // would be intervening whitespace).
                        LineSegment prevSegment = segmentList.getSegment(i-2);
                        if (prevSegment.getText().trim().equals("sub"))
                            isFunction = true;
                    }
                    if (!isFunction && i < segmentList.size()-1) {
                        LineSegment nextSegment = segmentList.getSegment(i+1);
                        if (nextSegment.getText().trim().startsWith("("))
                            isFunction = true;
                    }
                }
            }
            segment.setFormat(isFunction ? PERL_FORMAT_FUNCTION : PERL_FORMAT_TEXT);
        }
        return segmentList;
    }

    public boolean parseBuffer()
    {
        int state = STATE_NEUTRAL;
        Line line = buffer.getFirstLine();
        boolean changed = false;
        while (line != null) {
            int oldflags = line.flags();
            if (state == STATE_HERE_DOCUMENT) {
                if (line.getText().equals(endOfText))
                    state = STATE_NEUTRAL;
            }
            if (state == STATE_POD) {
                if (line.getText().startsWith("=cut")) {
                    if (state != oldflags) {
                        line.setFlags(state);
                        changed = true;
                    }
                    state = STATE_NEUTRAL;
                    line = line.next();
                    continue;
                }
            }
            // Assume no multiline quotes.
            if (state == STATE_QUOTE || state == STATE_SINGLEQUOTE)
                state = STATE_NEUTRAL;
            if (state == STATE_NEUTRAL)
                if (line.getText().startsWith("="))
                    state = STATE_POD;
            if (state != oldflags) {
                line.setFlags(state);
                changed = true;
            }
            if (state == STATE_HERE_DOCUMENT || state == STATE_POD) {
                line = line.next();
                continue;
            }
            final int limit = line.length();
            for (int i = 0; i < limit; i++) {
                char c = line.charAt(i);
                if (c == '\\' && i < limit-1) {
                    // Escape.
                    ++i;
                    continue;
                }
                if (state == STATE_QUOTE) {
                    if (c == '"')
                        state = STATE_NEUTRAL;
                    continue;
                }
                if (state == STATE_SINGLEQUOTE) {
                    if (c == '\'')
                        state = STATE_NEUTRAL;
                    continue;
                }
                // Not in comment or quoted string.
                if (c == '$' && i < limit-1) {
                    // In effect, another kind of escape.
                    // Next char can be quote or single quote but should be ignored.
                    ++i;
                    continue;
                }
                if (c == '<' && i < limit-2) {
                    if (line.charAt(i+1) == '<') {
                        // Line must have semicolon at end.
                        if (line.trim().endsWith(";")) {
                            endOfText = line.substring(i+2).trim();
                            int length = endOfText.length();
                            // Remove ';' at end of line.
                            if (length > 0 && endOfText.charAt(length-1) == ';')
                                endOfText = endOfText.substring(0, --length);
                            // Remove ')' if any.
                            if (length > 0 && endOfText.charAt(length-1) == ')')
                                endOfText = endOfText.substring(0, --length);
                            if (length > 2) {
                                if (endOfText.charAt(0) == '"' && endOfText.charAt(length-1) == '"')
                                    // Removed enclosing double quotes.
                                    endOfText = endOfText.substring(1, length - 1);
                                else if (endOfText.charAt(0) == '\'' && endOfText.charAt(length-1) == '\'')
                                    // Removed enclosing single quotes.
                                    endOfText = endOfText.substring(1, length - 1);
                            }
                            if (endOfText.length() > 0) {
                                // Make sure "<<" is not shift operator.
                                if (Character.isLetter(endOfText.charAt(0))) {
                                    state = STATE_HERE_DOCUMENT;
                                    break;
                                }
                            }
                        }
                    }
                    continue;
                }
                if (c == '#')
                    // Single-line comment beginning. Ignore rest of line.
                    break;
                else if (c == '"')
                    state = STATE_QUOTE;
                else if (c == '\'')
                    state = STATE_SINGLEQUOTE;
            }
            line = line.next();
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    private static final String[] perlFunctions =
    {
        "abs",
        "accept",
        "alarm",
        "atan2",
        "bind",
        "binmode",
        "bless",
        "caller",
        "chdir",
        "chmod",
        "chomp",
        "chop",
        "chown",
        "chr",
        "chroot",
        "close",
        "closedir",
        "connect",
        "cos",
        "crypt",
        "dbmclose",
        "dbmopen",
        "defined",
        "delete",
        "die",
        "dump",
        "each",
        "eof",
        "eval",
        "exec",
        "exists",
        "exit",
        "exp",
        "fcntl",
        "fileno",
        "flock",
        "fork",
        "format",
        "formline",
        "getc",
        "getgrent",
        "getgrgid",
        "getgrnam",
        "gethostbyaddr",
        "gethostbyname",
        "gethostent",
        "getlogin",
        "getnetbyaddr",
        "getnetbyname",
        "getnetent",
        "getpeername",
        "getpgrp",
        "getppid",
        "getpriority",
        "getprotobyname",
        "getprotobynumber",
        "getprotoent",
        "getpwent",
        "getpwnam",
        "getpwuid",
        "getservbyname",
        "getservbyport",
        "getservent",
        "getsockname",
        "getsockopt",
        "glob",
        "gmtime",
        "grep",
        "hex",
        "import",
        "index",
        "int",
        "ioctl",
        "join",
        "keys",
        "kill",
        "lc",
        "lcfirst",
        "length",
        "link",
        "listen",
        "localtime",
        "log",
        "lstat",
        "map",
        "mkdir",
        "msgctl",
        "msgget",
        "msgrcv",
        "msgsnd",
        "oct",
        "open",
        "opendir",
        "ord",
        "pack",
        "pipe",
        "pop",
        "pos",
        "print",
        "printf",
        "push",
        "quotemeta",
        "rand",
        "read",
        "readdir",
        "readlink",
        "recv",
        "rename",
        "reset",
        "reverse",
        "rewinddir",
        "rindex",
        "rmdir",
        "scalar",
        "seek",
        "seekdir",
        "select",
        "semctl",
        "semget",
        "semop",
        "send",
        "setpgrp",
        "setpriority",
        "setsockopt",
        "shift",
        "shmctl",
        "shmget",
        "shmread",
        "shmwrite",
        "shutdown",
        "sin",
        "sleep",
        "socket",
        "socketpair",
        "sort",
        "splice",
        "split",
        "sprintf",
        "sqrt",
        "srand",
        "stat",
        "study",
        "substr",
        "symlink",
        "syscall",
        "sysopen",
        "sysread",
        "system",
        "syswrite",
        "tell",
        "telldir",
        "time",
        "times",
        "truncate",
        "uc",
        "ucfirst",
        "umask",
        "unlink",
        "unpack",
        "unshift",
        "utime",
        "values",
        "vec",
        "wait",
        "waitpid",
        "wantarray",
        "warn",
        "write"
    };

    private final boolean isFunction(String s)
    {
        if (functions == null)
            return false;
        return functions.contains(s);
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("PerlMode");
            formatTable.addEntryFromPrefs(PERL_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(PERL_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(PERL_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(PERL_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(PERL_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(PERL_FORMAT_BRACE, "brace");
            formatTable.addEntryFromPrefs(PERL_FORMAT_NUMBER, "number");
            formatTable.addEntryFromPrefs(PERL_FORMAT_SCALAR, "scalar");
            formatTable.addEntryFromPrefs(PERL_FORMAT_LIST, "list");
        }
        return formatTable;
    }
}
