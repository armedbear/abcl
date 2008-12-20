/*
 * LispFormatter.java
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

public final class LispFormatter extends Formatter
{
    // States.
    private static final int STATE_OPEN_PAREN              = STATE_LAST + 1;
    private static final int STATE_CLOSE_PAREN             = STATE_LAST + 2;
    private static final int STATE_CAR                     = STATE_LAST + 3;
    private static final int STATE_DEFUN                   = STATE_LAST + 4;
    private static final int STATE_DEFINITION              = STATE_LAST + 5;
    private static final int STATE_NAME                    = STATE_LAST + 6;
    private static final int STATE_SUBSTITUTION            = STATE_LAST + 7;
    private static final int STATE_SECONDARY_KEYWORD       = STATE_LAST + 8;
    private static final int STATE_PUNCTUATION             = STATE_LAST + 9;
    private static final int STATE_ARGLIST                 = STATE_LAST + 10;
    private static final int STATE_QUOTED_LIST             = STATE_LAST + 11;

    // Formats.
    private static final int LISP_FORMAT_TEXT              = 0;
    private static final int LISP_FORMAT_COMMENT           = 1;
    private static final int LISP_FORMAT_STRING            = 2;
    private static final int LISP_FORMAT_KEYWORD           = 3;
    private static final int LISP_FORMAT_DEFUN             = 4;
    private static final int LISP_FORMAT_NAME              = 5;
    private static final int LISP_FORMAT_PARENTHESIS       = 6;
    private static final int LISP_FORMAT_PUNCTUATION       = 7;
    private static final int LISP_FORMAT_SUBSTITUTION      = 8;
    private static final int LISP_FORMAT_SECONDARY_KEYWORD = 9;

    private static final RE condRE =
        new UncheckedRE("\\([ \t]*cond[ \t]*\\(\\(");

    private static final RE dolistRE =
        new UncheckedRE("\\([ \t]*dolist[ \t]*\\(");

    // Matches e.g. "(do () ((endp list1))".
    private static final RE doRE =
        new UncheckedRE("\\([ \t]*do\\*?[ \t]*\\(.*\\)[ \t]\\(\\(");

    private static final RE letOrDoRE =
        new UncheckedRE("\\([ \t]*(let|do)\\*?[ \t]*\\(\\(");

    private final Mode mode;

    public LispFormatter(Buffer buffer)
    {
        this.buffer = buffer;
        this.mode = buffer.getMode();
    }

    private Line currentLine;
    private int tokenBegin = 0;

    private void endToken(String text, int tokenEnd, int state)
    {
        if (tokenEnd - tokenBegin > 0) {
            int format = LISP_FORMAT_TEXT;
            switch (state) {
                case STATE_NEUTRAL:
                case STATE_ARGLIST:
                case STATE_QUOTED_LIST:
                    break;
                case STATE_QUOTE:
                    format = LISP_FORMAT_STRING;
                    break;
                case STATE_OPEN_PAREN:
                case STATE_CLOSE_PAREN:
                    format = LISP_FORMAT_PARENTHESIS;
                    break;
                case STATE_CAR:
                    break;
                case STATE_DEFUN: {
                    String token = text.substring(tokenBegin, tokenEnd).trim();
                    if (isKeyword(token)) {
                        if (isDefiner(token))
                            format = LISP_FORMAT_DEFUN;
                        else
                            format = LISP_FORMAT_KEYWORD;
                    }
                    break;
                }
                case STATE_NAME:
                    format = LISP_FORMAT_NAME;
                    break;
                case STATE_DEFINITION:
                case STATE_IDENTIFIER:
                    break;
                case STATE_SECONDARY_KEYWORD:
                    format = LISP_FORMAT_SECONDARY_KEYWORD;
                    break;
                case STATE_SUBSTITUTION:
                    format = LISP_FORMAT_SUBSTITUTION;
                    break;
                case STATE_COMMENT:
                    format = LISP_FORMAT_COMMENT;
                    break;
                case STATE_PUNCTUATION:
                    format = LISP_FORMAT_PUNCTUATION;
            }
            addSegment(text, tokenBegin, tokenEnd, format);
            tokenBegin = tokenEnd;
        }
    }

    private static final boolean isDefiner(String s)
    {
        if (s.length() >= 5 && s.startsWith("def")) {
            String translated = LispMode.translateDefiner(s);
            if (translated != null) {
                // Exclude DEFCONSTANT, DEFPARAMETER, DEFVAR.
                if (translated.equals("defconstant"))
                    return false;
                if (translated.equals("defparameter"))
                    return false;
                if (translated.equals("defvar"))
                    return false;
                return true;
            }
        }
        return false;
    }

    // Returns true if token at specified offset in detabbed text from line is
    // in functional position, based on context.
    private static final boolean isPositionFunctional(
        final String text,      // Detabbed text.
        final int offset,       // Offset of token in detabbed text.
        final Line line)        // Line (which may contain tab characters).
    {
        if (offset >= 1 && text.charAt(offset - 1) == '(') {
            if (offset >= 2 && text.charAt(offset - 2) == '(') {
                // Token is preceded by "((".
                if (countLeadingSpaces(text) == offset - 2) {
                    // First non-whitespace text on line.
                    Position pos =
                        LispMode.findContainingSexp(new Position(line, 0));
                    if (pos != null) {
                        // Skip '('.
                        pos.skip();
                        String s = parseToken(pos).toLowerCase();
                        if (s.equals("cond"))
                            return true;
                        // Check for end-test form after DO/DO*.
                        if (s.equals("do") || s.equals("do*"))
                            return true;
                    }
                }
                REMatch m = condRE.getMatch(text);
                if (m != null && m.getEndIndex() == offset) {
                    return true;
                }
                m = doRE.getMatch(text);
                if (m != null && m.getEndIndex() == offset)
                    return true;
                return false;
            }
            // Text is preceded by single '('.
            if (countLeadingSpaces(text) == offset - 1) {
                // First non-whitespace on line.
                Position pos =
                    LispMode.findContainingSexp(new Position(line, 0));
                if (pos != null) {
                    if (pos.lookingAt("((")) {
                        REMatch m =
                            letOrDoRE.getMatch(pos.getLine().getText());
                        if (m != null && m.getEndIndex() == pos.getOffset() + 2)
                            return false;
                    } else {
                        // Skip '('.
                        pos.skip();
                        String s = parseToken(pos).toLowerCase();
                        if (s.equals("case"))
                            return false;
                        if (s.equals("ccase"))
                            return false;
                        if (s.equals("ecase"))
                            return false;
                        if (s.equals("typecase"))
                            return false;
                        if (s.equals("ctypecase"))
                            return false;
                        if (s.equals("etypecase"))
                            return false;
                    }
                }
            } else {
                // Not first non-whitespace on line.
                if (text.startsWith("(defknown "))
                    return false;
                REMatch m = dolistRE.getMatch(text);
                if (m != null && m.getEndIndex() == offset)
                    return false;
            }
        }
        return true;
    }

    // Returns next whitespace-delimited token starting at (or after) pos.
    // Same line only. Never returns null.
    private static final String parseToken(Position pos)
    {
        final Line line = pos.getLine();
        final int limit = line.length();
        int begin = pos.getOffset();
        while (begin < limit && Character.isWhitespace(line.charAt(begin)))
            ++begin;
        if (begin == limit)
            return "";
        int end = begin + 1;
        while (end < limit && !Character.isWhitespace(line.charAt(end)))
            ++end;
        return line.getText().substring(begin, end);
    }

    private static final int countLeadingSpaces(String s)
    {
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            if (s.charAt(i) != ' ')
                return i;
        }
        return limit;
    }

    private void parseLine(Line line)
    {
        currentLine = line;
        tokenBegin = 0;
        final String text = getDetabbedText(line);
        int state = line.flags();
        clearSegmentList();
        final int limit = text.length();
        int i = 0;
        while (i < limit) {
            char c = text.charAt(i);
            if (c == '\\' && i < limit-1) {
                i += 2;
                continue;
            }
            if (state == STATE_COMMENT) {
                if (c == '|' && i < limit-1) {
                    c = text.charAt(i+1);
                    if (c == '#') {
                        i += 2;
                        endToken(text, i, state);
                        state = STATE_NEUTRAL;
                        continue;
                    }
                }
                ++i;
                continue;
            }
            if (state == STATE_QUOTE) {
                if (c == '"') {
                    endToken(text, i+1, state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            // Reaching here, we're not in a comment or quoted string.
            if (c == '"') {
                endToken(text, i, state);
                state = STATE_QUOTE;
                ++i;
                continue;
            }
            if (c == ';') {
                endToken(text, i, state);
                endToken(text, limit, STATE_COMMENT);
                return;
            }
            if (c == '#' && i < limit - 1) {
                endToken(text, i, state);
                c = text.charAt(i + 1);
                if (c == '|') {
                    state = STATE_COMMENT;
                    i += 2;
                    continue;
                }
                if (c == '\'') {
                    i += 2;
                    continue;
                }
                if (c == ':') {
                    // Uninterned symbol.
                    i += 2;
                    continue;
                }
                state = STATE_NEUTRAL;
                ++i;
                continue;
            }
            if (c == '\'') {
                endToken(text, i, state);
                state = STATE_NEUTRAL;
                i = skipQuotedObject(text, ++i, state);
                continue;
            }
            if (c == '`') {
                // Backquote.
                endToken(text, i, state);
                state = STATE_PUNCTUATION;
                ++i;
                endToken(text, i, state);
                state = STATE_NEUTRAL;
                continue;
            }
            if (c == ',') {
                endToken(text, i, state);
                state = STATE_PUNCTUATION;
                ++i;
                if (i < limit) {
                    c = text.charAt(i);
                    if (c == '@' || c == '.')
                        ++i;
                }
                endToken(text, i, state);
                state = STATE_SUBSTITUTION;
                continue;
            }
            if (state == STATE_ARGLIST) {
                if (c == '(') {
                    endToken(text, i, state);
                    ++i;
                    endToken(text, i, STATE_OPEN_PAREN);
                    continue;
                }
            }
            if (c == '(') {
                endToken(text, i, state);
                state = STATE_OPEN_PAREN;
                ++i;
                continue;
            }
            if (c == ')') {
                endToken(text, i, state);
                state = STATE_CLOSE_PAREN;
                ++i;
                continue;
            }
            if (state == STATE_OPEN_PAREN) {
                if (c == ':' || c == '&') {
                    endToken(text, i, state);
                    state = STATE_SECONDARY_KEYWORD;
                } else if (!Character.isWhitespace(c)) {
                    endToken(text, i, state);
                    if (isPositionFunctional(text, i, currentLine))
                        state = STATE_DEFUN;
                    else
                        state = STATE_CAR;
                }
                ++i;
                continue;
            }
            if (state == STATE_CLOSE_PAREN) {
                if (c != ')') {
                    endToken(text, i, state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_CAR) {
                if (Character.isWhitespace(c)) {
                    endToken(text, i, state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_DEFUN) {
                if (Character.isWhitespace(c)) {
                    endToken(text, i, state);
                    LineSegment s = segmentList.getLastSegment();
                    if (s != null) {
                        String translated = LispMode.translateDefiner(s.getText());
                        if (translated != null && isDefiner(translated)) {
                            state = STATE_DEFINITION;
                            ++i;
                            continue;
                        }
                    }
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_NAME) {
                if (!mode.isIdentifierPart(c) && c != ':') {
                    endToken(text, i, state);
                    state = STATE_ARGLIST;
                }
                ++i;
                continue;
            }
            if (state == STATE_IDENTIFIER) {
                if (!mode.isIdentifierPart(c) && c != ':') {
                    endToken(text, i, state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_SECONDARY_KEYWORD ||
                state == STATE_SUBSTITUTION) {
                if (!mode.isIdentifierPart(c)) {
                    endToken(text, i, state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_DEFINITION) {
                if (mode.isIdentifierStart(c))
                    state = STATE_NAME;
                ++i;
                continue;
            }
            if (state == STATE_NEUTRAL || state == STATE_ARGLIST ||
                state == STATE_QUOTED_LIST) {
                if (c == ':' || c == '&') {
                    endToken(text, i, state);
                    state = STATE_SECONDARY_KEYWORD;
                } else if (mode.isIdentifierStart(c)) {
                    endToken(text, i, state);
                    state = STATE_IDENTIFIER;
                } else // Still neutral...
                    ;
            }
            ++i;
        }
        endToken(text, i, state);
    }

    public LineSegmentList formatLine(Line line)
    {
        if (line == null) {
            clearSegmentList();
            addSegment("", LISP_FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line);
        return segmentList;
    }

    public boolean parseBuffer()
    {
        int state = STATE_NEUTRAL;
        boolean changed = false;
        Position pos = new Position(buffer.getFirstLine(), 0);
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (c == EOL) {
                if (pos.nextLine()) {
                    changed =
                        setLineFlags(pos.getLine(), state) || changed;
                    continue;
                } else
                    break; // Reached end of buffer.
            }
            if (c == '\\') {
                // Escape.
                pos.skip();
                pos.next();
                continue;
            }
            // Not in comment or quoted string.
            if (c == ';') {
                // Single-line comment beginning. Ignore rest of line.
                if (pos.nextLine()) {
                    changed =
                        setLineFlags(pos.getLine(), state) || changed;
                    continue;
                } else {
                    pos.moveTo(pos.getLine(), pos.getLine().length());
                    break; // Reached end of buffer.
                }
            }
            if (c == '#') {
                if (pos.lookingAt("#|")) {
                    pos.skip(2);
                    changed = skipBalancedComment(pos) || changed;
                } else if (pos.lookingAt("#'"))
                    pos.skip(2);
                else
                    pos.skip();
                continue;
            }
            if (c == '"') {
                pos.skip();
                changed = skipString(pos) || changed;
                continue;
            }
            if (c == '\'') {
                pos.skip();
                changed = skipQuotedObject(pos) || changed;
                continue;
            }
            if (c == '(') {
                state = STATE_OPEN_PAREN;
                pos.skip();
                continue;
            }
            if (state == STATE_OPEN_PAREN) {
                if (!Character.isWhitespace(c))
                    state = STATE_CAR;
                pos.next();
                continue;
            }
            if (state == STATE_CAR) {
                if (c == ')' || Character.isWhitespace(c))
                    state = STATE_NEUTRAL;
                pos.next();
                continue;
            }
            // Default.
            pos.skip();
            continue;
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    private static boolean skipString(Position pos)
    {
        boolean changed = false;
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (c == EOL) {
                if (pos.nextLine()) {
                    if (setLineFlags(pos.getLine(), STATE_QUOTE))
                        changed = true;
                    continue;
                } else
                    break; // Reached end of buffer.
            }
            if (c == '\\') {
                // Escape.
                pos.skip();
                if (pos.getChar() == EOL) {
                    if (pos.nextLine()) {
                        if (setLineFlags(pos.getLine(), STATE_QUOTE))
                            changed = true;
                        continue;
                    } else
                        break; // End of buffer.
                } else {
                    // Not end of line.
                    pos.next();
                    continue;
                }
            }
            if (c == '"') {
                pos.next();
                break;
            }
            // Default.
            pos.skip();
        }
        return changed;
    }

    private static boolean skipBalancedComment(Position pos)
    {
        boolean changed = false;
        int count = 1;
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (c == EOL) {
                if (pos.nextLine()) {
                    if (setLineFlags(pos.getLine(), STATE_COMMENT))
                        changed = true;
                    continue;
                } else
                    break; // End of buffer.
            }
            if (c == '\\') {
                // Escape.
                pos.skip();
                pos.next();
                continue;
            }
            if (c == '#' && pos.lookingAt("#|")) {
                pos.skip(2);
                ++count;
                continue;
            }
            if (c == '|' && pos.lookingAt("|#")) {
                pos.skip(2);
                if (--count == 0)
                    break; // End of comment.
                else
                    continue;
            }
            // Default.
            pos.skip();
        }
        return changed;
    }

    private int skipQuotedObject(String text, int i, int state)
    {
        int count = 0;
        final int limit = text.length();
        // Skip whitespace after quote character.
        while (i < limit && Character.isWhitespace(text.charAt(i)))
            ++i;
        while (i < limit) {
            switch (text.charAt(i)) {
                case ' ':
                case '\t':
                    return i;
                case '(':
                    endToken(text, i, state);
                    ++count;
                    ++i;
                    endToken(text, i, STATE_OPEN_PAREN);
                    break;
                case ')':
                    endToken(text, i, state);
                    ++i;
                    endToken(text, i, STATE_CLOSE_PAREN);
                    if (--count <= 0)
                        return i;
                    break;
                case '\\':
                    ++i;
                    if (i < limit)
                        ++i;
                    break;
                case ';':
                case ',':
                case '"':
                    return i;
                case ':':
                    if (i > 0) {
                        char c = text.charAt(i - 1);
                        if (!mode.isIdentifierPart(c) && c != ':')
                            return i;
                    }
                    ++i;
                    break;
                default:
                    ++i;
                    break;
            }
        }
        return i;
    }

    private static boolean skipQuotedObject(Position pos)
    {
        boolean changed = false;
        int count = 0;
        while (!pos.atEnd()) {
            char c = pos.getChar();
            if (c == EOL) {
                if (pos.nextLine()) {
                    changed =
                        setLineFlags(pos.getLine(), STATE_QUOTED_LIST) || changed;
                    continue;
                } else
                    break; // End of buffer.
            }
            if (Character.isWhitespace(c)) {
                pos.skip();
                continue;
            }
            if (c == '\\') {
                pos.skip();
                pos.next();
                continue;
            }
            if (c == '"') {
                pos.skip();
                changed = skipString(pos) || changed;
                continue;
            }
            if (c == '#' && pos.lookingAt("#(")) {
                ++count;
                pos.skip(2);
                continue;
            }
            if (c == '(') {
                ++count;
                pos.skip();
                continue;
            }
            if (c == ')') {
                pos.skip();
                if (count > 0) {
                    --count;
                    if (count == 0)
                        break;
                }
                continue;
            }
            // Not EOL, whitespace or paren.
            if (count == 0) {
                skipToken(pos);
                break;
            }
            // Default.
            pos.skip();
        }
        return changed;
    }

    private static boolean setLineFlags(Line line, int newFlags)
    {
        if (line.flags() == newFlags)
            return false; // No change.
        line.setFlags(newFlags);
        return true;
    }

    private static void skipToken(Position pos)
    {
        while (!Character.isWhitespace(pos.getChar()) && pos.next())
            ;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("LispMode");
            formatTable.addEntryFromPrefs(LISP_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(LISP_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(LISP_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(LISP_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(LISP_FORMAT_DEFUN, "keyword");
            formatTable.addEntryFromPrefs(LISP_FORMAT_NAME, "function");
            formatTable.addEntryFromPrefs(LISP_FORMAT_PARENTHESIS,
                                          "parenthesis","text");
            formatTable.addEntryFromPrefs(LISP_FORMAT_PUNCTUATION,
                                          "punctuation", "text");
            formatTable.addEntryFromPrefs(LISP_FORMAT_SUBSTITUTION,
                                          "substitution", "text");
            formatTable.addEntryFromPrefs(LISP_FORMAT_SECONDARY_KEYWORD,
                                          "secondaryKeyword", "text");
        }
        return formatTable;
    }
}
