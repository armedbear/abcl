/*
 * RubyFormatter.java
 *
 * Copyright (C) 2002 Jens Luedicke <jens@irs-net.com>
 * based on PythonFormatter.java
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

public final class RubyFormatter extends Formatter
{
    private static final int RUBY_STATE_NEUTRAL          =  0;
    private static final int RUBY_STATE_SINGLE_QUOTE     =  1;
    private static final int RUBY_STATE_DOUBLE_QUOTE     =  2;
    private static final int RUBY_STATE_IDENTIFIER       =  3;
    private static final int RUBY_STATE_COMMENT          =  4;
    private static final int RUBY_STATE_BRACE            =  5;
    private static final int RUBY_STATE_NUMBER           =  6;
    private static final int RUBY_STATE_HEXNUMBER        =  7;
    private static final int RUBY_STATE_OPERATOR         =  8;
    private static final int RUBY_STATE_HERE_DOCUMENT    =  9;
    private static final int RUBY_STATE_POD              = 10;
    private static final int RUBY_STATE_REGEXP           = 11;
    private static final int RUBY_STATE_REGEXP_DELIMITER = 12;

    private static final int RUBY_FORMAT_TEXT       =  0;
    private static final int RUBY_FORMAT_COMMENT    =  1;
    private static final int RUBY_FORMAT_STRING     =  2;
    private static final int RUBY_FORMAT_IDENTIFIER =  3;
    private static final int RUBY_FORMAT_KEYWORD    =  4;
    private static final int RUBY_FORMAT_FUNCTION   =  5;
    private static final int RUBY_FORMAT_OPERATOR   =  6;
    private static final int RUBY_FORMAT_BRACE      =  7;
    private static final int RUBY_FORMAT_NUMBER     =  8;

    private static final RubyMode mode = RubyMode.getMode();

    private String endOfText;

    public RubyFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    private int begin = 0;

    private void endSegment(String text, int offset, int state)
    {
        if (offset - begin > 0) {
            int format;
            switch (state) {
                case RUBY_STATE_NEUTRAL:
                    format = RUBY_FORMAT_TEXT;
                    break;
                case RUBY_STATE_SINGLE_QUOTE:
                case RUBY_STATE_DOUBLE_QUOTE:
                case RUBY_STATE_HERE_DOCUMENT:
                case RUBY_STATE_REGEXP:
                    format = RUBY_FORMAT_STRING;
                    break;
                case RUBY_STATE_REGEXP_DELIMITER:
                    format = RUBY_FORMAT_FUNCTION;
                    break;
                case RUBY_STATE_IDENTIFIER:
                    format = RUBY_FORMAT_IDENTIFIER;
                    break;
                case RUBY_STATE_COMMENT:
                case RUBY_STATE_POD:
                    format = RUBY_FORMAT_COMMENT;
                    break;
                case RUBY_STATE_OPERATOR:
                    format = RUBY_FORMAT_OPERATOR;
                    break;
                case RUBY_STATE_BRACE:
                    format = RUBY_FORMAT_BRACE;
                    break;
                case RUBY_STATE_NUMBER:
                case RUBY_STATE_HEXNUMBER:
                    format = RUBY_FORMAT_NUMBER;
                    break;
                default:
                    format = RUBY_FORMAT_TEXT;
                    break;
            }
            addSegment(text, begin, offset, format);
            begin = offset;
        }
    }

    private void parseLine(Line line)
    {
        String text;
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(line.getText(), buffer.getTabWidth());
        else
            text = Utilities.detab(line.getText(), buffer.getTabWidth());
        begin = 0;
        int state = line.flags();
        int i = 0;
        final int limit = text.length();
        if (state == RUBY_STATE_HERE_DOCUMENT) {
            if (text.trim().startsWith(endOfText))
                state = RUBY_STATE_NEUTRAL;
            else {
                endSegment(text, limit, state);
                return;
            }
        }
        if (state == RUBY_STATE_POD) {
            endSegment(text, limit, state);
            return;
        }
        // Skip whitespace at start of line.
        while (i < limit) {
            if (Character.isWhitespace(text.charAt(i))) {
                ++i;
            } else {
                endSegment(text, i, state);
                break;
            }
        }
        while (i < limit) {
            char c = text.charAt(i);
            if (c == '\\' && i < limit-1) {
                // Escape char.
                i += 2;
                continue;
            }
            if (state == RUBY_STATE_SINGLE_QUOTE) {
                if (c == '\'') {
                    endSegment(text, i+1, state);
                    state = RUBY_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == RUBY_STATE_DOUBLE_QUOTE) {
                if (c == '"') {
                    endSegment(text, i+1, state);
                    state = RUBY_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == RUBY_STATE_REGEXP) {
                if (c == '/') {
                    endSegment(text, i, state);
                    endSegment(text, i+1, RUBY_STATE_REGEXP_DELIMITER);
                    state = RUBY_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            // Reaching here, we're not in a quoted string or regexp.
            if (c == '\'') {
                if (i == 0 || text.charAt(i-1) != '$') {
                    endSegment(text, i, state);
                    state = RUBY_STATE_SINGLE_QUOTE;
                }
                ++i;
                continue;
            }
            if (c == '"') {
                if (i == 0 || text.charAt(i-1) != '$') {
                    endSegment(text, i, state);
                    state = RUBY_STATE_DOUBLE_QUOTE;
                }
                ++i;
                continue;
            }
            if (c == '/') {
                if (isRegExp(text, i)) {
                    endSegment(text, i, state);
                    endSegment(text, i+1, RUBY_STATE_REGEXP_DELIMITER);
                    state = RUBY_STATE_REGEXP;
                }
                ++i;
                continue;
            }
            if (c == '#') {
                endSegment(text, i, state);
                endSegment(text, limit, RUBY_STATE_COMMENT);
                return;
            }
            if (isOperatorChar(c)) {
                if (state != RUBY_STATE_OPERATOR) {
                    endSegment(text, i, state);
                    state = RUBY_STATE_OPERATOR;
                }
                ++i;
                continue;
            }
            if (c == '{' || c == '}') {
                if (state != RUBY_STATE_BRACE) {
                    endSegment(text, i, state);
                    // Check for keyword.
                    LineSegment segment = getLastSegment();
                    if (segment != null && isKeyword(segment.getText()))
                        segment.setFormat(RUBY_FORMAT_KEYWORD);
                    state = RUBY_STATE_BRACE;
                }
                ++i;
                continue;
            }
            if (state == RUBY_STATE_OPERATOR || state == RUBY_STATE_BRACE) {
                if (mode.isIdentifierStart(c)) {
                    endSegment(text, i, state);
                    state = RUBY_STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endSegment(text, i, state);
                    state = RUBY_STATE_NUMBER;
                } else {
                    endSegment(text, i, state);
                    state = RUBY_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == RUBY_STATE_IDENTIFIER) {
                if (!mode.isIdentifierPart(c)) {
                    endSegment(text, i, state);
                    // Check for keyword or function.
                    LineSegment segment = getLastSegment();
                    if (segment != null) {
                        String segmentText = segment.getText();
                        if (isKeyword(segment.getText())) {
                            segment.setFormat(RUBY_FORMAT_KEYWORD);
                        } else if (c == '(') {
                            segment.setFormat(RUBY_FORMAT_FUNCTION);
                        } else if (Character.isWhitespace(c)) {
                            // Look ahead to see if next non-whitespace char is '('.
                            int j = i+1;
                            while (j < limit && Character.isWhitespace(c = text.charAt(j)))
                                ++j;
                            if (c == '(')
                                segment.setFormat(RUBY_FORMAT_FUNCTION);
                        }
                    }
                    state = RUBY_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == RUBY_STATE_NUMBER) {
                if (Character.isDigit(c))
                    ;
                else if (c == 'l' || c == 'L')
                    ;
                else if (i - begin == 1 && c == 'x' || c == 'X')
                    state = RUBY_STATE_HEXNUMBER;
                else {
                    endSegment(text, i, state);
                    if (mode.isIdentifierStart(c))
                        state = RUBY_STATE_IDENTIFIER;
                    else
                        state = RUBY_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == RUBY_STATE_HEXNUMBER) {
                if (Character.isDigit(c))
                    ;
                else if ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
                    ;
                else if (c == 'l' || c == 'L')
                    ;
                else {
                    endSegment(text, i, state);
                    if (mode.isIdentifierStart(c))
                        state = RUBY_STATE_IDENTIFIER;
                    else
                        state = RUBY_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == RUBY_STATE_NEUTRAL) {
                if (mode.isIdentifierStart(c)) {
                    endSegment(text, i, state);
                    state = RUBY_STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    if (i == 0 || text.charAt(i-1) != '$') {
                        endSegment(text, i, state);
                        state = RUBY_STATE_NUMBER;
                    }
                }
            }
            ++i;
        }
        // Reached end of line.
        endSegment(text, i, state);
        if (state == RUBY_STATE_IDENTIFIER) {
            // Last token might be a keyword.
            LineSegment segment = getLastSegment();
            if (segment != null && isKeyword(segment.getText()))
                segment.setFormat(RUBY_FORMAT_KEYWORD);
        }
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

        if (mode.isIdentifierPart(c))
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
        if (!mode.isIdentifierPart(c))
            return true;

        // Last non-whitespace character is a valid identifier character.
        FastStringBuffer sb = new FastStringBuffer(c);
        for (int j = s.length()-2; j >= 0; j--) {
            c = s.charAt(j);
            if (mode.isIdentifierPart(c))
                sb.append(c);
            else
                break;
        }
        String token = sb.reverse().toString();
        String[] ok = { "and", "or", "not", "if", "unless", "when" };
        if (Utilities.isOneOf(token, ok))
            return true;

        return false;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        parseLine(line);
        return segmentList;
    }

    public boolean parseBuffer()
    {
        int state = RUBY_STATE_NEUTRAL;
        Line line = buffer.getFirstLine();
        boolean changed = false;
        while (line != null) {
            int oldflags = line.flags();
            if (state == RUBY_STATE_HERE_DOCUMENT) {
                if (line.getText().equals(endOfText))
                    state = RUBY_STATE_NEUTRAL;
            }
            if (state == RUBY_STATE_POD) {
                if (line.getText().startsWith("=end")) {
                    if (state != oldflags) {
                        line.setFlags(state);
                        changed = true;
                    }
                    state = RUBY_STATE_NEUTRAL;
                    line = line.next();
                    continue;
                }
            }
            if (state == RUBY_STATE_NEUTRAL)
                if (line.getText().startsWith("=begin"))
                    state = RUBY_STATE_POD;
            if (state != oldflags) {
                line.setFlags(state);
                changed = true;
            }
            if (state == RUBY_STATE_HERE_DOCUMENT || state == RUBY_STATE_POD) {
                line = line.next();
                continue;
            }
            final String text = line.getText();
            final int limit = line.length();
            int i = 0;
            while (i < limit) {
                char c = text.charAt(i);
                if (c == '\\') {
                    // Escape.
                    i += 2;
                    continue;
                }
                if (state == RUBY_STATE_SINGLE_QUOTE) {
                    if (c == '\'')
                        state = RUBY_STATE_NEUTRAL;
                    ++i;
                    continue;
                }
                if (state == RUBY_STATE_DOUBLE_QUOTE) {
                    if (c == '"')
                        state = RUBY_STATE_NEUTRAL;
                    ++i;
                    continue;
                }
                if (state == RUBY_STATE_REGEXP) {
                    if (c == '/')
                        state = RUBY_STATE_NEUTRAL;
                    ++i;
                    continue;
                }
                // Not in quoted string or regexp.
                if (c == '/') {
                    if (isRegExp(text, i))
                        state = RUBY_STATE_REGEXP;
                    ++i;
                    continue;
                }
                if (c == '<' && i < limit-3 && line.charAt(i+1) == '<') {
                    // There must be no space between "<<" and the terminator.
                    if (!Character.isWhitespace(line.charAt(i+2))) {
                        endOfText = line.substring(i+2).trim();
                        int length = endOfText.length();
                        // Remove ';' at end of line.
                        if (length > 0 && endOfText.charAt(length-1) == ';')
                            endOfText = endOfText.substring(0, --length);
                        // Remove leading '-'.
                        if (length > 0 && endOfText.charAt(0) == '-') {
                            endOfText = endOfText.substring(1);
                            --length;
                        }
                        // Remove enclosing quotes.
                        if (length > 2) {
                            char firstChar = endOfText.charAt(0);
                            if ("\"'`".indexOf(firstChar) >= 0)
                                if (endOfText.charAt(length-1) == firstChar)
                                    endOfText = endOfText.substring(1, length-1);
                        }
                        if (endOfText.length() > 0) {
                            // Make sure "<<" is not shift operator.
                            if (Character.isLetter(endOfText.charAt(0))) {
                                state = RUBY_STATE_HERE_DOCUMENT;
                                break;
                            }
                        }
                    }
                    ++i;
                    continue;
                }
                if (c == '\'') {
                    if (i == 0 || line.charAt(i-1) != '$')
                        state = RUBY_STATE_SINGLE_QUOTE;
                    ++i;
                    continue;
                }
                if (c == '"') {
                    if (i == 0 || line.charAt(i-1) != '$')
                        state = RUBY_STATE_DOUBLE_QUOTE;
                    ++i;
                    continue;
                }
                if (c == '#')
                    break;
                ++i;
            }
            line = line.next();
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    private static final boolean isOperatorChar(char c)
    {
        return "!&|<>=+/*-".indexOf(c) >= 0;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable(null);
            formatTable.addEntryFromPrefs(RUBY_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(RUBY_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(RUBY_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(RUBY_FORMAT_IDENTIFIER, "identifier", "text");
            formatTable.addEntryFromPrefs(RUBY_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(RUBY_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(RUBY_FORMAT_OPERATOR, "operator");
            formatTable.addEntryFromPrefs(RUBY_FORMAT_BRACE, "brace");
            formatTable.addEntryFromPrefs(RUBY_FORMAT_NUMBER, "number");
        }
        return formatTable;
    }
}
