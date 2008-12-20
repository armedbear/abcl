/*
 * JavaFormatter.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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

public final class JavaFormatter extends Formatter implements Constants
{
    private static final int JAVA_FORMAT_TEXT       = 0;
    private static final int JAVA_FORMAT_COMMENT    = 1;
    private static final int JAVA_FORMAT_STRING     = 2;
    private static final int JAVA_FORMAT_IDENTIFIER = 3;
    private static final int JAVA_FORMAT_KEYWORD    = 4;
    private static final int JAVA_FORMAT_FUNCTION   = 5;
    private static final int JAVA_FORMAT_OPERATOR   = 6;
    private static final int JAVA_FORMAT_BRACE      = 7;
    private static final int JAVA_FORMAT_NUMBER     = 8;

    public static final int JAVA_FORMAT_LAST = 8;

    private final int language;

    public JavaFormatter(Buffer buffer)
    {
        this(buffer, LANGUAGE_JAVA);
    }

    public JavaFormatter(Buffer buffer, int language)
    {
        this.buffer = buffer;
        this.language = language;
    }

    private int tokenBegin = 0;

    private void endToken(String text, int tokenEnd, int state)
    {
        if (tokenEnd - tokenBegin > 0) {
            int format = JAVA_FORMAT_TEXT;
            switch (state) {
                case STATE_NEUTRAL:
                    format = JAVA_FORMAT_TEXT;
                    break;
                case STATE_QUOTE:
                    format = JAVA_FORMAT_STRING;
                    break;
                case STATE_IDENTIFIER:
                    format = JAVA_FORMAT_IDENTIFIER;
                    break;
                case STATE_COMMENT:
                    format = JAVA_FORMAT_COMMENT;
                    break;
                case STATE_OPERATOR:
                    format = JAVA_FORMAT_OPERATOR;
                    break;
                case STATE_BRACE:
                    format = JAVA_FORMAT_BRACE;
                    break;
                case STATE_NUMBER:
                case STATE_HEXNUMBER:
                    format = JAVA_FORMAT_NUMBER;
                    break;
            }
            addSegment(text, tokenBegin, tokenEnd, format);
            tokenBegin = tokenEnd;
        }
    }

    private void parseLine(Line line)
    {
        final String text = getDetabbedText(line);
        tokenBegin = 0;
        boolean isPreprocessorLine = false;
        char quoteChar = '\0';
        int state = line.flags();
        if (state == STATE_QUOTE)
            quoteChar = '"';
        int i = 0;
        final int limit = text.length();
        // Skip whitespace at start of line.
        while (i < limit) {
            if (Character.isWhitespace(text.charAt(i)))
                ++i;
            else {
                endToken(text, i, state);
                break;
            }
        }
        char c;
        if (state == STATE_SCRIPT)
            state = STATE_NEUTRAL;
        while (i < limit) {
            c = text.charAt(i);
            if (state == STATE_COMMENT) {
                if (i < limit - 1 && c == '*' && text.charAt(i+1) == '/') {
                    endToken(text, i + 2, state);
                    state = STATE_NEUTRAL;
                    i += 2;
                }
                else
                    ++i;
                continue;
            }
            if (state == STATE_QUOTE) {
                if (c == quoteChar) {
                    endToken(text, i+1, state);
                    state = STATE_NEUTRAL;
                } else if (c == '\\' && i < limit-1) {
                    // Escape char.
                    ++i;
                }
                ++i;
                continue;
            }
            // Reaching here, we're not in a comment or a quoted string.
            if (c == '"' || c == '\'') {
                endToken(text, i, state);
                state = STATE_QUOTE;
                quoteChar = c;
                ++i;
                continue;
            }
            if (c == '/') {
                if (i < limit-1) {
                    if (text.charAt(i+1) == '*') {
                        endToken(text, i, state);
                        state = STATE_COMMENT;
                        i += 2;
                    } else if (text.charAt(i+1) == '/') {
                        endToken(text, i, state);
                        endToken(text, limit, STATE_COMMENT);
                        return;
                    } else
                        ++i;
                } else
                    ++i;
                continue;
            }
            if (isOperatorChar(c)) {
                if (state != STATE_OPERATOR) {
                    endToken(text, i, state);
                    // Check for keyword (as in e.g. "char*").
                    LineSegment segment = getLastSegment();
                    if (segment != null && isKeyword(segment.getText()))
                        segment.setFormat(JAVA_FORMAT_KEYWORD);
                    state = STATE_OPERATOR;
                }
                ++i;
                continue;
            }
            if (c == '{' || c == '}') {
                if (state != STATE_BRACE) {
                    endToken(text, i, state);
                    // Check for keyword (e.g. "try").
                    LineSegment segment = getLastSegment();
                    if (segment != null && isKeyword(segment.getText()))
                        segment.setFormat(JAVA_FORMAT_KEYWORD);
                    state = STATE_BRACE;
                }
                ++i;
                continue;
            }
            if (state == STATE_OPERATOR || state == STATE_BRACE) {
                if (Character.isJavaIdentifierStart(c)) {
                    endToken(text, i, state);
                    state = STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endToken(text, i, state);
                    state = STATE_NUMBER;
                } else {
                    endToken(text, i, state);
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_IDENTIFIER) {
                if (!Character.isJavaIdentifierPart(c)) {
                    endToken(text, i, state);
                    // Check for keyword or function.
                    LineSegment segment = getLastSegment();
                    if (segment != null) {
                        String segmentText = segment.getText();
                        if (!isPreprocessorLine && isKeyword(segmentText))
                            segment.setFormat(JAVA_FORMAT_KEYWORD);
                        else if (c == '(')
                            segment.setFormat(JAVA_FORMAT_FUNCTION);
                        else if (Character.isWhitespace(c)) {
                            // Look ahead to see if next non-whitespace char is '('.
                            int j = i + 1;
                            while (j < limit && Character.isWhitespace(c = text.charAt(j)))
                                ++j;
                            if (c == '(')
                                segment.setFormat(JAVA_FORMAT_FUNCTION);
                        }
                    }
                    state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_NUMBER) {
                if (Character.isDigit(c))
                    ;
                else if (c == 'u' || c == 'U' || c == 'l' || c == 'L')
                    ;
                else if (i - tokenBegin == 1 && c == 'x' || c == 'X')
                    state = STATE_HEXNUMBER;
                else {
                    endToken(text, i, state);
                    if (Character.isJavaIdentifierStart(c))
                        state = STATE_IDENTIFIER;
                    else
                        state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_HEXNUMBER) {
                if (Character.isDigit(c))
                    ;
                else if ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
                    ;
                else if (c == 'u' || c == 'U' || c == 'l' || c == 'L')
                    ;
                else {
                    endToken(text, i, state);
                    if (Character.isJavaIdentifierStart(c))
                        state = STATE_IDENTIFIER;
                    else
                        state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_NEUTRAL) {
                if (Character.isJavaIdentifierStart(c)) {
                    endToken(text, i, state);
                    state = STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endToken(text, i, state);
                    state = STATE_NUMBER;
                }
            }
            ++i;
        }
        // Reached end of line.
        endToken(text, i, state);
        if (state == STATE_IDENTIFIER) {
            // Last token might be a keyword.
            LineSegment segment = getLastSegment();
            if (segment != null && isKeyword(segment.getText()))
                segment.setFormat(JAVA_FORMAT_KEYWORD);
        }
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null) {
            addSegment("", JAVA_FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line);
        return segmentList;
    }

    public boolean parseBuffer()
    {
        int state = STATE_NEUTRAL;
        Line line = buffer.getFirstLine();
        boolean changed = false;
        while (line != null) {
            int oldflags = line.flags();
            // Quoted strings can't span lines in Java.
            if (state == STATE_QUOTE && language == LANGUAGE_JAVA)
                state = STATE_NEUTRAL;

            if (state != oldflags) {
                line.setFlags(state);
                changed = true;
            }
            char quoteChar = state == STATE_QUOTE ? '"' : '\0';
            final int limit = line.length();
            for (int i = 0; i < limit; i++) {
                char c = line.charAt(i);
                if (c == '\\' && i < limit-1) {
                    // Escape.
                    ++i;
                    continue;
                }
                if (state == STATE_COMMENT) {
                    if (c == '*' && i < limit-1) {
                        c = line.charAt(i+1);
                        if (c == '/') {
                            ++i;
                            state = STATE_NEUTRAL;
                        }
                    }
                    continue;
                }
                if (state == STATE_QUOTE) {
                    if (c == quoteChar) {
                        state = STATE_NEUTRAL;
                        quoteChar = '\0';
                    }
                    continue;
                }

                // Not in comment or quoted string.
                if (c == '/' && i < limit-1) {
                    c = line.charAt(++i);
                    if (c == '/') {
                        // Single-line comment beginning.
                        // Ignore rest of line.
                        break;
                    } else if (c == '*')
                        state = STATE_COMMENT;
                } else if (c == '"' || c == '\'') {
                    state = STATE_QUOTE;
                    quoteChar = c;
                }
            }
            line = line.next();
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    private static final boolean isOperatorChar(char c)
    {
        return "!&|<>=+/*-^".indexOf(c) >= 0;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("JavaMode");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_IDENTIFIER, "identifier", "text");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_OPERATOR, "operator");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_BRACE, "brace");
            formatTable.addEntryFromPrefs(JAVA_FORMAT_NUMBER, "number");
        }
        return formatTable;
    }
}
