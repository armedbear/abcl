/*
 * PythonFormatter.java
 *
 * Copyright (C) 2002-2007 Peter Graves <peter@armedbear.org>
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
 */

package org.armedbear.j;

import java.util.ArrayList;
import java.util.List;

public final class PythonFormatter extends Formatter
{
    private static final int PYTHON_STATE_NEUTRAL       =  0;
    private static final int PYTHON_STATE_SINGLE_QUOTE  =  1;
    private static final int PYTHON_STATE_DOUBLE_QUOTE  =  2;
    private static final int PYTHON_STATE_IDENTIFIER    =  3;
    private static final int PYTHON_STATE_COMMENT       =  4;
    private static final int PYTHON_STATE_BRACE         =  5;
    private static final int PYTHON_STATE_NUMBER        =  6;
    private static final int PYTHON_STATE_HEXNUMBER     =  7;
    private static final int PYTHON_STATE_OPERATOR      =  8;
    private static final int PYTHON_STATE_TRIPLE_SINGLE =  9;
    private static final int PYTHON_STATE_TRIPLE_DOUBLE = 10;

    private static final int PYTHON_FORMAT_TEXT         =  0;
    private static final int PYTHON_FORMAT_COMMENT      =  1;
    private static final int PYTHON_FORMAT_STRING       =  2;
    private static final int PYTHON_FORMAT_IDENTIFIER   =  3;
    private static final int PYTHON_FORMAT_KEYWORD      =  4;
    private static final int PYTHON_FORMAT_FUNCTION     =  5;
    private static final int PYTHON_FORMAT_OPERATOR     =  6;
    private static final int PYTHON_FORMAT_BRACE        =  7;
    private static final int PYTHON_FORMAT_NUMBER       =  8;

    private static final PythonMode mode = PythonMode.getMode();

    public PythonFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    private int begin = 0;

    private void endSegment(String text, int offset, int state)
    {
        if (offset - begin > 0) {
            int format;
            switch (state) {
                case PYTHON_STATE_NEUTRAL:
                    format = PYTHON_FORMAT_TEXT;
                    break;
                case PYTHON_STATE_SINGLE_QUOTE:
                case PYTHON_STATE_DOUBLE_QUOTE:
                case PYTHON_STATE_TRIPLE_SINGLE:
                case PYTHON_STATE_TRIPLE_DOUBLE:
                    format = PYTHON_FORMAT_STRING;
                    break;
                case PYTHON_STATE_IDENTIFIER:
                    format = PYTHON_FORMAT_IDENTIFIER;
                    break;
                case PYTHON_STATE_COMMENT:
                    format = PYTHON_FORMAT_COMMENT;
                    break;
                case PYTHON_STATE_OPERATOR:
                    format = PYTHON_FORMAT_OPERATOR;
                    break;
                case PYTHON_STATE_BRACE:
                    format = PYTHON_FORMAT_BRACE;
                    break;
                case PYTHON_STATE_NUMBER:
                case PYTHON_STATE_HEXNUMBER:
                    format = PYTHON_FORMAT_NUMBER;
                    break;
                default:
                    format = PYTHON_FORMAT_TEXT;
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

            if (state == PYTHON_STATE_SINGLE_QUOTE) {
                if (c == '\'') {
                    endSegment(text, i+1, state);
                    state = PYTHON_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }

            if (state == PYTHON_STATE_DOUBLE_QUOTE) {
                if (c == '"') {
                    endSegment(text, i+1, state);
                    state = PYTHON_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }

            if (state == PYTHON_STATE_TRIPLE_SINGLE) {
                if (c == '\'' && text.regionMatches(i, "'''", 0, 3)) {
                    i += 3;
                    endSegment(text, i, state);
                    state = PYTHON_STATE_NEUTRAL;
                } else if (((c == '>' && text.regionMatches(i, ">>>", 0, 3)) ||
                            (c == '.' && text.regionMatches(i, "...", 0, 3))) &&
                           (text.substring(0, i).trim().length() == 0)) {
                    endSegment(text, i, state);
                    state = PYTHON_STATE_NEUTRAL;
                } else
                    ++i;
                continue;
            }

            if (state == PYTHON_STATE_TRIPLE_DOUBLE) {
                if (c == '"' && text.regionMatches(i, "\"\"\"", 0, 3)) {
                    i += 3;
                    endSegment(text, i, state);
                    state = PYTHON_STATE_NEUTRAL;
                } else if (((c == '>' && text.regionMatches(i, ">>>", 0, 3)) ||
                            (c == '.' && text.regionMatches(i, "...", 0, 3))) &&
                           (text.substring(0, i).trim().length() == 0)) {
                    endSegment(text, i, state);
                    state = PYTHON_STATE_NEUTRAL;
                } else
                    ++i;
                continue;
            }

            // Reaching here, we're not in a quoted string.
            if (c == '\'') {
                endSegment(text, i, state);
                if (text.regionMatches(i, "'''", 0, 3)) {
                    state = PYTHON_STATE_TRIPLE_SINGLE;
                    i += 3;
                } else {
                    state = PYTHON_STATE_SINGLE_QUOTE;
                    ++i;
                }
                continue;
            }

            if (c == '"') {
                endSegment(text, i, state);
                if (text.regionMatches(i, "\"\"\"", 0, 3)) {
                    state = PYTHON_STATE_TRIPLE_DOUBLE;
                    i += 3;
                } else {
                    state = PYTHON_STATE_DOUBLE_QUOTE;
                    ++i;
                }
                continue;
            }

            if (c == '#') {
                endSegment(text, i, state);
                endSegment(text, limit, PYTHON_STATE_COMMENT);
                return;
            }

            if (isOperatorChar(c)) {
                if (state != PYTHON_STATE_OPERATOR) {
                    endSegment(text, i, state);
                    state = PYTHON_STATE_OPERATOR;
                }
                ++i;
                continue;
            }

            if (c == '{' || c == '}') {
                if (state != PYTHON_STATE_BRACE) {
                    endSegment(text, i, state);
                    // Check for keyword.
                    LineSegment segment = getLastSegment();
                    if (segment != null && isKeyword(segment.getText()))
                        segment.setFormat(PYTHON_FORMAT_KEYWORD);
                    state = PYTHON_STATE_BRACE;
                }
                ++i;
                continue;
            }

            if (state == PYTHON_STATE_OPERATOR || state == PYTHON_STATE_BRACE) {
                if (mode.isIdentifierStart(c)) {
                    endSegment(text, i, state);
                    state = PYTHON_STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endSegment(text, i, state);
                    state = PYTHON_STATE_NUMBER;
                } else {
                    endSegment(text, i, state);
                    state = PYTHON_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }

            if (state == PYTHON_STATE_IDENTIFIER) {
                if (!mode.isIdentifierPart(c)) {
                    endSegment(text, i, state);
                    // Check for keyword or function.
                    LineSegment segment = getLastSegment();
                    if (segment != null) {
                        String segmentText = segment.getText();
                        if (isKeyword(segment.getText())) {
                            segment.setFormat(PYTHON_FORMAT_KEYWORD);
                        } else if (c == '(') {
                            segment.setFormat(PYTHON_FORMAT_FUNCTION);
                        } else if (Character.isWhitespace(c)) {
                            // Look ahead to see if next non-whitespace char is '('.
                            int j = i+1;
                            while (j < limit && Character.isWhitespace(c = text.charAt(j)))
                                ++j;
                            if (c == '(')
                                segment.setFormat(PYTHON_FORMAT_FUNCTION);
                        }
                    }
                    state = PYTHON_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }

            if (state == PYTHON_STATE_NUMBER) {
                if (Character.isDigit(c))
                    ;
                else if (c == 'l' || c == 'L')
                    ;
                else if (i - begin == 1 && c == 'x' || c == 'X')
                    state = PYTHON_STATE_HEXNUMBER;
                else {
                    endSegment(text, i, state);
                    if (mode.isIdentifierStart(c))
                        state = PYTHON_STATE_IDENTIFIER;
                    else
                        state = PYTHON_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }

            if (state == PYTHON_STATE_HEXNUMBER) {
                if (Character.isDigit(c))
                    ;
                else if ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
                    ;
                else if (c == 'l' || c == 'L')
                    ;
                else {
                    endSegment(text, i, state);
                    if (mode.isIdentifierStart(c))
                        state = PYTHON_STATE_IDENTIFIER;
                    else
                        state = PYTHON_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }

            if (state == PYTHON_STATE_NEUTRAL) {
                if (mode.isIdentifierStart(c)) {
                    endSegment(text, i, state);
                    state = PYTHON_STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endSegment(text, i, state);
                    state = PYTHON_STATE_NUMBER;
                }
            }
            ++i;
        }

        // Reached end of line.
        endSegment(text, i, state);

        if (state == PYTHON_STATE_IDENTIFIER) {
            // Last token might be a keyword.
            LineSegment segment = getLastSegment();
            if (segment != null && isKeyword(segment.getText()))
                segment.setFormat(PYTHON_FORMAT_KEYWORD);
        }
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        parseLine(line);
        return segmentList;
    }

    public boolean parseBuffer()
    {
        int state = PYTHON_STATE_NEUTRAL;
        Line line = buffer.getFirstLine();
        boolean changed = false;
        while (line != null) {
            if (state != line.flags()) {
                line.setFlags(state);
                changed = true;
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
                if (state == PYTHON_STATE_SINGLE_QUOTE) {
                    if (c == '\'')
                        state = PYTHON_STATE_NEUTRAL;
                    ++i;
                    continue;
                }
                if (state == PYTHON_STATE_DOUBLE_QUOTE) {
                    if (c == '"')
                        state = PYTHON_STATE_NEUTRAL;
                    ++i;
                    continue;
                }
                if (state == PYTHON_STATE_TRIPLE_SINGLE) {
                    if (c == '\'' && text.regionMatches(i, "'''", 0, 3)) {
                        state = PYTHON_STATE_NEUTRAL;
                        i += 3;
                    } else
                        ++i;
                    continue;
                }
                if (state == PYTHON_STATE_TRIPLE_DOUBLE) {
                    if (c == '"' && text.regionMatches(i, "\"\"\"", 0, 3)) {
                        state = PYTHON_STATE_NEUTRAL;
                        i += 3;
                    } else
                        ++i;
                    continue;
                }
                // Not in quoted string.
                if (c == '\'') {
                    if (text.regionMatches(i, "'''", 0, 3)) {
                        state = PYTHON_STATE_TRIPLE_SINGLE;
                        i += 3;
                    } else {
                        state = PYTHON_STATE_SINGLE_QUOTE;
                        ++i;
                    }
                    continue;
                }
                if (c == '"') {
                    if (text.regionMatches(i, "\"\"\"", 0, 3)) {
                        state = PYTHON_STATE_TRIPLE_DOUBLE;
                        i += 3;
                    } else {
                        state = PYTHON_STATE_DOUBLE_QUOTE;
                        ++i;
                    }
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
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_IDENTIFIER, "identifier", "text");
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_OPERATOR, "operator");
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_BRACE, "brace");
            formatTable.addEntryFromPrefs(PYTHON_FORMAT_NUMBER, "number");
        }
        return formatTable;
    }
}
