/*
 * VHDLFormatter.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.util.HashSet;

public final class VHDLFormatter extends Formatter implements Constants
{
    private static final int VHDL_FORMAT_TEXT       = 0;
    private static final int VHDL_FORMAT_COMMENT    = 1;
    private static final int VHDL_FORMAT_STRING     = 2;
    private static final int VHDL_FORMAT_IDENTIFIER = 3;
    private static final int VHDL_FORMAT_KEYWORD    = 4;
    private static final int VHDL_FORMAT_TYPE       = 5;
    private static final int VHDL_FORMAT_FUNCTION   = 6;
    private static final int VHDL_FORMAT_OPERATOR   = 7;
    private static final int VHDL_FORMAT_NUMBER     = 8;

    private static final VHDLMode mode = VHDLMode.getMode();

    public VHDLFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    private int tokenBegin = 0;

    private void endToken(String text, int tokenEnd, int state)
    {
        if (tokenEnd - tokenBegin > 0) {
            int format = VHDL_FORMAT_TEXT;
            switch (state) {
                case STATE_NEUTRAL:
                    format = VHDL_FORMAT_TEXT;
                    break;
                case STATE_QUOTE:
                    format = VHDL_FORMAT_STRING;
                    break;
                case STATE_IDENTIFIER:
                    format = VHDL_FORMAT_IDENTIFIER;
                    break;
                case STATE_COMMENT:
                    format = VHDL_FORMAT_COMMENT;
                    break;
                case STATE_OPERATOR:
                    format = VHDL_FORMAT_OPERATOR;
                    break;
                case STATE_NUMBER:
                    format = VHDL_FORMAT_NUMBER;
                    break;
            }
            addSegment(text, tokenBegin, tokenEnd, format);
            tokenBegin = tokenEnd;
        }
    }

    private void parseLine(Line line)
    {
        if (line == null) {
            addSegment("", VHDL_FORMAT_TEXT);
            return;
        }
        String text;
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(line.getText(), buffer.getTabWidth());
        else
            text = Utilities.detab(line.getText(), buffer.getTabWidth());
        tokenBegin = 0;
        int state = STATE_NEUTRAL;
        int i = 0;
        final int limit = text.length();
        // Skip whitespace at start of line.
        while (i < limit) {
            if (Character.isWhitespace(text.charAt(i))) {
                ++i;
            } else {
                endToken(text, i, state);
                break;
            }
        }
        while (i < limit) {
            char c = text.charAt(i);
            if (state == STATE_QUOTE) {
                if (c == '"') {
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
            if (c == '"') {
                endToken(text, i, state);
                state = STATE_QUOTE;
                ++i;
                continue;
            }
            if (c == '-') {
                if (i < limit-1) {
                    if (text.charAt(i+1) == '-') {
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
                    // Check for keyword.
                    final LineSegment segment = getLastSegment();
                    if (segment != null) {
                        final String segmentText = segment.getText();
                        if (isKeyword(segmentText))
                            segment.setFormat(VHDL_FORMAT_KEYWORD);
                        else if (isType(segmentText))
                            segment.setFormat(VHDL_FORMAT_TYPE);
                    }
                    state = STATE_OPERATOR;
                }
                ++i;
                continue;
            }
            if (state == STATE_OPERATOR) {
                if (mode.isIdentifierStart(c)) {
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
                if (!mode.isIdentifierPart(c)) {
                    endToken(text, i, state);
                    // Check for keyword or function.
                    final LineSegment segment = getLastSegment();
                    if (segment != null) {
                        final String segmentText = segment.getText();
                        if (isKeyword(segmentText)) {
                            segment.setFormat(VHDL_FORMAT_KEYWORD);
                        } else if (isType(segmentText)) {
                            segment.setFormat(VHDL_FORMAT_TYPE);
                        } else if (c == '(') {
                            segment.setFormat(VHDL_FORMAT_FUNCTION);
                        } else if (Character.isWhitespace(c)) {
                            // Look ahead to see if next non-whitespace char is '('.
                            int j = i + 1;
                            while (j < limit && Character.isWhitespace(c = text.charAt(j)))
                                ++j;
                            if (c == '(')
                                segment.setFormat(VHDL_FORMAT_FUNCTION);
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
                else if ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
                    ; // Hex digits are OK.
                else {
                    endToken(text, i, state);
                    if (mode.isIdentifierStart(c))
                        state = STATE_IDENTIFIER;
                    else
                        state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_NEUTRAL) {
                if (mode.isIdentifierStart(c)) {
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
            final LineSegment segment = getLastSegment();
            if (segment != null) {
                final String segmentText = segment.getText();
                if (isKeyword(segmentText))
                    segment.setFormat(VHDL_FORMAT_KEYWORD);
                else if (isType(segmentText))
                    segment.setFormat(VHDL_FORMAT_TYPE);
            }
        }
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        parseLine(line);
        return segmentList;
    }

    private static final boolean isOperatorChar(char c)
    {
        return "!&|<>=+/*-".indexOf(c) >= 0;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("VHDLMode");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_IDENTIFIER, "identifier", "text");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_TYPE, "type", "preprocessor");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_OPERATOR, "operator");
            formatTable.addEntryFromPrefs(VHDL_FORMAT_NUMBER, "number");
        }
        return formatTable;
    }

    private static boolean isType(String s)
    {
        return getTypes().contains(s.toLowerCase());
    }

    private static HashSet typeHashSet;

    private static HashSet getTypes()
    {
        if (typeHashSet == null) {
            String[] array = types;
            int count = array.length;
            typeHashSet = new HashSet(Math.max(2 * count, 11));
            for (int i = count - 1; i >= 0; i--)
                typeHashSet.add(array[i]);
        }
        return typeHashSet;
    }

    private static final String[] types = {
        "bit",
        "bit_vector",
        "boolean",
        "character",
        "delay_length",
        "file_open_kind",
        "file_open_status",
        "integer",
        "line",
        "natural",
        "positive",
        "real",
        "severity_level",
        "side",
        "signed",
        "std_logic",
        "std_logic_vector",
        "std_ulogic",
        "std_ulogic_vector",
        "string",
        "text",
        "time",
        "unsigned"
    };
}
