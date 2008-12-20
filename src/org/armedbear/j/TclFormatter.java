/*
 * TclFormatter.java
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

public final class TclFormatter extends Formatter implements Constants
{
    private static final int TCL_STATE_NEUTRAL     = 0;
    private static final int TCL_STATE_COMMENT     = 1;
    private static final int TCL_STATE_QUOTE       = 2;
    private static final int TCL_STATE_IDENTIFIER  = 3;
    private static final int TCL_STATE_OPERATOR    = 4;
    private static final int TCL_STATE_BRACE       = 5;
    private static final int TCL_STATE_BRACKET     = 6;
    private static final int TCL_STATE_NUMBER      = 7;

    private static final int TCL_FORMAT_TEXT       = 0;
    private static final int TCL_FORMAT_COMMENT    = 1;
    private static final int TCL_FORMAT_STRING     = 2;
    private static final int TCL_FORMAT_IDENTIFIER = 3;
    private static final int TCL_FORMAT_KEYWORD    = 4;
    private static final int TCL_FORMAT_ARRAY      = 5;
    private static final int TCL_FORMAT_OPERATOR   = 6;
    private static final int TCL_FORMAT_BRACE      = 7;
    private static final int TCL_FORMAT_BRACKET    = 8;
    private static final int TCL_FORMAT_NUMBER     = 9;

    private static final TclMode mode = TclMode.getMode();

    public TclFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    private int tokenBegin = 0;

    private void endToken(String text, int tokenEnd, int state)
    {
        if (tokenEnd - tokenBegin > 0) {
            int format = TCL_FORMAT_TEXT;
            switch (state) {
                case TCL_STATE_NEUTRAL:
                    format = TCL_FORMAT_TEXT;
                    break;
                case TCL_STATE_QUOTE:
                    format = TCL_FORMAT_STRING;
                    break;
                case TCL_STATE_IDENTIFIER:
                    format = TCL_FORMAT_IDENTIFIER;
                    break;
                case TCL_STATE_COMMENT:
                    format = TCL_FORMAT_COMMENT;
                    break;
                case TCL_STATE_OPERATOR:
                    format = TCL_FORMAT_OPERATOR;
                    break;
                case TCL_STATE_BRACE:
                    format = TCL_FORMAT_BRACE;
                    break;
                case TCL_STATE_BRACKET:
                    format = TCL_FORMAT_BRACKET;
                    break;
                case TCL_STATE_NUMBER:
                    format = TCL_FORMAT_NUMBER;
                    break;
            }
            addSegment(text, tokenBegin, tokenEnd, format);
            tokenBegin = tokenEnd;
        }
    }

    private void parseLine(Line line)
    {
        if (line == null) {
            addSegment("", TCL_FORMAT_TEXT);
            return;
        }
        String text;
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(line.getText(), buffer.getTabWidth());
        else
            text = Utilities.detab(line.getText(), buffer.getTabWidth());
        tokenBegin = 0;
        int state = TCL_STATE_NEUTRAL;
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
            if (c == '\\' && i < limit-1) {
                i += 2;
                continue;
            }
            if (state == TCL_STATE_QUOTE) {
                if (c == '"') {
                    endToken(text, i+1, state);
                    state = TCL_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (c == '"') {
                endToken(text, i, state);
                state = TCL_STATE_QUOTE;
                ++i;
                continue;
            }
            if (c == '#') {
                // It's only a real comment if '#' is the first non-whitespace
                // character on the line or if it is immediately preceded by a
                // semicolon, opening brace, or opening bracket.
                boolean isComment = true;
                for (int j = i-1; j >= 0; j--) {
                    char cc = text.charAt(j);
                    if (Character.isWhitespace(cc))
                        continue;
                    if (";{[".indexOf(cc) >= 0)
                        break;
                    // Otherwise...
                    isComment = false;
                    break;
                }
                if (isComment) {
                    endToken(text, i, state);
                    endToken(text, limit, TCL_STATE_COMMENT);
                    return;
                } else {
                    ++i;
                    continue;
                }
            }
            if (c == '{' || c == '}') {
                if (state != TCL_STATE_BRACE) {
                    endToken(text, i, state);
                    state = TCL_STATE_BRACE;
                }
                ++i;
                continue;
            }
            if (c == '[' || c == ']') {
                if (state != TCL_STATE_BRACKET) {
                    endToken(text, i, state);
                    state = TCL_STATE_BRACKET;
                }
                ++i;
                continue;
            }
            if (state == TCL_STATE_BRACE || state == TCL_STATE_BRACKET) {
                if (mode.isIdentifierStart(c)) {
                    endToken(text, i, state);
                    state = TCL_STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endToken(text, i, state);
                    state = TCL_STATE_NUMBER;
                } else {
                    endToken(text, i, state);
                    state = TCL_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == TCL_STATE_IDENTIFIER) {
                if (!mode.isIdentifierPart(c)) {
                    endToken(text, i, state);
                    final LineSegment segment = getLastSegment();
                    if (segment != null) {
                        final String segmentText = segment.getText();
                        if (isKeyword(segmentText))
                            segment.setFormat(TCL_FORMAT_KEYWORD);
                        else if (isOperator(segmentText))
                            segment.setFormat(TCL_FORMAT_OPERATOR);
                        else if (c == '(')
                            segment.setFormat(TCL_FORMAT_ARRAY);
                    }
                    state = TCL_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == TCL_STATE_NUMBER) {
                if (!Character.isDigit(c)) {
                    endToken(text, i, state);
                    if (mode.isIdentifierStart(c))
                        state = TCL_STATE_IDENTIFIER;
                    else
                        state = TCL_STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == TCL_STATE_NEUTRAL) {
                if (mode.isIdentifierStart(c)) {
                    endToken(text, i, state);
                    state = TCL_STATE_IDENTIFIER;
                } else if (Character.isDigit(c)) {
                    endToken(text, i, state);
                    state = TCL_STATE_NUMBER;
                }
            }
            ++i;
        }

        // Reached end of line.
        endToken(text, i, state);
        if (state == TCL_STATE_IDENTIFIER) {
            final LineSegment segment = getLastSegment();
            if (segment != null) {
                final String segmentText = segment.getText();
                if (isKeyword(segmentText))
                    segment.setFormat(TCL_FORMAT_KEYWORD);
                else if (isOperator(segmentText))
                    segment.setFormat(TCL_FORMAT_OPERATOR);
            }
        }
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        parseLine(line);
        return segmentList;
    }

    private static final boolean isOperator(String s)
    {
        for (int i = s.length()-1; i >= 0; i--) {
            if (!isOperatorChar(s.charAt(i)))
                return false;
        }
        return true;
    }

    private static final boolean isOperatorChar(char c)
    {
        return "!&|<>=+/*-".indexOf(c) >= 0;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("TclMode");
            formatTable.addEntryFromPrefs(TCL_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(TCL_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(TCL_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(TCL_FORMAT_IDENTIFIER, "identifier", "text");
            formatTable.addEntryFromPrefs(TCL_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(TCL_FORMAT_ARRAY, "text");
            formatTable.addEntryFromPrefs(TCL_FORMAT_OPERATOR, "operator");
            formatTable.addEntryFromPrefs(TCL_FORMAT_BRACE, "brace");
            formatTable.addEntryFromPrefs(TCL_FORMAT_BRACKET, "bracket", "brace");
            formatTable.addEntryFromPrefs(TCL_FORMAT_NUMBER, "number");
        }
        return formatTable;
    }
}
