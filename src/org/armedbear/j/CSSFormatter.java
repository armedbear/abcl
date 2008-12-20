/*
 * CSSFormatter.java
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

public final class CSSFormatter extends Formatter implements Constants
{
    // States (used in line flags).
    private static final int CSS_STATE_QUOTE       = 0x0001;
    private static final int CSS_STATE_SINGLEQUOTE = 0x0002;
    private static final int CSS_STATE_COMMENT     = 0x0004;
    private static final int CSS_STATE_BRACE       = 0x0008;
    private static final int CSS_STATE_IN_BLOCK    = 0x0010;
    private static final int CSS_STATE_NUMBER      = 0x0020;
    private static final int CSS_STATE_PROPERTY    = 0x0040;
    private static final int CSS_STATE_VALUE       = 0x0080;
    private static final int CSS_STATE_SELECTOR    = 0x0100;

    // Formats.
    private static final int CSS_FORMAT_TEXT       = 0;
    private static final int CSS_FORMAT_COMMENT    = 1;
    private static final int CSS_FORMAT_STRING     = 2;
    private static final int CSS_FORMAT_PROPERTY   = 3;
    private static final int CSS_FORMAT_BRACE      = 4;
    private static final int CSS_FORMAT_NUMBER     = 5;
    private static final int CSS_FORMAT_SELECTOR   = 6;

    private static final CSSMode mode = CSSMode.getMode();

    public CSSFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    private int tokenBegin = 0;

    private void endToken(String text, int tokenEnd, int state)
    {
        if (tokenEnd - tokenBegin > 0) {
            int format = CSS_FORMAT_TEXT;
            if ((state & (CSS_STATE_QUOTE | CSS_STATE_SINGLEQUOTE)) != 0)
                format = CSS_FORMAT_STRING;
            else if ((state & CSS_STATE_COMMENT) != 0)
                format = CSS_FORMAT_COMMENT;
            else if ((state & CSS_STATE_BRACE) != 0)
                format = CSS_FORMAT_BRACE;
            else if ((state & CSS_STATE_NUMBER) != 0)
                format = CSS_FORMAT_NUMBER;
            else if ((state & CSS_STATE_PROPERTY) != 0)
                format = CSS_FORMAT_PROPERTY;
            else if ((state & CSS_STATE_VALUE) != 0)
                format = CSS_FORMAT_TEXT;
            else if ((state & CSS_STATE_SELECTOR) != 0)
                format = CSS_FORMAT_SELECTOR;
            addSegment(text, tokenBegin, tokenEnd, format);
            tokenBegin = tokenEnd;
        }
    }

    private void parseLine(Line line)
    {
        final String text;
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(line.getText(), buffer.getTabWidth());
        else
            text = Utilities.detab(line.getText(), buffer.getTabWidth());
        tokenBegin = 0;
        int state = line.flags();
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
        while (i < limit) {
            final char c = text.charAt(i);
            if (c == '\\' && i < limit-1) {
                // Escape character.
                i += 2;
                continue;
            }
            if ((state & CSS_STATE_COMMENT) != 0) {
                if (i < limit-1 && c == '*' && text.charAt(i+1) == '/') {
                    endToken(text, i+2, state);
                    state &= ~CSS_STATE_COMMENT;
                    i += 2;
                } else
                    ++i;
                continue;
            }
            if ((state & CSS_STATE_QUOTE) != 0) {
                if (c == '"') {
                    endToken(text, i+1, state);
                    state &= ~CSS_STATE_QUOTE;
                }
                ++i;
                continue;
            }
            if ((state & CSS_STATE_SINGLEQUOTE) != 0) {
                if (c == '"') {
                    endToken(text, i+1, state);
                    state &= ~CSS_STATE_SINGLEQUOTE;
                }
                ++i;
                continue;
            }
            // Reaching here, we're not in a comment or a quoted string.
            if (c == '"') {
                endToken(text, i, state);
                state |= CSS_STATE_QUOTE;
                ++i;
                continue;
            }
            if (c == '\'') {
                endToken(text, i, state);
                state |= CSS_STATE_SINGLEQUOTE;
                ++i;
                continue;
            }
            if (c == '/') {
                if (i < limit-1) {
                    if (text.charAt(i+1) == '*') {
                        endToken(text, i, state);
                        state |= CSS_STATE_COMMENT;
                        i += 2;
                    } else
                        ++i;
                } else
                    ++i;
                continue;
            }
            if (c == '{') {
                endToken(text, i, state);
                endToken(text, ++i, CSS_STATE_BRACE);
                state |= CSS_STATE_IN_BLOCK;
                continue;
            }
            if (c == '}') {
                endToken(text, i, state);
                endToken(text, ++i, CSS_STATE_BRACE);
                state &= ~CSS_STATE_IN_BLOCK;
                continue;
            }
            if ((state & CSS_STATE_IN_BLOCK) != 0) {
                if ((state & CSS_STATE_NUMBER) != 0) {
                    boolean isNumeric;
                    if ("0123456789".indexOf(c) >= 0) {
                        // Definitely a number.
                        isNumeric = true;
                    } else if ("abcdefABCDEF".indexOf(c) >= 0) {
                        if (c == 'e' && i < limit-1 && text.charAt(i+1) == 'm') {
                            // Not a number ("em").
                            isNumeric = false;
                        } else {
                            // Hex digit.
                            isNumeric = true;
                        }
                    } else
                        isNumeric = false;
                    if (!isNumeric) {
                        // Not a number.
                        endToken(text, i, state);
                        state &= ~CSS_STATE_NUMBER;
                    }
                    ++i;
                    continue;
                }
                if ((state & CSS_STATE_VALUE) != 0) {
                    if (c == '#' || Character.isDigit(c)) {
                        endToken(text, i, state);
                        state |= CSS_STATE_NUMBER;
                        ++i;
                        continue;
                    }
                    if (c == '-' && i < limit-1 && Character.isDigit(text.charAt(i+1))) {
                        endToken(text, i, state);
                        state |= CSS_STATE_NUMBER;
                        i += 2;
                        continue;
                    }
                    if (c == ';') {
                        // End of value.
                        endToken(text, i, state);
                        state &= ~CSS_STATE_VALUE;
                        ++i;
                        continue;
                    }
                    ++i;
                    continue;
                }
                if (c == ':') {
                    endToken(text, i, CSS_STATE_PROPERTY);
                    state |= CSS_STATE_VALUE;
                    ++i;
                    continue;
                }
            }
            if ((state & CSS_STATE_SELECTOR) != 0) {
                if (!mode.isIdentifierPart(c)) {
                    endToken(text, i, state);
                    state &= ~CSS_STATE_SELECTOR;
                }
                ++i;
                continue;
            }
            if (state == 0) {
                if (mode.isIdentifierStart(c)) {
                    endToken(text, i, state);
                    state |= CSS_STATE_SELECTOR;
                }
            }
            ++i;
        }
        // Reached end of line.
        endToken(text, i, state);
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null) {
            addSegment("", CSS_FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line);
        return segmentList;
    }

    public boolean parseBuffer()
    {
        int state = 0;
        Line line = buffer.getFirstLine();
        boolean changed = false;
        while (line != null) {
            int oldflags = line.flags();
            // Quoted strings can't span lines. (Can they?)
            state &= ~(CSS_STATE_QUOTE | CSS_STATE_SINGLEQUOTE);
            if (state != oldflags) {
                line.setFlags(state);
                changed = true;
            }
            final int limit = line.length();
            for (int i = 0; i < limit; i++) {
                char c = line.charAt(i);
                if (c == '\\' && i < limit-1) {
                    // Escape.
                    ++i;
                    continue;
                }
                if ((state & CSS_STATE_COMMENT) != 0) {
                    if (c == '*' && i < limit-1) {
                        c = line.charAt(i+1);
                        if (c == '/') {
                            ++i;
                            state &= ~CSS_STATE_COMMENT;
                        }
                    }
                    continue;
                }
                if ((state & CSS_STATE_QUOTE) != 0) {
                    if (c == '"')
                        state &= ~CSS_STATE_QUOTE;
                    continue;
                }
                if ((state & CSS_STATE_SINGLEQUOTE) != 0) {
                    if (c == '"')
                        state &= ~CSS_STATE_SINGLEQUOTE;
                    continue;
                }
                // Not in comment or quoted string.
                if (c == '{') {
                    state |= CSS_STATE_IN_BLOCK;
                    continue;
                }
                if (c == '}') {
                    state &= ~CSS_STATE_IN_BLOCK;
                    continue;
                }
                if (c == '/' && i < limit-1) {
                    c = line.charAt(++i);
                    if (c == '*')
                        state |= CSS_STATE_COMMENT;
                } else if (c == '"') {
                    state |= CSS_STATE_QUOTE;
                } else if (c == '\'') {
                    state |= CSS_STATE_SINGLEQUOTE;
                }
            }
            line = line.next();
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("CSSMode");
            formatTable.addEntryFromPrefs(CSS_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(CSS_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(CSS_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(CSS_FORMAT_PROPERTY, "property", "keyword");
            formatTable.addEntryFromPrefs(CSS_FORMAT_BRACE, "brace");
            formatTable.addEntryFromPrefs(CSS_FORMAT_NUMBER, "number");
            formatTable.addEntryFromPrefs(CSS_FORMAT_SELECTOR, "selector", "function");
        }
        return formatTable;
    }
}
