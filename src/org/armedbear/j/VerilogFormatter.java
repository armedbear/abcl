/*
 * VerilogFormatter.java
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

public final class VerilogFormatter extends Formatter implements Constants
{
    private static final int VERILOG_FORMAT_TEXT               = 0;
    private static final int VERILOG_FORMAT_COMMENT            = 1;
    private static final int VERILOG_FORMAT_STRING             = 2;
    private static final int VERILOG_FORMAT_IDENTIFIER         = 3;
    private static final int VERILOG_FORMAT_KEYWORD            = 4;
    private static final int VERILOG_FORMAT_COMPILER_DIRECTIVE = 5;
    private static final int VERILOG_FORMAT_FUNCTION           = 6;
    private static final int VERILOG_FORMAT_OPERATOR           = 7;
    private static final int VERILOG_FORMAT_NUMBER             = 8;

    private static final VerilogMode mode = VerilogMode.getMode();

    public VerilogFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    private int tokenBegin = 0;

    private void endToken(String text, int tokenEnd, int state)
    {
        if (tokenEnd - tokenBegin > 0) {
            int format = VERILOG_FORMAT_TEXT;
            switch (state) {
                case STATE_NEUTRAL:
                    format = VERILOG_FORMAT_TEXT;
                    break;
                case STATE_QUOTE:
                    format = VERILOG_FORMAT_STRING;
                    break;
                case STATE_IDENTIFIER:
                    format = VERILOG_FORMAT_IDENTIFIER;
                    break;
                case STATE_COMMENT:
                    format = VERILOG_FORMAT_COMMENT;
                    break;
                case STATE_OPERATOR:
                    format = VERILOG_FORMAT_OPERATOR;
                    break;
                case STATE_NUMBER:
                    format = VERILOG_FORMAT_NUMBER;
                    break;
            }
            addSegment(text, tokenBegin, tokenEnd, format);
            tokenBegin = tokenEnd;
        }
    }

    private void parseLine(Line line)
    {
        if (line == null) {
            addSegment("", VERILOG_FORMAT_TEXT);
            return;
        }
        String text;
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
            if (Character.isWhitespace(text.charAt(i))) {
                ++i;
            } else {
                endToken(text, i, state);
                break;
            }
        }
        while (i < limit) {
            char c = text.charAt(i);
            if (state == STATE_COMMENT) {
                if (i < limit-1 && c == '*' && text.charAt(i+1) == '/') {
                    endToken(text, i + 2, state);
                    state = STATE_NEUTRAL;
                    i += 2;
                } else
                    ++i;
                continue;
            }
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
                    // Check for keyword.
                    final LineSegment segment = getLastSegment();
                    if (segment != null) {
                        final String segmentText = segment.getText();
                        if (isKeyword(segmentText))
                            segment.setFormat(VERILOG_FORMAT_KEYWORD);
                        else if (isCompilerDirective(segmentText))
                            segment.setFormat(VERILOG_FORMAT_COMPILER_DIRECTIVE);
                    }
                    state = STATE_OPERATOR;
                }
                ++i;
                continue;
            }
            if (state == STATE_OPERATOR) {
                if (c == '\'') {
                    if (i < limit-1) {
                        c = text.charAt(i+1);
                        if ("bBoOdDhH".indexOf(c) >= 0) {
                            endToken(text, i, state);
                            state = STATE_NUMBER;
                            i += 2;
                            continue;
                        }
                    }
                } else if (mode.isIdentifierStart(c)) {
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
                            segment.setFormat(VERILOG_FORMAT_KEYWORD);
                        } else if (isCompilerDirective(segmentText)) {
                            segment.setFormat(VERILOG_FORMAT_COMPILER_DIRECTIVE);
                        } else if (c == '(') {
                            segment.setFormat(VERILOG_FORMAT_FUNCTION);
                        } else if (Character.isWhitespace(c)) {
                            // Look ahead to see if next non-whitespace char is '('.
                            int j = i + 1;
                            while (j < limit && Character.isWhitespace(c = text.charAt(j)))
                                ++j;
                            if (c == '(')
                                segment.setFormat(VERILOG_FORMAT_FUNCTION);
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
                else if ("xXzZ?_".indexOf(c) >= 0)
                    ; // Other legal values.
                else if (c == '\'') {
                    if (i < limit-1) {
                        c = text.charAt(i+1);
                        if ("bBoOdDhH".indexOf(c) >= 0) {
                            i += 2;
                            continue;
                        }
                    }
                } else {
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
                if (c == '\'') {
                    if (i < limit-1) {
                        c = text.charAt(i+1);
                        if ("bBoOdDhH".indexOf(c) >= 0) {
                            endToken(text, i, state);
                            state = STATE_NUMBER;
                            i += 2;
                            continue;
                        }
                    }
                } else if (mode.isIdentifierStart(c)) {
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
                    segment.setFormat(VERILOG_FORMAT_KEYWORD);
                else if (isCompilerDirective(segmentText))
                    segment.setFormat(VERILOG_FORMAT_COMPILER_DIRECTIVE);
            }
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
        int state = STATE_NEUTRAL;
        Line line = buffer.getFirstLine();
        boolean changed = false;
        while (line != null) {
            int oldflags = line.flags();
            // Quoted strings can't span lines.
            if (state == STATE_QUOTE)
                state = STATE_NEUTRAL;
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
                    if (c == '"')
                        state = STATE_NEUTRAL;
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
                } else if (c == '"')
                    state = STATE_QUOTE;
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
            formatTable = new FormatTable("VerilogMode");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_IDENTIFIER, "identifier", "text");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_COMPILER_DIRECTIVE, "preprocessor");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_OPERATOR, "operator");
            formatTable.addEntryFromPrefs(VERILOG_FORMAT_NUMBER, "number");
        }
        return formatTable;
    }

    private static boolean isCompilerDirective(String s)
    {
        if (s.length() > 0 && s.charAt(0) == '`')
            return getCompilerDirectives().contains(s);
        return false;
    }

    private static HashSet compilerDirectiveHashSet;

    private static HashSet getCompilerDirectives()
    {
        if (compilerDirectiveHashSet == null) {
            String[] array = compilerDirectives;
            int count = array.length;
            compilerDirectiveHashSet = new HashSet(Math.max(2 * count, 11));
            for (int i = count - 1; i >= 0; i--)
                compilerDirectiveHashSet.add(array[i]);
        }
        return compilerDirectiveHashSet;
    }

    private static String[] compilerDirectives = {
        "`autoexpand_vectornets",
        "`celldefine",
        "`default_nettype",
        "`define",
        "`delay_mode_distributed",
        "`delay_mode_path",
        "`delay_mode_unit",
        "`delay_mode_zero",
        "`else",
        "`endcelldefine",
        "`endif",
        "`endprotect",
        "`endprotected",
        "`expand_vectornets",
        "`ifdef",
        "`ifndef",
        "`include",
        "`noexpand_vectornets",
        "`noremove_gatename",
        "`noremove_netname",
        "`nounconnected_drive",
        "`protect",
        "`protected",
        "`remove_gatename",
        "`remove_netname",
        "`reset_all",
        "`signed",
        "`timescale",
        "`unconnected_drive",
        "`undef",
        "`unsigned",
        "`uselib"
    };
}
