/*
 * ShellScriptFormatter.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

public final class ShellScriptFormatter extends Formatter
{
    private static final int STATE_BACKQUOTE     = STATE_LAST + 1;
    private static final int STATE_EXPANSION     = STATE_LAST + 2;
    private static final int STATE_HERE_DOCUMENT = STATE_LAST + 3;
    private static final int STATE_ECHO          = STATE_LAST + 4;

    private static final int SHELLSCRIPT_FORMAT_TEXT     =  0;
    private static final int SHELLSCRIPT_FORMAT_COMMENT  =  2;

    private static final int SHELLSCRIPT_FORMAT_STRING   =  4;
    private static final int SHELLSCRIPT_FORMAT_KEYWORD  =  5;
    private static final int SHELLSCRIPT_FORMAT_FUNCTION =  6;
    private static final int SHELLSCRIPT_FORMAT_OPERATOR =  7;
    private static final int SHELLSCRIPT_FORMAT_BRACE    =  8;
    private static final int SHELLSCRIPT_FORMAT_NUMBER   =  9;

    private static StringSet keywords;

    private FastStringBuffer sb = new FastStringBuffer();
    private int tokStart;
    private String token;

    private String endOfText;

    public ShellScriptFormatter(Buffer buffer)
    {
        this.buffer = buffer;
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
                case STATE_BACKQUOTE:
                case STATE_HERE_DOCUMENT:
                case STATE_ECHO:
                    format = SHELLSCRIPT_FORMAT_STRING;
                    break;
                case STATE_IDENTIFIER:
                    break;
                case STATE_COMMENT:
                    format = SHELLSCRIPT_FORMAT_COMMENT;
                    break;
                case STATE_OPERATOR:
                    format = SHELLSCRIPT_FORMAT_OPERATOR;
                    break;
                case STATE_BRACE:
                    format = SHELLSCRIPT_FORMAT_BRACE;
                    break;
                case STATE_NUMBER:
                case STATE_HEXNUMBER:
                    format = SHELLSCRIPT_FORMAT_NUMBER;
                    break;
            }
            token = sb.toString();
            addSegment(token, format);
            tokStart += token.length();
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
        int braceCount = 0;
        sb.setLength(0);
        int i = 0;
        tokStart = 0;
        if (state == STATE_HERE_DOCUMENT) {
            if (text.startsWith(endOfText))
                state = STATE_NEUTRAL;
            else {
                sb.append(text);
                endToken(state);
                return;
            }
        }
        int limit = text.length();
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
        while (i < limit) {
            c = text.charAt(i);
            if (state == STATE_QUOTE) {
                if (c == '"') {
                    sb.append(c);
                    endToken(state);
                    state = STATE_NEUTRAL;
                } else {
                    sb.append(c);
                    if (c == '\\' && i < limit-1) {
                        // Escape char.
                        sb.append(text.charAt(++i));
                    }
                }
                ++i;
                continue;
            }
            if (state == STATE_SINGLEQUOTE) {
                if (c == '\'') {
                    sb.append(c);
                    endToken(state);
                    state = STATE_NEUTRAL;
                } else {
                    sb.append(c);
                }
                ++i;
                continue;
            }
            if (state == STATE_BACKQUOTE) {
                if (c == '`') {
                    sb.append(c);
                    endToken(state);
                    state = STATE_NEUTRAL;
                } else {
                    sb.append(c);
                }
                ++i;
                continue;
            }
            // Reaching here, we're not in a quoted string.
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
            if (c == '`') {
                endToken(state);
                sb.append(c);
                state = STATE_BACKQUOTE;
                ++i;
                continue;
            }
            if (state == STATE_ECHO) {
                if (c == '\\' && i < limit-1) {
                    // Escape.
                    sb.append(c);
                    ++i;
                    sb.append(text.charAt(i));
                    ++i;
                    continue;
                }
                // Look for terminating ';'.
                if (c == ';') {
                    endToken(state);
                    sb.append(c);
                    state = STATE_NEUTRAL;
                } else
                    sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_EXPANSION) {
                if (c == '{') {
                    ++braceCount;
                    sb.append(c);
                    ++i;
                    continue;
                }
                if (c == '}') {
                    --braceCount;
                    if (braceCount == 0) {
                        sb.append(c) ;
                        endToken(state);
                        state = STATE_NEUTRAL;
                        ++i;
                        continue;
                    } else {
                        sb.append(c);
                        ++i;
                        continue;
                    }
                }
                if (braceCount == 0) {
                    if (!buffer.mode.isIdentifierPart(c)) {
                        endToken(state);
                        sb.append(c) ;
                        state = STATE_NEUTRAL;
                        ++i;
                        continue;
                    }
                }
                sb.append(c);
                ++i;
                continue;
            }
            if (c == '$') {
                endToken(state);
                sb.append(c);
                state = STATE_EXPANSION;
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
                if (buffer.mode.isIdentifierPart(c))
                    sb.append(c);
                else {
                    endToken(state);
                    if (token.equals("echo"))
                        state = STATE_ECHO;
                    else
                        state = STATE_NEUTRAL;
                    sb.append(c);
                }
                ++i;
                continue;
            }
            if (state == STATE_NUMBER) {
                if (Character.isDigit(c))
                    sb.append(c);
                else {
                    endToken(state);
                    sb.append(c);
                    if (buffer.mode.isIdentifierStart(c))
                        state = STATE_IDENTIFIER;
                    else
                        state = STATE_NEUTRAL;
                }
                ++i;
                continue;
            }
            if (state == STATE_NEUTRAL) {
                if (buffer.mode.isIdentifierStart(c)) {
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

    public LineSegmentList formatLine(Line line)
    {
        if (line == null) {
            clearSegmentList();
            addSegment("", SHELLSCRIPT_FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line.getText(), line.flags());
        for (int i = 0; i < segmentList.size(); i++) {
            LineSegment segment = segmentList.getSegment(i);
            if (segment.getFormat() > 0)
                continue;
            String s = segment.getText();
            if (isKeyword(s))
                segment.setFormat(SHELLSCRIPT_FORMAT_KEYWORD);
            else
                segment.setFormat(SHELLSCRIPT_FORMAT_TEXT);
        }
        return segmentList;
    }

    public boolean parseBuffer()
    {
        int state = STATE_NEUTRAL;
        Line line = buffer.getFirstLine();
        boolean changed = false;
        char quoteChar = '\0';
        while (line != null) {
            int oldflags = line.flags();
            if (state == STATE_HERE_DOCUMENT) {
                if (line.getText().equals(endOfText))
                    state = STATE_NEUTRAL;
            }
            if (state != oldflags) {
                line.setFlags(state);
                changed = true;
            }
            if (state == STATE_HERE_DOCUMENT) {
                line = line.next();
                continue;
            }
            final int limit = line.length();
            for (int i = 0; i < limit; i++) {
                char c = line.charAt(i);
                if (c == '\\' && i < limit - 1) {
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
                if (c == '<' && i < limit-2) {
                    if (line.charAt(i + 1) == '<') {
                        endOfText = line.substring(i + 2).trim();
                        if (endOfText.startsWith("-"))
                            endOfText = endOfText.substring(1);
                        int length = endOfText.length();
                        if (length > 2) {
                            if (endOfText.charAt(0) == '"' &&
                                endOfText.charAt(length - 1) == '"') {
                                // Removed enclosing double quotes.
                                endOfText = endOfText.substring(1, length-1);
                            } else if (endOfText.charAt(0) == '\'' &&
                                endOfText.charAt(length - 1) == '\'') {
                                // Removed enclosing single quotes.
                                endOfText = endOfText.substring(1, length-1);
                            }
                        }
                        if (endOfText.length() > 0) {
                            // Make sure "<<" is not shift operator.
                            if (Character.isLetter(endOfText.charAt(0))) {
                                state = STATE_HERE_DOCUMENT;
                                break;
                            }
                        }
                    }
                    continue;
                }
                if (c == '#') {
                    // BUG!! Could be inside ${ ... }
                    // Single-line comment beginning.
                    // Ignore rest of line.
                    break;
                }
                if (c == '"')
                    state = STATE_QUOTE;
                else if (c == '\'')
                    state = STATE_SINGLEQUOTE;
            }
            line = line.next();
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("ShellScriptMode");
            formatTable.addEntryFromPrefs(SHELLSCRIPT_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(SHELLSCRIPT_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(SHELLSCRIPT_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(SHELLSCRIPT_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(SHELLSCRIPT_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(SHELLSCRIPT_FORMAT_OPERATOR, "operator");
            formatTable.addEntryFromPrefs(SHELLSCRIPT_FORMAT_BRACE, "brace");
            formatTable.addEntryFromPrefs(SHELLSCRIPT_FORMAT_NUMBER, "number");
        }
        return formatTable;
    }
}
