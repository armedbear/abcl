/*
 * SchemeFormatter.java
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

public final class SchemeFormatter extends Formatter
{
    // Formats.
    private static final int SCHEME_FORMAT_TEXT     = 0;
    private static final int SCHEME_FORMAT_COMMENT  = 1;
    private static final int SCHEME_FORMAT_STRING   = 2;
    private static final int SCHEME_FORMAT_KEYWORD  = 3;
    private static final int SCHEME_FORMAT_FUNCTION = 4;
    private static final int SCHEME_FORMAT_NUMBER   = 5;

    private FastStringBuffer sb = new FastStringBuffer();
    private int tokStart;

    public SchemeFormatter(Buffer buffer)
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
                    format = SCHEME_FORMAT_STRING;
                    break;
                case STATE_IDENTIFIER:
                    break;
                case STATE_COMMENT:
                    format = SCHEME_FORMAT_COMMENT;
                    break;
                case STATE_NUMBER:
                case STATE_HEXNUMBER:
                    format = SCHEME_FORMAT_NUMBER;
                    break;
            }
            addSegment(sb.toString(), format);
            tokStart += sb.length();
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
        tokStart = 0;
        final int limit = text.length();
        // Skip whitespace at start of line.
        while (i < limit) {
            char c = text.charAt(i);
            if (Character.isWhitespace(c)) {
                sb.append(c);
                ++i;
            } else {
                endToken(state);
                break;
            }
        }
        while (i < limit) {
            char c = text.charAt(i);
            if (c == '\\' && i < limit-1) {
                sb.append(c);
                sb.append(text.charAt(++i));
                ++i;
                continue;
            }
            if (state == STATE_COMMENT) {
                if (c == '|' && i < limit-1) {
                    c = text.charAt(i+1);
                    if (c == '#') {
                        sb.append("|#");
                        endToken(state);
                        state = STATE_NEUTRAL;
                        i += 2;
                        continue;
                    }
                }
                sb.append(c);
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
            // Reaching here, we're not in a quoted string.
            if (c == '"') {
                endToken(state);
                sb.append(c);
                state = STATE_QUOTE;
                ++i;
                continue;
            }
            if (c == ';') {
                endToken(state);
                state = STATE_COMMENT;
                sb.append(text.substring(i));
                endToken(state);
                return;
            }
            if (c == '#' && i < limit-1) {
                if (text.charAt(i+1) == '|') {
                    endToken(state);
                    state = STATE_COMMENT;
                    sb.append("#|");
                    i += 2;
                    continue;
                }
            }
            if (state == STATE_IDENTIFIER) {
                if (buffer.getMode().isIdentifierPart(c))
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
                else if (c == 'u' || c == 'U' || c == 'l' || c == 'L')
                    sb.append(c); // Valid suffix.
                else if (sb.length() == 1 && c == 'x' || c == 'X') {
                    sb.append(c);
                    state = STATE_HEXNUMBER;
                } else {
                    endToken(state);
                    sb.append(c);
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
                    sb.append(c);
                else if ((c >= 'a' && c <= 'f') || (c >= 'A' && c <= 'F'))
                    sb.append(c);
                else if (c == 'u' || c == 'U' || c == 'l' || c == 'L')
                    sb.append(c); // Valid suffix.
                else {
                    endToken(state);
                    sb.append(c);
                    if (Character.isJavaIdentifierStart(c))
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
            addSegment("", SCHEME_FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line.getText(), line.flags());
        final int size = segmentList.size();
        for (int i = 0; i < size; i++) {
            LineSegment segment = segmentList.getSegment(i);
            if (segment.getFormat() >= 0)
                continue;
            String token = segment.getText();
            if (isKeyword(token))
                segment.setFormat(SCHEME_FORMAT_KEYWORD);
            else {
                boolean isFunction = false;
                if (i >= 2) {
                    LineSegment prevSegment = segmentList.getSegment(i-2);
                    String prevToken = prevSegment.getText();
                    String trim = prevToken.trim();
                    if (trim.equals("define"))
                        isFunction = true;
                }
                segment.setFormat(isFunction ? SCHEME_FORMAT_FUNCTION : SCHEME_FORMAT_TEXT);
            }
        }
        return segmentList;
    }

    public boolean parseBuffer()
    {
        int state = STATE_NEUTRAL;
        boolean changed = false;
        Line line = buffer.getFirstLine();
        while (line != null) {
            int oldflags = line.flags();
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
                    if (c == '|' && i < limit-1 && line.charAt(i+1) == '#') {
                        ++i;
                        state = STATE_NEUTRAL;
                    }
                    continue;
                }
                if (state == STATE_QUOTE) {
                    if (c == '"')
                        state = STATE_NEUTRAL;
                    continue;
                }
                // Not in comment or quoted string.
                if (c == ';') {
                    // Single-line comment beginning. Ignore rest of line.
                    break;
                }
                if (c == '#') {
                    if (i < limit-1 && line.charAt(i+1) == '|') {
                        state = STATE_COMMENT;
                        ++i;
                    }
                    continue;
                }
                if (c == '"')
                    state = STATE_QUOTE;
            }
            line = line.next();
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("SchemeMode");
            formatTable.addEntryFromPrefs(SCHEME_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(SCHEME_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(SCHEME_FORMAT_STRING, "string");
            formatTable.addEntryFromPrefs(SCHEME_FORMAT_KEYWORD, "keyword");
            formatTable.addEntryFromPrefs(SCHEME_FORMAT_FUNCTION, "function");
            formatTable.addEntryFromPrefs(SCHEME_FORMAT_NUMBER, "number");
        }
        return formatTable;
    }
}
