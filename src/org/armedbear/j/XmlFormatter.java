/*
 * XmlFormatter.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

public final class XmlFormatter extends Formatter
{
    private static final byte XML_FORMAT_TEXT      = 0;
    private static final byte XML_FORMAT_COMMENT   = 1;
    private static final byte XML_FORMAT_DELIMITER = 2;
    private static final byte XML_FORMAT_NAMESPACE = 3;
    private static final byte XML_FORMAT_TAG       = 4;
    private static final byte XML_FORMAT_ATTRIBUTE = 5;
    private static final byte XML_FORMAT_EQUALS    = 6;
    private static final byte XML_FORMAT_QUOTE     = 7;

    private static final byte STATE_NAMESPACE    = STATE_LAST + 1;
    private static final byte STATE_TAG_STARTING = STATE_LAST + 2;
    private static final byte STATE_TAG_ENDING   = STATE_LAST + 3;
    private static final byte STATE_ATTRIBUTE    = STATE_LAST + 4;
    private static final byte STATE_EQUALS       = STATE_LAST + 5;

    private FastStringBuffer sb = new FastStringBuffer();

    public XmlFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    private void endToken(int state)
    {
        if (sb.length() > 0) {
            byte format;
            switch (state) {
                case STATE_COMMENT:
                    format = XML_FORMAT_COMMENT;
                    break;
                case STATE_TAG_STARTING:
                case STATE_TAG_ENDING:
                    format = XML_FORMAT_DELIMITER;
                    break;
                case STATE_EQUALS:
                    format = XML_FORMAT_EQUALS;
                    break;
                case STATE_NAMESPACE:
                    format = XML_FORMAT_NAMESPACE;
                    break;
                case STATE_TAG:
                    format = XML_FORMAT_TAG;
                    break;
                case STATE_ATTRIBUTE:
                    format = XML_FORMAT_ATTRIBUTE;
                    break;
                case STATE_QUOTE:
                case STATE_SINGLEQUOTE:
                    format = XML_FORMAT_QUOTE;
                    break;
                case STATE_NEUTRAL:
                default:
                    format = XML_FORMAT_TEXT;
                    break;
            }
            addSegment(sb.toString(), format);
            sb.setLength(0);
        }
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line != null)
            parseLine(line);
        else
            addSegment("", XML_FORMAT_TEXT);
        return segmentList;
    }

    private void parseLine(Line line)
    {
        final String text = getDetabbedText(line);
        int state = line.flags();
        sb.setLength(0);
        int i = 0;
        final int limit = text.length();
        while (i < limit) {
            char c = text.charAt(i);
            if (state == STATE_COMMENT) {
                if (i < limit-2 && text.substring(i, i+3).equals("-->")) {
                    sb.append("-->");
                    endToken(state);
                    state = STATE_NEUTRAL;
                    i += 3;
                } else {
                    sb.append(c);
                    ++i;
                }
                continue;
            }
            if (state == STATE_CDATA) {
                if (c == ']') {
                    if (text.regionMatches(i, "]]>", 0, 3)) {
                        endToken(state);
                        sb.append("]]");
                        endToken(STATE_TAG);
                        sb.append('>');
                        endToken(STATE_TAG_ENDING);
                        state = STATE_NEUTRAL;
                        i += 3;
                        continue;
                    }
                }
                sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_TAG_STARTING) {
                if (c == '/' || c == '?') {
                    sb.append(c);
                    endToken(state);
                    state = STATE_NAMESPACE;
                    ++i;
                    continue;
                }
                if (c == '!') {
                    if (text.regionMatches(i, "![CDATA[", 0, 8)) {
                        sb.append(c);
                        endToken(state);
                        sb.append("[CDATA[");
                        endToken(STATE_TAG);
                        state = STATE_CDATA;
                        i += 8;
                        continue;
                    }
                    if (text.regionMatches(i, "!DOCTYPE", 0, 8)) {
                        sb.append(c);
                        endToken(state);
                        sb.append("DOCTYPE");
                        endToken(STATE_TAG);
                        state = STATE_NEUTRAL;
                        i += 8;
                        continue;
                    }
                    sb.append(c);
                    endToken(state);
                    state = STATE_TAG;
                    ++i;
                    continue;
                }
                endToken(state);
                state = STATE_NAMESPACE;
                sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_NAMESPACE) {
                if (c == '/' && text.regionMatches(i, "/>", 0, 2)) {
                    // It wasn't really a namespace.
                    endToken(STATE_TAG);
                    state = STATE_TAG_ENDING;
                    sb.append("/>");
                    endToken(state);
                    state = STATE_NEUTRAL;
                    i += 2;
                    continue;
                }
                if (c == '?' && text.regionMatches(i, "?>", 0, 2)) {
                    // Processing instruction.
                    endToken(STATE_TAG);
                    state = STATE_TAG_ENDING;
                    sb.append("?>");
                    endToken(state);
                    state = STATE_NEUTRAL;
                    i += 2;
                    continue;
                }
                if (c == ':') {
                    sb.append(c);
                    endToken(state);
                    state = STATE_TAG;
                } else if (isWhitespace(c)) {
                    // It wasn't really a namespace.
                    endToken(STATE_TAG);
                    state = STATE_ATTRIBUTE;
                    sb.append(c);
                } else if (c == '>') {
                    // It wasn't really a namespace.
                    endToken(STATE_TAG);
                    state = STATE_TAG_ENDING;
                    sb.append(c);
                    endToken(state);
                    state = STATE_NEUTRAL;
                } else
                    sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_TAG) {
                if (c == '/' && text.regionMatches(i, "/>", 0, 2)) {
                    endToken(state);
                    state = STATE_TAG_ENDING;
                    sb.append("/>");
                    endToken(state);
                    state = STATE_NEUTRAL;
                    i += 2;
                    continue;
                }
                if (c == '?' && text.regionMatches(i, "?>", 0, 2)) {
                    // Processing instruction.
                    endToken(STATE_TAG);
                    state = STATE_TAG_ENDING;
                    sb.append("?>");
                    endToken(state);
                    state = STATE_NEUTRAL;
                    i += 2;
                    continue;
                }
                if (isWhitespace(c)) {
                    endToken(state);
                    state = STATE_ATTRIBUTE;
                    sb.append(c);
                } else if (c == '>') {
                    endToken(state);
                    state = STATE_TAG_ENDING;
                    sb.append(c);
                    endToken(state);
                    state = STATE_NEUTRAL;
                } else
                    sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_ATTRIBUTE) {
                if (c == '/' && text.regionMatches(i, "/>", 0, 2)) {
                    endToken(state);
                    state = STATE_TAG_ENDING;
                    sb.append("/>");
                    endToken(state);
                    state = STATE_NEUTRAL;
                    i += 2;
                    continue;
                }
                if (c == '?' && text.regionMatches(i, "?>", 0, 2)) {
                    // Processing instruction.
                    endToken(state);
                    state = STATE_TAG_ENDING;
                    sb.append("?>");
                    endToken(state);
                    state = STATE_NEUTRAL;
                    i += 2;
                    continue;
                }
                if (c == '>') {
                    endToken(state);
                    state = STATE_TAG_ENDING;
                    sb.append(c);
                    endToken(state);
                    state = STATE_NEUTRAL;
                    ++i;
                    continue;
                }
                if (c == '=') {
                    endToken(state);
                    state = STATE_EQUALS;
                    sb.append(c);
                    endToken(state);
                    state = STATE_ATTRIBUTE;
                    ++i;
                    continue;
                }
                if (c == '"') {
                    endToken(state);
                    state = STATE_QUOTE;
                    sb.append(c);
                    ++i;
                    continue;
                }
                if (c == '\'') {
                    endToken(state);
                    state = STATE_SINGLEQUOTE;
                    sb.append(c);
                    ++i;
                    continue;
                }
                sb.append(c);
                ++i;
                continue;
            }
            if (state == STATE_QUOTE) {
                sb.append(c);
                if (c == '"') {
                    endToken(state);
                    state = STATE_ATTRIBUTE;
                }
                ++i;
                continue;
            }
            if (state == STATE_SINGLEQUOTE) {
                sb.append(c);
                if (c == '\'') {
                    endToken(state);
                    state = STATE_ATTRIBUTE;
                }
                ++i;
                continue;
            }
            // Not in comment or tag.
            if (c == '<') {
                endToken(state);
                if (text.regionMatches(i, "<!--", 0, 4)) {
                    state = STATE_COMMENT;
                    sb.append("<!--");
                    i += 4;
                    continue;
                }
                state = STATE_TAG_STARTING;
                sb.append(c);
            } else
                sb.append(c);
            ++i;
        }
        // Reached end of line.
        if (state == STATE_NAMESPACE)
            // It wasn't really a namespace.
            endToken(STATE_TAG);
        else
            endToken(state);
    }

    public boolean parseBuffer()
    {
        int state = STATE_NEUTRAL;
        Line line = buffer.getFirstLine();
        Position pos = new Position(line, 0);
        boolean changed = false;
        while (line != null) {
            int oldflags = line.flags();
            if (state != oldflags) {
                line.setFlags(state);
                changed = true;
            }
            final int limit = line.length();
            for (int i = 0; i < limit; i++) {
                char c = line.charAt(i);
                if (state == STATE_COMMENT) {
                    if (c == '-') {
                        pos.moveTo(line, i);
                        if (pos.lookingAt("-->")) {
                            state = STATE_NEUTRAL;
                            i += 2;
                            continue;
                        }
                    }
                    continue;
                }
                if (state == STATE_CDATA) {
                    if (c == ']') {
                        pos.moveTo(line, i);
                        if (pos.lookingAt("]]>")) {
                            state = STATE_NEUTRAL;
                            i += 2;
                            continue;
                        }
                    }
                    continue;
                }
                if (state == STATE_TAG) {
                    if (!isWhitespace(c)) {
                        // OK, we shouldn't really be in STATE_ATTRIBUTE just
                        // because we've seen one non-whitespace character
                        // after the opening '<'. But if the line ends before
                        // the '>', we don't want the next line to start in
                        // STATE_TAG.
                        state = STATE_ATTRIBUTE;
                        continue;
                    }
                }
                if (state == STATE_ATTRIBUTE) {
                    if (c == '>')
                        state = STATE_NEUTRAL;
                    else if (c == '"')
                        state = STATE_QUOTE;
                    else if (c == '\'')
                        state = STATE_SINGLEQUOTE;
                    continue;
                }
                if (state == STATE_QUOTE) {
                    if (c == '"')
                        state = STATE_ATTRIBUTE;
                    continue;
                }
                if (state == STATE_SINGLEQUOTE) {
                    if (c == '\'')
                        state = STATE_ATTRIBUTE;
                    continue;
                }
                // Neutral state.
                if (c == '<') {
                    pos.moveTo(line, i);
                    if (pos.lookingAt("<!--")) {
                        state = STATE_COMMENT;
                        i += 3;
                        continue;
                    }
                    if (pos.lookingAt("<![CDATA[")) {
                        state = STATE_CDATA;
                        i += 8;
                        continue;
                    }
                    if (pos.lookingAt("<!DOCTYPE")) {
                        // There is no STATE_DOCTYPE...
                        state = STATE_NEUTRAL;
                        i += 8;
                        continue;
                    }
                    state = STATE_TAG;
                    continue;
                }
            }
            line = line.next();
        }
        buffer.setNeedsParsing(false);
        return changed;
    }

    private static final boolean isWhitespace(char c)
    {
        return c <= ' ';
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("XmlMode");
            formatTable.addEntryFromPrefs(XML_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(XML_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(XML_FORMAT_DELIMITER, "delimiter");
            formatTable.addEntryFromPrefs(XML_FORMAT_NAMESPACE, "namespace");
            formatTable.addEntryFromPrefs(XML_FORMAT_TAG, "tag");
            formatTable.addEntryFromPrefs(XML_FORMAT_ATTRIBUTE, "attribute");
            formatTable.addEntryFromPrefs(XML_FORMAT_EQUALS, "equals", "delimiter");
            formatTable.addEntryFromPrefs(XML_FORMAT_QUOTE, "string");
        }
        return formatTable;
    }
}
