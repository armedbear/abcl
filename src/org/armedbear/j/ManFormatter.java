/*
 * ManFormatter.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

public class ManFormatter extends Formatter
{
    // States.
    private static final int STATE_BOLD      = STATE_LAST + 1;
    private static final int STATE_UNDERLINE = STATE_LAST + 2;

    // Formats.
    private static final int MAN_FORMAT_PLAIN     = 0;
    private static final int MAN_FORMAT_BOLD      = 1;
    private static final int MAN_FORMAT_UNDERLINE = 2;

    private final FastStringBuffer sb = new FastStringBuffer();
    private final boolean apropos;

    public ManFormatter(Buffer buffer, boolean apropos)
    {
        this.buffer = buffer;
        this.apropos = apropos;
    }

    private void endToken(int state)
    {
        if (sb.length() > 0) {
            int format;
            switch (state) {
                case STATE_NEUTRAL:
                    format = MAN_FORMAT_PLAIN;
                    break;
                case STATE_BOLD:
                    format = MAN_FORMAT_BOLD;
                    break;
                case STATE_UNDERLINE:
                    format = MAN_FORMAT_UNDERLINE;
                    break;
                default:
                    format = MAN_FORMAT_PLAIN;
                    break;
            }
            addSegment(sb.toString(), format);
            sb.setLength(0);
        }
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null || line.length() == 0) {
            addSegment("", MAN_FORMAT_PLAIN);
            return segmentList;
        }
        if (apropos)
            parseLineApropos(line);
        else
            parseLine(line);
        return segmentList;
    }

    private void parseLineApropos(Line line)
    {
        String text = line.getText();
        int index = text.indexOf(' ');
        if (index >= 0) {
            addSegment(text, 0, index, MAN_FORMAT_BOLD);
            addSegment(text, index, MAN_FORMAT_PLAIN);
        } else
            addSegment(text, MAN_FORMAT_PLAIN);
    }

    private void parseLine(Line line)
    {
        final String text = ((ManLine)line).getRawText();
        final int limit = text.length();
        final int tabWidth = buffer.getTabWidth();
        int state = STATE_NEUTRAL;
        for (int i = 0, length = 0; i < limit; i++) {
            final char c = text.charAt(i);
            final char nextChar = i+1 < limit ? text.charAt(i+1) : 0;
            switch (state) {
                case STATE_NEUTRAL:
                    if (nextChar == '\b') {
                        final char thirdChar = i+2 < limit ? text.charAt(i+2) : 0;
                        if (thirdChar == c) {
                            // Bold.
                            endToken(state);
                            i += 2;
                            state = STATE_BOLD;
                            sb.append(c);
                            length++;
                            continue;
                        } else if (c == '_') {
                            // Underline.
                            endToken(state);
                            i += 2;
                            state = STATE_UNDERLINE;
                            sb.append(thirdChar);
                            length++;
                            continue;
                        } else {
                            ++i;
                            continue;
                        }
                    } else {
                        if (c == '\t') {
                            for (int j = tabWidth - length % tabWidth; j-- > 0; length++)
                                sb.append(" ");
                        } else {
                            sb.append(c);
                            length++;
                        }
                    }
                    break;
                case STATE_BOLD:
                    if (c == '\b') {
                        char prevChar = text.charAt(i-1);
                        if (nextChar == prevChar)
                            ++i;
                        continue;
                    } else if (nextChar == '\b') {
                        final char thirdChar = i+2 < limit ? text.charAt(i+2) : 0;
                        if (thirdChar == c) {
                            // Bold.
                            i += 2;
                            sb.append(c);
                            length++;
                            continue;
                        }
                        if (c == '_') {
                            // Underline.
                            endToken(state);
                            i += 2;
                            state = STATE_UNDERLINE;
                            sb.append(thirdChar);
                            length++;
                            continue;
                        }
                    } else {
                        endToken(state);
                        state = STATE_NEUTRAL;
                        if (c == '\t') {
                            for (int j = tabWidth - length % tabWidth; j-- > 0; length++)
                                sb.append(" ");
                        } else {
                            sb.append(c);
                            length++;
                        }
                    }
                    break;
                case STATE_UNDERLINE:
                    if (c == '_' && nextChar == '\b') {
                        final char thirdChar = i+2 < limit ? text.charAt(i+2) : 0;
                        if (thirdChar != 0) {
                            sb.append(thirdChar);
                            length++;
                        }
                        i += 2;
                    } else if (nextChar == '\b') {
                        endToken(state);
                        i += 2;
                        state = STATE_BOLD;
                        sb.append(c);
                        length++;
                        continue;
                    } else {
                        endToken(state);
                        state = STATE_NEUTRAL;
                        if (c == '\t') {
                            for (int j = tabWidth - length % tabWidth; j-- > 0; length++)
                                sb.append(" ");
                        } else {
                            sb.append(c);
                            length++;
                        }
                    }
                    break;
            }
        }
        endToken(state);
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("ManMode");
            formatTable.addEntryFromPrefs(MAN_FORMAT_PLAIN, "text");
            formatTable.addEntryFromPrefs(MAN_FORMAT_BOLD, "function");
            formatTable.addEntryFromPrefs(MAN_FORMAT_UNDERLINE, "keyword");
        }
        return formatTable;
    }
}
