/*
 * PropertiesFormatter.java
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

public final class PropertiesFormatter extends Formatter
{
    private static final byte PROPERTIES_FORMAT_TEXT      = 0;
    private static final byte PROPERTIES_FORMAT_COMMENT   = 1;
    private static final byte PROPERTIES_FORMAT_SECTION   = 2;
    private static final byte PROPERTIES_FORMAT_KEY       = 3;
    private static final byte PROPERTIES_FORMAT_VALUE     = 4;
    private static final byte PROPERTIES_FORMAT_DELIMITER = 5;

    public PropertiesFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        final String text = getDetabbedText(line);
        Line p = line.previous();
        if (p != null) {
            final int length = p.length();
            if (length > 0 && p.charAt(length-1) == '\\') {
                // Continuation line.
                addSegment(text, PROPERTIES_FORMAT_VALUE);
                return segmentList;
            }
        }
        if (text.length() > 0) {
            switch (text.charAt(0)) {
                case '#':
                case ';':
                case '!':
                    addSegment(text, PROPERTIES_FORMAT_COMMENT);
                    break;
                case '[':
                case '<':
                    addSegment(text, PROPERTIES_FORMAT_SECTION);
                    break;
                default:
                    int index = text.indexOf('=');
                    if (index < 0)
                        index = text.indexOf(':');
                    if (index >= 0) {
                        addSegment(text, 0, index, PROPERTIES_FORMAT_KEY);
                        addSegment(text, index, index+1, PROPERTIES_FORMAT_DELIMITER);
                        addSegment(text, index+1, PROPERTIES_FORMAT_VALUE);
                    } else
                        addSegment(text, PROPERTIES_FORMAT_TEXT);
                    break;
            }
        } else
            addSegment(text, PROPERTIES_FORMAT_TEXT);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("PropertiesMode");
            formatTable.addEntryFromPrefs(PROPERTIES_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(PROPERTIES_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(PROPERTIES_FORMAT_SECTION, "section");
            formatTable.addEntryFromPrefs(PROPERTIES_FORMAT_KEY, "key", "function");
            formatTable.addEntryFromPrefs(PROPERTIES_FORMAT_VALUE, "value", "text");
            formatTable.addEntryFromPrefs(PROPERTIES_FORMAT_DELIMITER, "delimiter", "operator");
        }
        return formatTable;
    }
}
