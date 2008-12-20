/*
 * CheckPathFormatter.java
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

public final class CheckPathFormatter extends Formatter
{
    private static final byte FORMAT_TEXT         = 0;
    private static final byte FORMAT_COMMENT      = 1;
    private static final byte FORMAT_HEADER_NAME  = 2;
    private static final byte FORMAT_HEADER_VALUE = 4;
    private static final byte FORMAT_NOT_FOUND    = 5;
    private static final byte FORMAT_STATUS       = 6;

    public CheckPathFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null) {
            addSegment("", FORMAT_TEXT);
            return segmentList;
        }
        parseLine(line);
        return segmentList;

    }

    private void parseLine(Line line)
    {
        final String text = getDetabbedText(line);
        if (text.endsWith(" -->")) {
            addSegment(text, FORMAT_COMMENT);
            return;
        }
        String trim = text.trim();
        char c;
        if (trim.length() > 0 && ((c = trim.charAt(0)) == '"' || c == '<')) {
            if (text.endsWith("  NOT FOUND")) {
                int index = text.length() - 11;
                addSegment(text, 0, index, FORMAT_TEXT);
                addSegment(text, index, FORMAT_NOT_FOUND);
            } else if (text.endsWith("  (Already listed)")) {
                int index = text.length() - 16;
                addSegment(text, 0, index, FORMAT_TEXT);
                addSegment(text, index, FORMAT_STATUS);
            } else
                addSegment(text, FORMAT_TEXT);
            return;
        }
        int index = text.indexOf(':');
        if (index > 0) {
            addSegment(text, 0, index+1, FORMAT_HEADER_NAME);
            addSegment(text, index+1, FORMAT_HEADER_VALUE);
            return;
        }
        // Default.
        addSegment(text, FORMAT_TEXT);
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            // Currently there's no CheckPathMode...
            formatTable = new FormatTable("ListOccurrencesMode");
            formatTable.addEntryFromPrefs(FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(FORMAT_HEADER_NAME, "headerName", "keyword");
            formatTable.addEntryFromPrefs(FORMAT_HEADER_VALUE, "headerValue", "operator");
            formatTable.addEntryFromPrefs(FORMAT_NOT_FOUND, "matchingText", "function");
            formatTable.addEntryFromPrefs(FORMAT_STATUS, "status", "comment");
        }
        return formatTable;
    }
}
