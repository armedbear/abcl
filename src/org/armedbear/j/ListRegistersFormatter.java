/*
 * ListRegistersFormatter.java
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

public final class ListRegistersFormatter extends Formatter
{
    private static final byte FORMAT_TEXT            = 0;
    private static final byte FORMAT_REGISTER_HEADER = 1;
    private static final byte FORMAT_REGISTER_NAME   = 2;
    private static final byte FORMAT_ELLIPSIS        = 3;

    public ListRegistersFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        if (line instanceof ListRegistersLine)
            return formatStatusLine((ListRegistersLine)line);
        return formatTextLine(line);
    }

    private LineSegmentList formatStatusLine(ListRegistersLine line)
    {
        clearSegmentList();
        final String text = getDetabbedText(line);
        if (text.startsWith("Register ")) {
            addSegment(text, 0, 9, FORMAT_REGISTER_HEADER);
            addSegment(text, 9, FORMAT_REGISTER_NAME);
        } else
            addSegment(text, FORMAT_ELLIPSIS);
        return segmentList;
    }

    private LineSegmentList formatTextLine(Line line)
    {
        clearSegmentList();
        final String text = getDetabbedText(line);
        addSegment(text, FORMAT_TEXT);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("ListRegistersMode");
            formatTable.addEntryFromPrefs(FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(FORMAT_REGISTER_HEADER, "registerHeader", "keyword");
            formatTable.addEntryFromPrefs(FORMAT_REGISTER_NAME, "registerName", "function");
            formatTable.addEntryFromPrefs(FORMAT_ELLIPSIS, "disabled");
        }
        return formatTable;
    }
}
