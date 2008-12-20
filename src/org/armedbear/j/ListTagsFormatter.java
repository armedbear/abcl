/*
 * ListTagsFormatter.java
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

public final class ListTagsFormatter extends Formatter
{
    private static final byte FORMAT_TEXT          = 0;
    private static final byte FORMAT_HEADER_NAME   = 1;
    private static final byte FORMAT_HEADER_VALUE  = 2;
    private static final byte FORMAT_MATCHING_TEXT = 3;
    private static final byte FORMAT_STATUS        = 4;
    private static final byte FORMAT_VISITED       = 5;

    public ListTagsFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line instanceof TagLine)
            return formatTagLine((TagLine)line);
        else if (line instanceof FileLine)
            return formatFileLine((FileLine)line);
        else
            return formatHeaderLine(line);
    }

    private LineSegmentList formatHeaderLine(Line line)
    {
        final String text = getDetabbedText(line);
        int index = text.indexOf(':');
        if (index > 0) {
            addSegment(text, 0, index+1, FORMAT_HEADER_NAME);
            addSegment(text, index+1, FORMAT_HEADER_VALUE);
            return segmentList;
        }
        addSegment(text, FORMAT_TEXT);
        return segmentList;
    }

    private LineSegmentList formatTagLine(TagLine line)
    {
        final String text = getDetabbedText(line);
        final String name = line.getTag().getMethodName();
        if (name != null) {
            int index = text.indexOf(name);
            if (index >= 0) {
                if (index > 0)
                    addSegment(text, 0, index, FORMAT_TEXT);
                addSegment(text, index, index + name.length(), FORMAT_MATCHING_TEXT);
                if (index + name.length() < text.length())
                    addSegment(text, index + name.length(), FORMAT_TEXT);
                return segmentList;
            }
        }
        addSegment(text, FORMAT_TEXT);
        return segmentList;
    }

    private LineSegmentList formatFileLine(FileLine line)
    {
        final String text = getDetabbedText(line);
        int index = text.indexOf(':');
        if (index > 0) {
            addSegment(text, 0, index+1, FORMAT_HEADER_NAME);
            addSegment(text, index+1, FORMAT_HEADER_VALUE);
        } else
            addSegment(text, line.visited() ? FORMAT_VISITED : FORMAT_TEXT);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("ListTagsMode");
            formatTable.addEntryFromPrefs(FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(FORMAT_HEADER_NAME, "headerName", "keyword");
            formatTable.addEntryFromPrefs(FORMAT_HEADER_VALUE, "headerValue", "operator");
            formatTable.addEntryFromPrefs(FORMAT_MATCHING_TEXT, "matchingText", "function");
            formatTable.addEntryFromPrefs(FORMAT_STATUS, "status", "comment");
            formatTable.addEntryFromPrefs(FORMAT_VISITED, "visited", "disabled");
        }
        return formatTable;
    }
}
