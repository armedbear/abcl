/*
 * ListOccurrencesFormatter.java
 *
 * Copyright (C) 2000-2003 Peter Graves
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

public final class ListOccurrencesFormatter extends Formatter
{
    private static final byte FORMAT_TEXT          = 0;
    private static final byte FORMAT_COMMENT       = 1;
    private static final byte FORMAT_HEADER_NAME   = 2;
    private static final byte FORMAT_HEADER_VALUE  = 4;
    private static final byte FORMAT_MATCHING_TEXT = 5;
    private static final byte FORMAT_STATUS        = 6;
    private static final byte FORMAT_VISITED       = 7;

    private Search search;
    private Mode parentMode;

    public ListOccurrencesFormatter(Buffer buffer)
    {
        this.buffer = buffer;
        search = (Search) ((ListOccurrences)buffer).getSearch().clone();
        parentMode = ((ListOccurrences)buffer).getParentMode();
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line instanceof OccurrenceLine)
            return formatOutputLine((OccurrenceLine)line);
        else if (line instanceof FileLine)
            return formatFileLine((FileLine)line);
        else if (line instanceof ListOccurrencesStatusLine)
            return formatStatusLine((ListOccurrencesStatusLine)line);
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

    private LineSegmentList formatOutputLine(OccurrenceLine line)
    {
        final String text = getDetabbedText(line);
        // Include the ':' in the first segment.
        int index = text.indexOf(':') + 1;
        addSegment(text, 0, index, FORMAT_COMMENT);
        Position start = new Position(line, index);
        int startCol = index;
        while (true) {
            Position pos = search.findInLine(parentMode, start);
            if (pos == null)
                break;
            int matchCol = buffer.getCol(pos);
            if (matchCol != startCol)
                addSegment(text, startCol, matchCol, FORMAT_TEXT);
            int length;
            if (search.getMatch() != null)
                length = search.getMatch().toString().length();
            else
                length = search.getPatternLength();
            startCol = buffer.getCol(pos.getLine(), pos.getOffset() + length);
            addSegment(text, matchCol, startCol, FORMAT_MATCHING_TEXT);
            start = new Position(pos.getLine(), pos.getOffset() + length);
        }
        startCol = buffer.getCol(start);
        addSegment(text, startCol, FORMAT_TEXT);
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

    private LineSegmentList formatStatusLine(ListOccurrencesStatusLine line)
    {
        final String text = getDetabbedText(line);
        addSegment(text, FORMAT_STATUS);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("ListOccurrencesMode");
            formatTable.addEntryFromPrefs(FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(FORMAT_HEADER_NAME, "headerName", "keyword");
            formatTable.addEntryFromPrefs(FORMAT_HEADER_VALUE, "headerValue", "operator");
            formatTable.addEntryFromPrefs(FORMAT_MATCHING_TEXT, "matchingText", "function");
            formatTable.addEntryFromPrefs(FORMAT_STATUS, "status", "comment");
            formatTable.addEntryFromPrefs(FORMAT_VISITED, "visited", "disabled");
        }
        return formatTable;
    }
}
