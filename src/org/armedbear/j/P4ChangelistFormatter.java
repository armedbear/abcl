/*
 * P4ChangelistFormatter.java
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

public final class P4ChangelistFormatter extends Formatter
{
    private static final byte FORMAT_TEXT      = 0;
    private static final byte FORMAT_COMMENT   = 1;
    private static final byte FORMAT_KEY       = 2;
    private static final byte FORMAT_VALUE     = 3;

    public P4ChangelistFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        String text = getDetabbedText(line);
        final String comment;
        int hash = text.indexOf('#');
        if (hash >= 0) {
            comment = text.substring(hash);
            text = text.substring(0, hash);
        } else
            comment = null;
        int colon = text.indexOf(':');
        if (colon >= 0) {
            int space = text.indexOf('\t');
            if (space < 0)
                space = text.indexOf(' ');
            if (space < 0 || colon < space) {
                addSegment(text, 0, colon+1, FORMAT_KEY);
                addSegment(text, colon+1, FORMAT_VALUE);
            } else
                addSegment(text, FORMAT_TEXT);
        } else
            addSegment(text, FORMAT_TEXT);
        if (comment != null)
            addSegment(comment, FORMAT_COMMENT);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("P4Changelist");
            formatTable.addEntryFromPrefs(FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(FORMAT_KEY, "key", "function");
            formatTable.addEntryFromPrefs(FORMAT_VALUE, "value", "text");
        }
        return formatTable;
    }
}
