/*
 * AsmFormatter.java
 *
 * Copyright (C) 2003 Peter Graves
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;

public final class AsmFormatter extends Formatter
{
    private static final UncheckedRE labelRE = new UncheckedRE("^[_a-zA-z0-9]+:");

    private static final int ASM_FORMAT_TEXT    = 0;
    private static final int ASM_FORMAT_COMMENT = 1;
    private static final int ASM_FORMAT_LABEL   = 2;

    public AsmFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        final String text = getDetabbedText(line);
        if (text.length() > 0) {
            int start = 0;
            int index = text.indexOf(':');
            if (index > 0) {
                REMatch match = labelRE.getMatch(text);
                if (match != null) {
                    index = match.getEndIndex();
                    addSegment(text, 0, index, ASM_FORMAT_LABEL);
                    start = index;
                }
            }
            index = text.indexOf(';', start);
            if (index >= 0) {
                addSegment(text, start, index, ASM_FORMAT_TEXT);
                addSegment(text, index, ASM_FORMAT_COMMENT);
            } else
                addSegment(text, start, ASM_FORMAT_TEXT);
        } else
            addSegment(text, ASM_FORMAT_TEXT);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable(null);
            formatTable.addEntryFromPrefs(ASM_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(ASM_FORMAT_COMMENT, "comment");
            formatTable.addEntryFromPrefs(ASM_FORMAT_LABEL, "function");
        }
        return formatTable;
    }
}
