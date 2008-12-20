/*
 * WebFormatter.java
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

import gnu.regexp.RE;
import gnu.regexp.UncheckedRE;
import java.awt.Color;
import java.awt.Font;

public final class WebFormatter extends Formatter implements WebConstants
{
    // Includes '/' for "Parts/Attachments".
    private static final RE headerRE = new UncheckedRE("^ *[a-zA-Z\\-/]+:");

    public WebFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public final LineSegmentList formatLine(Line line)
    {
        if (line instanceof WebLine) {
            LineSegmentList list = ((WebLine)line).getSegmentList();
            if (list == null) {
                list = new LineSegmentList();
                list.addSegment(new HtmlLineSegment("", 0));
            }
            return list;
        }
        clearSegmentList();
        final String text = getDetabbedText(line);
        if (line instanceof MessageHeaderLine) {
            if (text.length() > 0) {
                int i = text.indexOf(':');
                if (i >= 0 && headerRE.getMatch(text) != null) {
                    addSegment(text, 0, i+1, FORMAT_HEADER_NAME);
                    addSegment(text, i+1, FORMAT_HEADER_VALUE);
                    return segmentList;
                }
            }
            // Continuation line.
            addSegment(text, FORMAT_HEADER_VALUE);
        } else
            addSegment(text, FORMAT_TEXT);
        return segmentList;
    }

    public final Color getColor(int format)
    {
        return super.getColor(format & ~(FORMAT_BOLD | FORMAT_ITALIC));
    }

    public int getStyle(int format)
    {
        FormatTableEntry entry =
            getFormatTable().lookup(format & ~(FORMAT_BOLD | FORMAT_ITALIC));
        int style = entry != null ? entry.getStyle() : Font.PLAIN;
        if ((format & FORMAT_BOLD) != 0)
            style |= Font.BOLD;
        else if ((format & FORMAT_ITALIC) != 0)
            style |= Font.ITALIC;
        return style;
    }

    public final FormatTableEntry getFormatTableEntry(int format)
    {
        return null;
    }

    public final boolean getUnderline(int format)
    {
        if ((format & FORMAT_WHITESPACE) != 0)
            return false;
        else
            return (format & FORMAT_LINK) != 0;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("WebMode");
            formatTable.addEntryFromPrefs(FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(FORMAT_LINK, "link", "keyword");
            formatTable.addEntryFromPrefs(FORMAT_WHITESPACE, "text");
            formatTable.addEntryFromPrefs(FORMAT_DISABLED, "disabled");
            formatTable.addEntryFromPrefs(FORMAT_HEADER_NAME, "headerName", "keyword");
            formatTable.addEntryFromPrefs(FORMAT_HEADER_VALUE, "headerValue", "operator");
        }
        return formatTable;
    }
}
