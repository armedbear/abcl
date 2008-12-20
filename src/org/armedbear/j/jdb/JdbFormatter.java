/*
 * JdbFormatter.java
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

package org.armedbear.j.jdb;

import org.armedbear.j.Buffer;
import org.armedbear.j.Editor;
import org.armedbear.j.FormatTable;
import org.armedbear.j.Formatter;
import org.armedbear.j.Line;
import org.armedbear.j.LineSegmentList;
import org.armedbear.j.Utilities;

public final class JdbFormatter extends Formatter
{
    private static final String prompt = Jdb.getPrompt();
    private static final int promptLength = prompt.length();

    // Formats.
    public static final byte JDB_FORMAT_TEXT   = 0;
    public static final byte JDB_FORMAT_PROMPT = 1;
    public static final byte JDB_FORMAT_INPUT  = 2;
    public static final byte JDB_FORMAT_OUTPUT = 3;
    public static final byte JDB_FORMAT_LOG    = 4;

    public JdbFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null) {
            addSegment("", JDB_FORMAT_TEXT);
            return segmentList;
        }
        int flags = line.flags();
        String text = getDetabbedText(line);
        if (flags == JDB_FORMAT_OUTPUT) {
            addSegment(text, JDB_FORMAT_OUTPUT);
        } else if (text.startsWith(prompt)) {
            addSegment(text, 0, promptLength, JDB_FORMAT_PROMPT);
            if (text.length() > promptLength)
                addSegment(text, promptLength, JDB_FORMAT_INPUT);
        } else
            addSegment(text, JDB_FORMAT_LOG);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("JdbMode");
            formatTable.addEntryFromPrefs(JDB_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(JDB_FORMAT_PROMPT, "prompt");
            formatTable.addEntryFromPrefs(JDB_FORMAT_INPUT, "input");
            formatTable.addEntryFromPrefs(JDB_FORMAT_OUTPUT, "output", "text");
            formatTable.addEntryFromPrefs(JDB_FORMAT_LOG, "log", "comment");
        }
        return formatTable;
    }
}
