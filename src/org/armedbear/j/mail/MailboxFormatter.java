/*
 * MailboxFormatter.java
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

package org.armedbear.j.mail;

import org.armedbear.j.Formatter;
import org.armedbear.j.Buffer;
import org.armedbear.j.Editor;
import org.armedbear.j.Line;
import org.armedbear.j.LineSegment;
import org.armedbear.j.LineSegmentList;
import org.armedbear.j.Property;
import org.armedbear.j.Utilities;
import org.armedbear.j.FormatTable;

public final class MailboxFormatter extends Formatter
{
    private static final boolean SHOW_MESSAGE_NUMBERS =
        Editor.preferences().getBooleanProperty(Property.SHOW_MESSAGE_NUMBERS);

    private static final byte FORMAT_TEXT            =  0;
    private static final byte FORMAT_DELETED         =  1;
    private static final byte FORMAT_TAGGED          =  2;

    private static final byte FORMAT_TO              =  3; // '+', 'T', 'C', 'F', '!'
    private static final byte FORMAT_FLAGS           =  4;
    private static final byte FORMAT_DATE            =  5;
    private static final byte FORMAT_FROM            =  6;
    private static final byte FORMAT_SIZE            =  7;
    private static final byte FORMAT_SUBJECT         =  8;

    private static final byte FORMAT_FLAGGED_TO      =  9;
    private static final byte FORMAT_FLAGGED_FLAGS   = 10;
    private static final byte FORMAT_FLAGGED_DATE    = 11;
    private static final byte FORMAT_FLAGGED_FROM    = 12;
    private static final byte FORMAT_FLAGGED_SIZE    = 13;
    private static final byte FORMAT_FLAGGED_SUBJECT = 14;

    private static final int TO_COLUMN_WIDTH    =  2;
    private static final int FLAGS_COLUMN_WIDTH =  3;
    private static final int DATE_COLUMN_WIDTH  = 14;
    private static final int FROM_COLUMN_WIDTH  = 22;
    private static final int SIZE_COLUMN_WIDTH  =  6;

    private static final int TO_COLUMN      = SHOW_MESSAGE_NUMBERS ? 6 : 0;
    private static final int FLAGS_COLUMN   = TO_COLUMN + TO_COLUMN_WIDTH;
    private static final int DATE_COLUMN    = FLAGS_COLUMN + FLAGS_COLUMN_WIDTH;
    private static final int FROM_COLUMN    = DATE_COLUMN + DATE_COLUMN_WIDTH;
    private static final int SIZE_COLUMN    = FROM_COLUMN + FROM_COLUMN_WIDTH;
    private static final int SUBJECT_COLUMN = SIZE_COLUMN + SIZE_COLUMN_WIDTH;

    public MailboxFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        String text;
        if (Editor.tabsAreVisible())
            text = Utilities.makeTabsVisible(line.getText(), buffer.getTabWidth());
        else
            text = Utilities.detab(line.getText(), buffer.getTabWidth());
        clearSegmentList();
        if (line instanceof MailboxLine) {
            MailboxEntry entry = ((MailboxLine) line).getMailboxEntry();
            if (entry.isTagged()) {
                addSegment(text, FORMAT_TAGGED);
            } else if (entry.isDeleted()) {
                addSegment(text, FORMAT_DELETED);
            } else if (entry.isFlagged()) {
                if (TO_COLUMN > 0)
                    addSegment(text, 0, TO_COLUMN, FORMAT_TEXT);
                addSegment(text, TO_COLUMN, FLAGS_COLUMN, FORMAT_FLAGGED_TO);
                addSegment(text, FLAGS_COLUMN, DATE_COLUMN, FORMAT_FLAGGED_FLAGS);
                addSegment(text, DATE_COLUMN, FROM_COLUMN, FORMAT_FLAGGED_DATE);
                addSegment(text, FROM_COLUMN, SIZE_COLUMN, FORMAT_FLAGGED_FROM);
                addSegment(text, SIZE_COLUMN, SUBJECT_COLUMN, FORMAT_FLAGGED_SIZE);
                addSegment(text, SUBJECT_COLUMN, FORMAT_FLAGGED_SUBJECT);
            } else {
                // Not flagged.
                if (TO_COLUMN > 0)
                    addSegment(text, 0, TO_COLUMN, FORMAT_TEXT);
                addSegment(text, TO_COLUMN, FLAGS_COLUMN, FORMAT_TO);
                addSegment(text, FLAGS_COLUMN, DATE_COLUMN, FORMAT_FLAGS);
                addSegment(text, DATE_COLUMN, FROM_COLUMN, FORMAT_DATE);
                addSegment(text, FROM_COLUMN, SIZE_COLUMN, FORMAT_FROM);
                addSegment(text, SIZE_COLUMN, SUBJECT_COLUMN, FORMAT_SIZE);
                addSegment(text, SUBJECT_COLUMN, FORMAT_SUBJECT);
            }
        } else
            addSegment(text, FORMAT_TEXT);
        return segmentList;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("MailboxMode");
            formatTable.addEntryFromPrefs(FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(FORMAT_DELETED, "deleted", "disabled");
            formatTable.addEntryFromPrefs(FORMAT_TAGGED, "marked");
            formatTable.addEntryFromPrefs(FORMAT_TO, "to", "function");
            formatTable.addEntryFromPrefs(FORMAT_FLAGS, "flags", "text");
            formatTable.addEntryFromPrefs(FORMAT_DATE, "date", "keyword");
            formatTable.addEntryFromPrefs(FORMAT_FROM, "from", "function");
            formatTable.addEntryFromPrefs(FORMAT_SIZE, "size", "text");
            formatTable.addEntryFromPrefs(FORMAT_SUBJECT, "subject", "string");
            formatTable.addEntryFromPrefs(FORMAT_FLAGGED_TO, "flaggedTo", "to");
            formatTable.addEntryFromPrefs(FORMAT_FLAGGED_FLAGS, "flaggedFlags", "flags");
            formatTable.addEntryFromPrefs(FORMAT_FLAGGED_DATE, "flaggedDate", "date");
            formatTable.addEntryFromPrefs(FORMAT_FLAGGED_FROM, "flaggedFrom", "from");
            formatTable.addEntryFromPrefs(FORMAT_FLAGGED_SIZE, "flaggedSize", "size");
            formatTable.addEntryFromPrefs(FORMAT_FLAGGED_SUBJECT, "flaggedSubject", "subject");
        }
        return formatTable;
    }
}
