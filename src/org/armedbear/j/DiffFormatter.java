/*
 * DiffFormatter.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

public final class DiffFormatter extends Formatter
{
    // Formats.
    private static final int DIFF_FORMAT_TEXT     = 0;
    private static final int DIFF_FORMAT_FILE     = 1;
    private static final int DIFF_FORMAT_HEADER   = 2;
    private static final int DIFF_FORMAT_CONTEXT  = 3;
    private static final int DIFF_FORMAT_INSERTED = 4;
    private static final int DIFF_FORMAT_DELETED  = 5;
    private static final int DIFF_FORMAT_CHANGED  = 6;

    public static final int DIFF_FORMAT_LAST = 6;

    public DiffFormatter(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null || line.length() == 0) {
            addSegment("", DIFF_FORMAT_TEXT);
            return segmentList;
        }
        final String text = getDetabbedText(line);
        char c = text.charAt(0);
        if (c == '+' && !text.startsWith("+++ ")) {
            // Inserted line.
            addSegment(text, DIFF_FORMAT_INSERTED);
            return segmentList;
        }
        if (c == '>') {
            // Inserted line.
            addSegment(text, DIFF_FORMAT_INSERTED);
            return segmentList;
        }
        if (c == '-' && !text.equals("---") && !text.startsWith("--- ")) {
            // Deleted line.
            addSegment(text, DIFF_FORMAT_DELETED);
            return segmentList;
        }
        if (c == '<') {
            // Deleted line.
            addSegment(text, DIFF_FORMAT_DELETED);
            return segmentList;
        }
        if (c == '!') {
            // Changed line (diff -c).
            addSegment(text, DIFF_FORMAT_CHANGED);
            return segmentList;
        }
        if (c == '?' || text.startsWith("Index: ")) {
            // File line.
            addSegment(text, DIFF_FORMAT_FILE);
            return segmentList;
        }
        if (text.startsWith("==== ") && text.endsWith(" ====")) {
            // File line (p4 diff).
            addSegment(text, DIFF_FORMAT_FILE);
            return segmentList;
        }
        if (isDiffHeader(text)) {
            addSegment(text, DIFF_FORMAT_HEADER);
            return segmentList;
        }
        if (c == ' ' && buffer instanceof org.armedbear.j.mail.MessageBuffer) {
            // Diff might be indented.
            if (isDiffHeader(text.trim())) {
                addSegment(text, DIFF_FORMAT_HEADER);
                return segmentList;
            }
            int indent = -1;
            for (Line ln = line.previous(); ln != null; ln = ln.previous()) {
                String trim = ln.trim();
                if (trim.startsWith("@@ ") && trim.endsWith(" @@")) {
                    indent = getDetabbedText(ln).indexOf('@');
                    break;
                }
                if (trim.startsWith("+++ ")) {
                    indent = getDetabbedText(ln).indexOf('+');
                    break;
                }
            }
            if (indent >= 0) {
                String s = text.substring(indent);
                if (s.length() == 0) {
                    addSegment("", DIFF_FORMAT_TEXT);
                    return segmentList;
                }
                c = s.charAt(0);
                if (c == '+' && !s.startsWith("+++ ")) {
                    // Inserted line.
                    addSegment(text, DIFF_FORMAT_INSERTED);
                    return segmentList;
                }
                if (c == '-' && !s.startsWith("--- ")) {
                    // Deleted line.
                    addSegment(text, DIFF_FORMAT_DELETED);
                    return segmentList;
                }
                if (c == '!') {
                    // Changed line (diff -c).
                    addSegment(s, DIFF_FORMAT_CHANGED);
                    return segmentList;
                }
                if (c == '?' || s.startsWith("Index: ")) {
                    // File line.
                    addSegment(text, DIFF_FORMAT_FILE);
                    return segmentList;
                }
                if (s.startsWith("==== ") && s.endsWith(" ====")) {
                    // File line (p4 diff).
                    addSegment(text, DIFF_FORMAT_FILE);
                    return segmentList;
                }
            }
        }
        // Otherwise, assume it's a context line.
        addSegment(text, DIFF_FORMAT_CONTEXT);
        return segmentList;
    }

    private static boolean isDiffHeader(String s)
    {
        if ((s.startsWith("cvs server: ") ||
            s.startsWith("========") ||
            s.startsWith("RCS file: ") ||
            s.startsWith("retrieving revision ") ||
            s.startsWith("diff ") ||
            s.startsWith("*** ") ||
            s.startsWith("--- ") ||
            s.startsWith("+++ ") ||
            (s.startsWith("@@ ") || s.endsWith(" @@")) ||
            (s.startsWith("@ ") && s.endsWith(" @"))))
            return true;
        if (s.equals("***************"))
            return true;
        if (s.equals("---"))
            return true;
        if (s.length() > 0) {
            char c = s.charAt(0);
            if (c >= '0' && c <= '9')
                return true;
        }
        return false;
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("DiffMode");
            formatTable.addEntryFromPrefs(DIFF_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(DIFF_FORMAT_FILE, "file", "text");
            formatTable.addEntryFromPrefs(DIFF_FORMAT_HEADER, "header", "comment");
            formatTable.addEntryFromPrefs(DIFF_FORMAT_CONTEXT, "context", "text");
            formatTable.addEntryFromPrefs(DIFF_FORMAT_INSERTED, "inserted", "text");
            formatTable.addEntryFromPrefs(DIFF_FORMAT_DELETED, "deleted", "text");
            formatTable.addEntryFromPrefs(DIFF_FORMAT_CHANGED, "changed", "text");
        }
        return formatTable;
    }
}
