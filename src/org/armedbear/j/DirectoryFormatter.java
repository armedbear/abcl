/*
 * DirectoryFormatter.java
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

import gnu.regexp.REMatch;

public final class DirectoryFormatter extends Formatter
{
    // Formats.
    private static final byte DIRECTORY_FORMAT_TEXT      = 0;
    private static final byte DIRECTORY_FORMAT_DIRECTORY = 1;
    private static final byte DIRECTORY_FORMAT_SYMLINK   = 2;
    private static final byte DIRECTORY_FORMAT_TAGGED    = 3;

    private final Directory directory;

    public DirectoryFormatter(Buffer buffer)
    {
        this.buffer = buffer;
        directory = (Directory) buffer;
    }

    public LineSegmentList formatLine(Line line)
    {
        clearSegmentList();
        if (line == null || line.length() == 0) {
            addSegment("", DIRECTORY_FORMAT_TEXT);
            return segmentList;
        }
        final String text = line.getText();
        if (text.charAt(0) == 'T') {
            // Tagged file.
            int nameOffset = getNameOffset(text);
            addSegment("T", DIRECTORY_FORMAT_TAGGED);
            addSegment(text, 1, nameOffset, DIRECTORY_FORMAT_TEXT);
            addSegment(text, nameOffset, DIRECTORY_FORMAT_TAGGED);
            return segmentList;
        }
        final int index = text.indexOf(" -> ");
        if (index >= 0) {
            // Symbolic link.
            int nameOffset = getNameOffset(text);
            addSegment(text, 0, nameOffset, DIRECTORY_FORMAT_TEXT);
            addSegment(text, nameOffset, index, DIRECTORY_FORMAT_SYMLINK);
            addSegment(text, index, DIRECTORY_FORMAT_TEXT);
            return segmentList;
        }
        final String trim = text.trim();
        if (trim.startsWith("d") || trim.startsWith("<DIR>")) {
            // Directory.
            int nameOffset = getNameOffset(text);
            addSegment(text, 0, nameOffset, DIRECTORY_FORMAT_TEXT);
            addSegment(text, nameOffset, DIRECTORY_FORMAT_DIRECTORY);
            return segmentList;
        }
        addSegment(text, DIRECTORY_FORMAT_TEXT);
        return segmentList;
    }

    private int getNameOffset(String text)
    {
        REMatch match;
        if (directory.isUsingNativeFormat())
            match = Directory.getNativeMoveToFilenameRegExp().getMatch(text);
        else
            match = Directory.getInternalMoveToFilenameRegExp().getMatch(text);
        return (match != null) ? match.getEndIndex() : text.length();
    }

    public FormatTable getFormatTable()
    {
        if (formatTable == null) {
            formatTable = new FormatTable("DirectoryMode");
            formatTable.addEntryFromPrefs(DIRECTORY_FORMAT_TEXT, "text");
            formatTable.addEntryFromPrefs(DIRECTORY_FORMAT_DIRECTORY, "directory");
            formatTable.addEntryFromPrefs(DIRECTORY_FORMAT_SYMLINK, "symlink");
            formatTable.addEntryFromPrefs(DIRECTORY_FORMAT_TAGGED, "marked");
        }
        return formatTable;
    }
}
