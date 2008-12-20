/*
 * DirectoryEntry.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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
import java.text.SimpleDateFormat;
import java.util.Date;

public final class DirectoryEntry
{
    private String name;
    private long date;
    private long size;
    private boolean isDirectory;
    private boolean isLink;
    private String linkedTo;
    private boolean isMarked;

    // For native format, this is all we store.
    private final String string;

    private static final String DIR = "     <DIR>";
    private static final int DIRLENGTH = DIR.length();
    private static final String DATEFORMAT = "MMM dd yyyy HH:mm";
    private static SimpleDateFormat dateFormatter =
        new SimpleDateFormat(DATEFORMAT);

    // Constructor for native "ls -l" format.
    private DirectoryEntry(String string, char firstChar)
    {
        this.string = string;
        if (firstChar == 'd')
            isDirectory = true;
        else if (firstChar == 'l')
            isLink = true;
        size = -1;
    }

    // Constructor for internal format.
    public DirectoryEntry(String name, long date, long size)
    {
        this.name = name;
        this.date = date;
        this.size = size;
        string = null;
    }

    // Constructor for internal format.
    public DirectoryEntry(String name, long date, long size, boolean isDirectory)
    {
        this.name = name;
        this.date = date;
        this.size = size;
        this.isDirectory = isDirectory;
        string = null;
    }

    // Wrapper for constructor for native "ls -l" format.
    // Ignore strings that aren't really directory entries.
    // Apply filter (if any).
    public static DirectoryEntry getDirectoryEntry(String s,
                                                   DirectoryFilenameFilter filter)
    {
        if (s.length() == 0)
            return null;
        // Ignore "total" line, command line echo.
        // First char must be one of "-dlcbsp".
        final char c = s.charAt(0);
        if ("-dlcbsp".indexOf(c) < 0)
            return null;
        if (c == 'l' && s.startsWith("ls "))
            return null;
        if (filter != null) {
            if (c != 'd' && !filter.accepts(getName(s)))
                return null;
        }
        return new DirectoryEntry(s, c);
    }

    // Extracts filename from "ls -l" directory listing.
    public static String getName(String s)
    {
        // Strip symbolic link if any.
        int end = s.indexOf(" -> ");
        if (end >= 0)
            s = s.substring(0, end);
        REMatch match = Directory.getNativeMoveToFilenameRegExp().getMatch(s);
        if (match != null)
            return s.substring(match.getEndIndex());
        Log.error("DirectoryEntry.getName returning null s = |" + s + "|");
        return null;
    }

    public static int getNameColumn(String s)
    {
        // Strip symbolic link if any.
        int end = s.indexOf(" -> ");
        if (end >= 0)
            s = s.substring(0, end);
        REMatch match = Directory.getNativeMoveToFilenameRegExp().getMatch(s);
        if (match != null)
            return match.getEndIndex();
        return 0;
    }

    public final String extractName()
    {
        if (name == null && string != null)
            name = getName(string);
        return name;
    }

    // nameColumn must be > 0. It's just a hint.
    public final String extractName(int nameColumn)
    {
        if (name != null)
            return name; // Cached.
        if (string == null)
            return null;
        if (nameColumn > 0 && nameColumn < string.length()) {
            String s = string;
            // Strip symbolic link if any.
            int end = s.indexOf(" -> ");
            if (end >= 0)
                s = s.substring(0, end);
            if (s.charAt(nameColumn-1) == ' ')
                if (!Character.isWhitespace(s.charAt(nameColumn)))
                    return name = s.substring(nameColumn);
        }
        // Do it the hard way.
        return getName(string);
    }

    public final String getName()
    {
        return name;
    }

    public final long getDate()
    {
        return date;
    }

    public final long getSize()
    {
        return size;
    }

    public final void setSize(long size)
    {
        this.size = size;
    }

    public final boolean isDirectory()
    {
        return isDirectory;
    }

    public final boolean isLink()
    {
        return isLink;
    }

    public final void setLinkedTo(String linkedTo)
    {
        this.linkedTo = linkedTo;
    }

    public final boolean isMarked()
    {
        return isMarked;
    }

    public final void setMarked(boolean b)
    {
        isMarked = b;
    }

    public final String getString()
    {
        return string;
    }

    public String toString()
    {
        String marked = isMarked ? "T " : "  ";
        // Use saved string for native format.
        if (string != null)
            return marked.concat(string);
        // Construct string for internal format.
        FastStringBuffer sb = new FastStringBuffer(256);
        sb.append(marked);
        if (isDirectory) {
            sb.append(DIR);
        } else {
            String sizeString = String.valueOf(size);
            for (int j = 0; j < DIRLENGTH - sizeString.length(); j++)
                sb.append(' ');
            sb.append(sizeString);
        }
        sb.append(' ');
        String dateString = dateFormatter.format(new Date(date));
        sb.append(dateString);
        sb.append(' ');
        sb.append(name);
        if (linkedTo != null) {
            sb.append(" -> ");
            sb.append(linkedTo);
        }
        return sb.toString();
    }
}
