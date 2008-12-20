/*
 * RecentFilesEntry.java
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

import java.util.StringTokenizer;

public final class RecentFilesEntry
{
    private static boolean ignoreCase = Platform.isPlatformWindows();

    String name;
    String location;
    long firstVisit;
    long lastVisit;
    int lineNumber;
    int offs;

    public RecentFilesEntry(File file)
    {
        name = file.getName();
        if (file.isRemote()) {
            if (file.getProtocol() == File.PROTOCOL_HTTP) {
                location = File.PREFIX_HTTP + file.getHostName();
                if (file.getParent() != null)
                    location += file.getParent();
            } else if (file.getProtocol() == File.PROTOCOL_HTTPS) {
                location = File.PREFIX_HTTPS + file.getHostName();
                if (file.getParent() != null)
                    location += file.getParent();
            } else if (file.getProtocol() == File.PROTOCOL_FTP) {
                location = File.PREFIX_FTP + file.getHostName() + file.getParent();
            } else if (file.getProtocol() == File.PROTOCOL_SSH) {
                File parent = file.getParentFile();
                if (parent != null)
                    location = parent.netPath();
                else {
                    name = "";
                    location = file.netPath();
                }
            }
        } else
            location = file.getParent();
    }

    public RecentFilesEntry(String s)
    {
        StringTokenizer st = new StringTokenizer(s, "\t");
        name = st.nextToken();
        location = st.nextToken();
        firstVisit = Long.parseLong(st.nextToken());
        lastVisit = Long.parseLong(st.nextToken());
        lineNumber = Integer.parseInt(st.nextToken());
        offs = Integer.parseInt(st.nextToken());
        if (name.equals("\"\""))
            name = "";
    }

    public boolean matches(File file)
    {
        if (!name.equals(file.getName()))
             return false;
        if (location.startsWith(File.PREFIX_FTP) ||
            location.startsWith(File.PREFIX_HTTP) ||
            location.startsWith(File.PREFIX_HTTPS) ||
            location.startsWith(File.PREFIX_SSH)) {
            File parent = File.getInstance(location);
            return file.equals(File.getInstance(parent, name));
        }
        if (file.isRemote())
            return false;
        // Local file.
        return ignoreCase ? location.equalsIgnoreCase(file.getParent()) : location.equals(file.getParent());
    }

    public boolean equals(Object obj)
    {
        if (!(obj instanceof RecentFilesEntry))
            return false;
        RecentFilesEntry entry = (RecentFilesEntry) obj;
        if (ignoreCase) {
            if (entry.name.equalsIgnoreCase(name))
                if (entry.location.equalsIgnoreCase(location))
                    return true;
        } else {
            if (entry.name.equals(name))
                if (entry.location.equals(location))
                    return true;
        }
        return false;
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer(name);
        if (sb.length() == 0)
            sb.append("\"\"");
        sb.append('\t');
        sb.append(location);
        sb.append('\t');
        sb.append(String.valueOf(firstVisit));
        sb.append('\t');
        sb.append(String.valueOf(lastVisit));
        sb.append('\t');
        sb.append(String.valueOf(lineNumber));
        sb.append('\t');
        sb.append(String.valueOf(offs));
        return sb.toString();
    }

    public static final int getVersion()
    {
        return 1;
    }
}
