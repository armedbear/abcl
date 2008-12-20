/*
 * Compression.java
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

import java.util.zip.ZipEntry;

public final class Compression
{
    private final int type;
    private String source;
    private ZipEntry zipEntry;

    public Compression(int type)
    {
        this.type = type;
    }

    public Compression(int type, ZipEntry zipEntry, String source)
    {
        this.type = type;
        this.zipEntry = zipEntry;
        this.source = source;
    }

    public int getType()
    {
        return type;
    }

    public ZipEntry getZipEntry()
    {
        return zipEntry;
    }

    public String getSource()
    {
        return source;
    }

    public String getEntryName()
    {
        if (zipEntry == null)
            return null;
        return zipEntry.getName();
    }
}
