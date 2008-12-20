/*
 * WebHistoryEntry.java
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

public final class WebHistoryEntry
{
    private final File file;
    private int offset;
    private final String contentType;

    public WebHistoryEntry(File file, int offset, String contentType)
    {
        this.file = file;
        this.offset = offset;
        this.contentType = contentType;
    }

    public final File getFile()
    {
        return file;
    }

    public final int getOffset()
    {
        return offset;
    }

    public final void setOffset(int offset)
    {
        this.offset = offset;
    }

    public final String getContentType()
    {
        return contentType;
    }
}
