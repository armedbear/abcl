/*
 * DirectoryTreeElement.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

public final class DirectoryTreeElement
{
    private File file;

    public DirectoryTreeElement(File file)
    {
        this.file = file;
    }

    public File getFile()
    {
        return file;
    }

    public String getPath()
    {
        return file.canonicalPath();
    }

    public String toString()
    {
        if (file.canonicalPath().equals("/")) {
            if (file.isRemote())
                return file.getHostName();
            // Local.
            return "/";
        }
        String s = file.getName();
        if (s != null && s.length() > 0)
            return s;
        // "C:\"
        return file.netPath();
    }

    public String getStatusText()
    {
        return null;
    }
}
