/*
 * WebHistory.java
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

import java.util.Vector;

public final class WebHistory
{
    private Vector v = new Vector();
    private int index = -1;

    public WebHistory()
    {
    }

    public boolean atEnd()
    {
        return index == -1;
    }

    public void truncate()
    {
        if (index >= 0)
            v.setSize(index);
    }

    public void append(File file, int offset, String contentType)
    {
        v.add(new WebHistoryEntry(file, offset, contentType));
    }

    public WebHistoryEntry getPrevious()
    {
        if (v.size() == 0)
            return null;
        if (index < 0)
            index = v.size();
        if (index > 0)
            return (WebHistoryEntry) v.get(--index);
        return null;
    }

    public WebHistoryEntry getNext()
    {
        if (v.size() == 0)
            return null;
        if (index < 0)
            return null;
        if (index < v.size()-1)
            return (WebHistoryEntry) v.get(++index);
        return null;
    }

    public WebHistoryEntry getCurrent()
    {
        if (index >= 0 && index < v.size())
            return (WebHistoryEntry) v.get(index);
        return null;
    }

    public void reset()
    {
        index = -1;
    }
}
