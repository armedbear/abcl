/*
 * EditorList.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.util.ArrayList;
import java.util.Iterator;

public final class EditorList
{
    private final ArrayList list = new ArrayList();

    public synchronized int size()
    {
        return list.size();
    }

    public synchronized Editor get(int i)
    {
        if (i >= 0 && i < list.size())
            return (Editor) list.get(i);
        else
            return null;
    }

    public synchronized void add(Editor editor)
    {
        if (list.contains(editor)) {
            Debug.bug();
            return;
        }
        list.add(editor);
    }

    public synchronized boolean remove(Editor editor)
    {
        return list.remove(editor);
    }

    public synchronized boolean contains(Editor editor)
    {
	return list.contains(editor);
    }

    public synchronized Iterator iterator()
    {
        return list.iterator();
    }
}
