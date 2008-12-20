/*
 * Ring.java
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

import java.util.ArrayList;

public class Ring
{
    private final int capacity;
    private final ArrayList list;
    private int index;
    private int indexOfNextPop = -1;
    private String lastPop;

    public Ring(int capacity)
    {
        this.capacity = capacity;
        list = new ArrayList(capacity);
    }

    public synchronized final int size()
    {
        return list.size();
    }

    public String get(int i)
    {
        if (i >= 0 && i < list.size())
            return (String) list.get(i);
        else
            return null;
    }

    public synchronized void appendToCurrent(String s)
    {
        if (list.size() == 0)
            list.add(s);
        else {
            String existing = (String) list.get(list.size() - 1);
            list.set(list.size() - 1, existing.concat(s));
        }
    }

    public synchronized void appendNew(String s)
    {
        final int size = list.size();
        // See if we already have the string in question.
        for (int i = size-1; i >= 0; i--) {
            String existing = (String) list.get(i);
            if (existing.equals(s)) {
                // Found it! If it's not already the last element, promote it.
                if (i != size-1) {
                    list.remove(i);
                    list.add(s);
                }
                return;
            }
        }
        if (size < capacity)
            list.add(s);
        else {
            for (int i = 1; i < size; i++)
                list.set(i-1, list.get(i));
            list.set(capacity-1, s);
        }
    }

    public synchronized String peek()
    {
        if (list.size() == 0)
            return null;
        return (String) list.get(list.size() - 1);
    }

    public synchronized String pop()
    {
        indexOfNextPop = list.size() - 2;
        if (list.size() == 0)
            return null;
        return lastPop = (String) list.get(list.size() - 1);
    }

    public synchronized String popNext()
    {
        if (indexOfNextPop < 0)
            return null;
        Debug.assertTrue(indexOfNextPop < list.size());
        lastPop = (String) list.get(indexOfNextPop);
        if (--indexOfNextPop < 0)
            indexOfNextPop = list.size() - 1;
        return lastPop;
    }

    protected synchronized void promoteLast()
    {
        if (lastPop != null)
            promote(lastPop);
    }

    private void promote(String s)
    {
        for (int i = list.size()-1; i >= 0; i--) {
            String existing = (String) list.get(i);
            if (existing.equals(s)) {
                if (i != list.size()-1) {
                    list.remove(i);
                    list.add(s);
                }
                break;
            }
        }
    }
}
