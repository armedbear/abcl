/*
 * LineSegmentList.java
 *
 * Copyright (C) 2002-2004 Peter Graves
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

public final class LineSegmentList
{
    private LineSegment[] array = new LineSegment[32];
    private int size;

    public final int size()
    {
        return size;
    }

    public final void clear()
    {
        for (int i = size-1; i >= 0; i--)
            array[i] = null;
        size = 0;
    }

    // No range checking.
    public final LineSegment getSegment(int index)
    {
        return array[index];
    }

    public final LineSegment getLastSegment()
    {
        if (size > 0)
            return array[size-1];
        else
            return null;
    }

    public final void addSegment(LineSegment segment)
    {
        if (size == array.length)
            ensureCapacity(size+1);
        array[size++] = segment;
    }

    public final void addSegment(int index, LineSegment segment)
    {
        if (index < 0 || index > size)
            throw new IndexOutOfBoundsException();
        if (size == array.length)
            ensureCapacity(size+1);
        if (index != size)
            System.arraycopy(array, index, array, index+1, size-index);
        array[index] = segment;
        size++;
    }

    public final void setSegment(int index, LineSegment segment)
    {
        if (index < 0 || index >= size)
            throw new IndexOutOfBoundsException();
        array[index] = segment;
    }

    public final void removeSegment(LineSegment segment)
    {
        for (int i = 0; i < size; i++) {
            if (array[i] == segment) {
                if (i != --size)
                    System.arraycopy(array, i+1, array, i, size-i);
                array[size] = null;
            }
        }
    }

    private void ensureCapacity(int minimumCapacity)
    {
        int currentCapacity = array.length;
        if (minimumCapacity > 0 && currentCapacity < minimumCapacity) {
            final int newCapacity = Math.max(minimumCapacity, currentCapacity*2+2);
            LineSegment newArray[] = new LineSegment[newCapacity];
            System.arraycopy(array, 0, newArray, 0, size);
            array = newArray;
        }
    }
}
