/*
 * ByteBuffer.java
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

public final class ByteBuffer
{
    private byte[] buffer;
    private int used;
    private final static int DEFAULT_CAPACITY = 16;

    public ByteBuffer()
    {
        buffer = new byte[DEFAULT_CAPACITY];
    }

    public ByteBuffer(int length) throws NegativeArraySizeException
    {
        if (length < 0)
            throw new NegativeArraySizeException();
        buffer = new byte[length];
    }

    public int length()
    {
        return used;
    }

    public int capacity()
    {
        return buffer.length;
    }

    public void ensureCapacity(int minimumCapacity)
    {
        if (minimumCapacity <= 0 || buffer.length >= minimumCapacity)
            return;
        int newCapacity = buffer.length * 2 + 2;
        if (newCapacity < minimumCapacity)
            newCapacity = minimumCapacity;
        byte newBuffer[] = new byte[newCapacity];
        System.arraycopy(buffer, 0, newBuffer, 0, used);
        buffer = newBuffer;
    }

    public void append(byte[] bytes)
    {
        if (used + bytes.length > buffer.length)
            ensureCapacity(used + bytes.length);
        System.arraycopy(bytes, 0, buffer, used, bytes.length);
        used += bytes.length;
    }

    public void append(byte b)
    {
        if (used + 1 > buffer.length)
            ensureCapacity(used + 1);
        buffer[used++] = b;
    }

    public void setLength(int newLength) throws IndexOutOfBoundsException
    {
        if (newLength < 0)
            throw new ArrayIndexOutOfBoundsException(newLength);
        ensureCapacity(newLength);
        used = newLength;
    }

    public byte[] getBytes()
    {
        return buffer;
    }
}
