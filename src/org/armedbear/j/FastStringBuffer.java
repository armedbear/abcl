/*
 * FastStringBuffer.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

public final class FastStringBuffer
{
  private static final int SPARE_CAPACITY = 128;

  private char[] buffer;
  private int used;

  public FastStringBuffer()
  {
    buffer = new char[SPARE_CAPACITY];
  }

  public FastStringBuffer(String s)
  {
    used = s.length();
    buffer = new char[used + SPARE_CAPACITY];
    s.getChars(0, used, buffer, 0);
  }

  public FastStringBuffer(char c)
  {
    used = 1;
    buffer = new char[1 + SPARE_CAPACITY];
    buffer[0] = c;
  }

  public FastStringBuffer(int length) throws NegativeArraySizeException
  {
    if (length < 0)
      throw new NegativeArraySizeException();
    buffer = new char[length];
  }

  public final int length()
  {
    return used;
  }

  public final int capacity()
  {
    return buffer.length;
  }

  public final char charAt(int index)
  {
    try
      {
        return buffer[index];
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        throw new StringIndexOutOfBoundsException();
      }
  }

  public void getChars(int srcBegin, int srcEnd, char dst[], int dstBegin)
  {
    if (srcBegin < 0 || srcBegin > srcEnd || srcEnd > used)
      throw new StringIndexOutOfBoundsException();
    System.arraycopy(buffer, srcBegin, dst, dstBegin, srcEnd - srcBegin);
  }

  public void setCharAt(int index, char c)
  {
    try
      {
        buffer[index] = c;
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        throw new StringIndexOutOfBoundsException();
      }
  }

  public void ensureCapacity(int minimumCapacity)
  {
    if (buffer.length < minimumCapacity)
      {
        int newCapacity = buffer.length * 2 + 2;
        if (newCapacity < minimumCapacity)
          newCapacity = minimumCapacity;
        char newBuffer[] = new char[newCapacity];
        System.arraycopy(buffer, 0, newBuffer, 0, used);
        buffer = newBuffer;
      }
  }

  public void setText(String s)
  {
    used = 0;
    append(s);
  }

  public FastStringBuffer append(String s)
  {
    if (s == null)
      s = "null";
    int addedLength = s.length();
    int combinedLength = used + addedLength;
    ensureCapacity(combinedLength);
    s.getChars(0, addedLength, buffer, used);
    used = combinedLength;
    return this;
  }

  public FastStringBuffer append(char[] chars)
  {
    if (used + chars.length > buffer.length)
      ensureCapacity(used + chars.length);
    System.arraycopy(chars, 0, buffer, used, chars.length);
    used += chars.length;
    return this;
  }

  public FastStringBuffer append(char[] chars, int offset, int len)
  {
    if (offset < 0 || len < 0 || offset + len > chars.length)
      throw new StringIndexOutOfBoundsException();
    if (used + len > buffer.length)
      ensureCapacity(used + len);
    System.arraycopy(chars, offset, buffer, used, len);
    used += len;
    return this;
  }

  public FastStringBuffer append(Object object)
  {
    return append(String.valueOf(object));
  }

  public FastStringBuffer append(char c)
  {
    if (used + 1 > buffer.length)
      ensureCapacity(used + 1);
    buffer[used++] = c;
    return this;
  }

  public final FastStringBuffer append(int n)
  {
    return append(String.valueOf(n));
  }

  public final FastStringBuffer append(long n)
  {
    return append(String.valueOf(n));
  }

  public void setLength(int newLength) throws IndexOutOfBoundsException
  {
    if (newLength < 0)
      throw new StringIndexOutOfBoundsException(newLength);
    ensureCapacity(newLength);
    used = newLength;
  }

  public FastStringBuffer reverse()
  {
    final int limit = used / 2;
    for (int i = 0; i < limit; ++i)
      {
        char c = buffer[i];
        buffer[i] = buffer[used - i - 1];
        buffer[used - i - 1] = c;
      }
    return this;
  }

  public final String toString()
  {
    return new String(buffer, 0, used);
  }

  public final char[] toCharArray()
  {
    char[] copy = new char[used];
    System.arraycopy(buffer, 0, copy, 0, used);
    return copy;
  }
}
