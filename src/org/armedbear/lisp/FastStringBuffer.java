/*
 * FastStringBuffer.java
 *
 * Copyright (C) 1998-2005 Peter Graves
 * Copyright (C) 2008 Phil Hudson
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
 *
 * As a special exception, the copyright holders of this library give you
 * permission to link this library with independent modules to produce an
 * executable, regardless of the license terms of these independent
 * modules, and to copy and distribute the resulting executable under
 * terms of your choice, provided that you also meet, for each linked
 * independent module, the terms and conditions of the license of that
 * module.  An independent module is a module which is not derived from
 * or based on this library.  If you modify this library, you may extend
 * this exception to your version of the library, but you are not
 * obligated to do so.  If you do not wish to do so, delete this
 * exception statement from your version.
 */

package org.armedbear.lisp;

/** 
 * An adaptor of the Java 1.5 java.lang.StringBuilder.
 * 
 * "This class should be removed with all references to it replaced
 *  with java.lang.StringBuilder once enough confidence in this change
 *  has been gained." -- Phil Hudson 20090202 via <armedbear-j-devel>.
 */
public final class FastStringBuffer implements Appendable, CharSequence
{
  private static final int SPARE_CAPACITY = 128;

  private final StringBuilder builder;

  public FastStringBuffer()
  {
    this(SPARE_CAPACITY);
  }

  public FastStringBuffer(String s) 
  {
    builder = new StringBuilder(s);
  }

  public FastStringBuffer(char c)
  {
    this(String.valueOf(c));
  }

  public FastStringBuffer(int length) throws NegativeArraySizeException
  {
    builder = new StringBuilder(length);
  }

  public final int length()
  {
    return builder.length();
  }

  public final int capacity()
  {
    return builder.capacity();
  }

  public final char charAt(int index)
  {
    return builder.charAt(index);
  }

  public void getChars(int srcBegin, int srcEnd, char dst[], int dstBegin)
  {
    builder.getChars(srcBegin, srcEnd, dst, dstBegin);
  }

  public void setCharAt(int index, char c)
  {
    builder.setCharAt(index, c);
  }

  public void ensureCapacity(int minimumCapacity)
  {
    builder.ensureCapacity(minimumCapacity);
  }

  public FastStringBuffer append(String s)
  {
    builder.append(s);
    return this;
  }

  public FastStringBuffer append(char[] chars)
  {
    builder.append(chars);
    return this;
  }

  public FastStringBuffer append(char[] chars, int offset, int len)
  {
    builder.append(chars, offset, len);
    return this;
  }

  public FastStringBuffer append(Object object)
  {
    return append(String.valueOf(object));
  }

  public FastStringBuffer append(char c)
  {
    builder.append(c);
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
    builder.setLength(newLength);
  }

  public FastStringBuffer reverse()
  {
    builder.reverse();
    return this; 
  }

  @Override
  public final String toString()
  {
    return builder.toString();
  }

  public final char[] toCharArray()
  {
    return toString().toCharArray();
  }

   public CharSequence subSequence(int start, int end)
  {
    return builder.subSequence(start, end);
   }
 
   public FastStringBuffer append(CharSequence seq)
   {
     builder.append(seq);
     return this;
   }
 
   public FastStringBuffer append(CharSequence seq, int start, int end)
   {
     builder.append(seq, start, end);
     return this;
   }
}
