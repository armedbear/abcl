/*
 * BasicVector_UnsignedByte8.java
 *
 * Copyright (C) 2002-2006 Peter Graves
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

import static org.armedbear.lisp.Lisp.*;

// A basic vector is a specialized vector that is not displaced to another
// array, has no fill pointer, and is not expressly adjustable.
public final class BasicVector_UnsignedByte8 extends AbstractVector
{
  private int capacity;
  private byte[] elements;

  public BasicVector_UnsignedByte8(int capacity)
  {
    elements = new byte[capacity];
    this.capacity = capacity;
  }

  public BasicVector_UnsignedByte8(byte[] array)
  {
    capacity = array.length;
    elements = new byte[capacity];
    System.arraycopy(array, 0, elements, 0, capacity);
  }


  public BasicVector_UnsignedByte8(LispObject[] array)
  {
    capacity = array.length;
    elements = new byte[capacity];
    for (int i = array.length; i-- > 0;)
      elements[i] = coerceLispObjectToJavaByte(array[i]);
  }

  @Override
  public LispObject typeOf()
  {
    return list(Symbol.SIMPLE_ARRAY, UNSIGNED_BYTE_8, new Cons(Fixnum.getInstance(capacity)));
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.VECTOR;
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type == Symbol.SIMPLE_ARRAY)
      return T;
    if (type == BuiltInClass.SIMPLE_ARRAY)
      return T;
    return super.typep(type);
  }

  @Override
  public LispObject getElementType()
  {
    return UNSIGNED_BYTE_8;
  }

  @Override
  public boolean isSimpleVector()
  {
    return false;
  }

  @Override
  public boolean hasFillPointer()
  {
    return false;
  }

  @Override
  public boolean isAdjustable()
  {
    return false;
  }

  @Override
  public int capacity()
  {
    return capacity;
  }

  @Override
  public int length()
  {
    return capacity;
  }

  @Override
  public LispObject elt(int index)
  {
    try
      {
        return coerceJavaByteToLispObject(elements[index]);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, capacity);
        return NIL; // Not reached.
      }
  }

  @Override
  public int aref(int index)
  {
    try
      {
        return (((int)elements[index]) & 0xff);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, elements.length);
        // Not reached.
        return 0;
      }
  }

  @Override
  public LispObject AREF(int index)
  {
    try
      {
        return coerceJavaByteToLispObject(elements[index]);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, elements.length);
        return NIL; // Not reached.
      }
  }

  @Override
  public void aset(int index, int n)
  {
    try
      {
        elements[index] = (byte) n;
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, capacity);
      }
  }

  @Override
  public void aset(int index, LispObject value)
  {
    try
      {
        elements[index] = coerceLispObjectToJavaByte(value);
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, capacity);
      }
  }

  @Override
  public LispObject subseq(int start, int end)
  {
    BasicVector_UnsignedByte8 v = new BasicVector_UnsignedByte8(end - start);
    int i = start, j = 0;
    try
      {
        while (i < end)
          v.elements[j++] = elements[i++];
        return v;
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        return error(new TypeError("Array index out of bounds: " + i + "."));
      }
  }

  @Override
  public void fill(LispObject obj)
  {
    if (!(obj instanceof Fixnum)) {
      type_error(obj, Symbol.FIXNUM);
      // Not reached.
      return;
    }
    int n = ((Fixnum) obj).value;
    if (n < 0 || n > 255) {
      type_error(obj, UNSIGNED_BYTE_8);
      // Not reached.
      return;
    }
    for (int i = capacity; i-- > 0;)
      elements[i] = (byte) n;
  }

  @Override
  public void shrink(int n)
  {
    if (n < capacity)
      {
        byte[] newArray = new byte[n];
        System.arraycopy(elements, 0, newArray, 0, n);
        elements = newArray;
        capacity = n;
        return;
      }
    if (n == capacity)
      return;
    error(new LispError());
  }

  @Override
  public LispObject reverse()
  {
    BasicVector_UnsignedByte8 result = new BasicVector_UnsignedByte8(capacity);
    int i, j;
    for (i = 0, j = capacity - 1; i < capacity; i++, j--)
      result.elements[i] = elements[j];
    return result;
  }

  @Override
  public LispObject nreverse()
  {
    int i = 0;
    int j = capacity - 1;
    while (i < j)
      {
        byte temp = elements[i];
        elements[i] = elements[j];
        elements[j] = temp;
        ++i;
        --j;
      }
    return this;
  }

  @Override
  public AbstractVector adjustArray(int newCapacity,
                                     LispObject initialElement,
                                     LispObject initialContents)

  {
    if (initialContents != null)
      {
        LispObject[] newElements = new LispObject[newCapacity];
        if (initialContents.listp())
          {
            LispObject list = initialContents;
            for (int i = 0; i < newCapacity; i++)
              {
                newElements[i] = list.car();
                list = list.cdr();
              }
          }
        else if (initialContents.vectorp())
          {
            for (int i = 0; i < newCapacity; i++)
              newElements[i] = initialContents.elt(i);
          }
        else
          type_error(initialContents, Symbol.SEQUENCE);
        return new BasicVector_UnsignedByte8(newElements);
      }
    if (capacity != newCapacity)
      {
        byte[] newElements = new byte[newCapacity];
        System.arraycopy(elements, 0, newElements, 0,
                         Math.min(capacity, newCapacity));
        if (initialElement != null) {
            byte initValue = (byte)(initialElement.intValue() & 0xFF);
            for (int i = capacity; i < newCapacity; i++)
                newElements[i] = initValue;
        }
        return new BasicVector_UnsignedByte8(newElements);
      }
    // No change.
    return this;
  }

  @Override
  public AbstractVector adjustArray(int newCapacity,
                                     AbstractArray displacedTo,
                                     int displacement)
  {
    return new ComplexVector(newCapacity, displacedTo, displacement);
  }
}
