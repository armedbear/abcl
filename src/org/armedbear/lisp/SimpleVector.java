/*
 * SimpleVector.java
 *
 * Copyright (C) 2002-2007 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

// "The type of a vector that is not displaced to another array, has no fill
// pointer, is not expressly adjustable and is able to hold elements of any
// type is a subtype of type SIMPLE-VECTOR."
public final class SimpleVector extends AbstractVector
{
  int capacity;
  LispObject[] data;

  public SimpleVector(int capacity)
  {
    data = new LispObject[capacity];
    for (int i = capacity; i-- > 0;)
      data[i] = Fixnum.ZERO;
    this.capacity = capacity;
  }

  public SimpleVector(LispObject obj)
  {
    if (obj.listp())
      {
        data = obj.copyToArray();
        capacity = data.length;
      }
    else if (obj instanceof AbstractVector)
      {
        capacity = obj.length();
        data = new LispObject[capacity];
        for (int i = 0; i < capacity; i++)
          data[i] = obj.elt(i);
      }
    else
      Debug.assertTrue(false);
  }

  public SimpleVector(LispObject[] array)
  {
    data = array;
    capacity = array.length;
  }

  @Override
  public LispObject typeOf()
  {
    return list(Symbol.SIMPLE_VECTOR, Fixnum.getInstance(capacity));
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.SIMPLE_VECTOR;
  }

  @Override
  public LispObject getDescription()
  {
    StringBuffer sb = new StringBuffer("A simple vector with ");
    sb.append(capacity);
    sb.append(" elements");
    return new SimpleString(sb);
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type == Symbol.SIMPLE_VECTOR)
      return T;
    if (type == Symbol.SIMPLE_ARRAY)
      return T;
    if (type == BuiltInClass.SIMPLE_VECTOR)
      return T;
    if (type == BuiltInClass.SIMPLE_ARRAY)
      return T;
    return super.typep(type);
  }

  @Override
  public LispObject getElementType()
  {
    return T;
  }

  @Override
  public boolean isSimpleVector()
  {
    return true;
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
        return data[index];
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, capacity);
        return NIL; // Not reached.
      }
  }

  @Override
  public LispObject AREF(int index)
  {
    try
      {
        return data[index];
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, data.length);
        return NIL; // Not reached.
      }
  }

  @Override
  public void aset(int index, LispObject newValue)
  {
    try
      {
        data[index] = newValue;
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, capacity);
      }
  }

  @Override
  public LispObject SVREF(int index)
  {
    try
      {
        return data[index];
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, data.length);
        return NIL; // Not reached.
      }
  }

  @Override
  public void svset(int index, LispObject newValue)
  {
    try
      {
        data[index] = newValue;
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        badIndex(index, capacity);
      }
  }

  @Override
  public LispObject subseq(int start, int end)
  {
    SimpleVector v = new SimpleVector(end - start);
    int i = start, j = 0;
    try
      {
        while (i < end)
          v.data[j++] = data[i++];
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
    for (int i = capacity; i-- > 0;)
      data[i] = obj;
  }

  @Override
  public LispObject deleteEq(LispObject item)
  {
    final int limit = capacity;
    int i = 0;
    int j = 0;
    while (i < limit)
      {
        LispObject obj = data[i++];
        if (obj != item)
          data[j++] = obj;
      }
    if (j < limit)
      shrink(j);
    return this;
  }

  @Override
  public LispObject deleteEql(LispObject item)
  {
    final int limit = capacity;
    int i = 0;
    int j = 0;
    while (i < limit)
      {
        LispObject obj = data[i++];
        if (!obj.eql(item))
          data[j++] = obj;
      }
    if (j < limit)
      shrink(j);
    return this;
  }

  @Override
  public void shrink(int n)
  {
    if (n < capacity)
      {
        LispObject[] newData = new LispObject[n];
        System.arraycopy(data, 0, newData, 0, n);
        data = newData;
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
    SimpleVector result = new SimpleVector(capacity);
    int i, j;
    for (i = 0, j = capacity - 1; i < capacity; i++, j--)
      result.data[i] = data[j];
    return result;
  }

  @Override
  public LispObject nreverse()
  {
    int i = 0;
    int j = capacity - 1;
    while (i < j)
      {
        LispObject temp = data[i];
        data[i] = data[j];
        data[j] = temp;
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
        LispObject[] newData = new LispObject[newCapacity];
        if (initialContents.listp())
          {
            LispObject list = initialContents;
            for (int i = 0; i < newCapacity; i++)
              {
                newData[i] = list.car();
                list = list.cdr();
              }
          }
        else if (initialContents.vectorp())
          {
            for (int i = 0; i < newCapacity; i++)
              newData[i] = initialContents.elt(i);
          }
        else
          error(new TypeError(initialContents, Symbol.SEQUENCE));
        return new SimpleVector(newData);
      }
    if (capacity != newCapacity)
      {
        LispObject[] newData = new LispObject[newCapacity];
        System.arraycopy(data, 0, newData, 0,
                         Math.min(capacity, newCapacity));
        if (initialElement != null)
            for (int i = capacity; i < newCapacity; i++)
                newData[i] = initialElement;
        return new SimpleVector(newData);
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

  // ### svref
  // svref simple-vector index => element
  private static final Primitive SVREF =
    new Primitive("svref", "simple-vector index")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
                        if (first instanceof SimpleVector) {
                                final SimpleVector sv = (SimpleVector)first;
                    int index = Fixnum.getValue(second);
                                try {
                                        return sv.data[index];
                                } catch (ArrayIndexOutOfBoundsException e) {
                                        int capacity = sv.capacity;
                                         sv.badIndex(index, capacity);
                                        // Not reached.
                                        return NIL;
                                }
                        }
                        return type_error(first, Symbol.SIMPLE_VECTOR);
                }
    };

  // ### svset simple-vector index new-value => new-value
  private static final Primitive SVSET =
    new Primitive("svset", PACKAGE_SYS, true, "simple-vector index new-value")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)

      {
                        if (first instanceof SimpleVector) {
                                final SimpleVector sv = (SimpleVector)first;
                    int index = Fixnum.getValue(second);
                                try {
                                        sv.data[index] = third;
                                        return third;
                                } catch (ArrayIndexOutOfBoundsException e) {
                                        int capacity = sv.capacity;
                                         sv.badIndex(index, capacity);
                                        // Not reached.
                                        return NIL;
                                }
                        }
                        return type_error(first, Symbol.SIMPLE_VECTOR);
      }
    };
}
