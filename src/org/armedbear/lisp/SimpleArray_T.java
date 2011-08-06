/*
 * SimpleArray_T.java
 *
 * Copyright (C) 2003-2007 Peter Graves
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

public final class SimpleArray_T extends AbstractArray
{
  private final int[] dimv;
  private final LispObject elementType;
  private final int totalSize;
  final LispObject[] data;

  public SimpleArray_T(int[] dimv, LispObject elementType)
  {
    this.dimv = dimv;
    this.elementType = elementType;
    totalSize = computeTotalSize(dimv);
    data = new LispObject[totalSize];
    for (int i = totalSize; i-- > 0;)
      data[i] = Fixnum.ZERO;
  }

  public SimpleArray_T(int[] dimv,
                       LispObject elementType,
                       LispObject initialContents)

  {
    this.dimv = dimv;
    this.elementType = elementType;
    final int rank = dimv.length;
    LispObject rest = initialContents;
    for (int i = 0; i < rank; i++)
      {
        dimv[i] = rest.length();
        rest = rest.elt(0);
      }
    totalSize = computeTotalSize(dimv);
    data = new LispObject[totalSize];
    setInitialContents(0, dimv, initialContents, 0);
  }

  public SimpleArray_T(int rank, LispObject initialContents)

  {
    if (rank < 2)
      Debug.assertTrue(false);
    dimv = new int[rank];
    this.elementType = T;
    LispObject rest = initialContents;
    for (int i = 0; i < rank; i++)
      {
        dimv[i] = rest.length();
        if (rest == NIL || rest.length() == 0)
          break;
        rest = rest.elt(0);
      }
    totalSize = computeTotalSize(dimv);
    data = new LispObject[totalSize];
    setInitialContents(0, dimv, initialContents, 0);
  }

  public SimpleArray_T(final int[] dimv, final LispObject[] initialData,
          final LispObject elementType) {
      this.dimv = dimv;
      this.elementType = elementType;
      this.data = initialData;
      this.totalSize = computeTotalSize(dimv);
  }

  private int setInitialContents(int axis, int[] dims, LispObject contents,
                                 int index)

  {
    if (dims.length == 0)
      {
        try
          {
            data[index] = contents;
          }
        catch (ArrayIndexOutOfBoundsException e)
          {
            error(new LispError("Bad initial contents for array."));
            return -1;
          }
        ++index;
      }
    else
      {
        int dim = dims[0];
        if (dim != contents.length())
          {
            error(new LispError("Bad initial contents for array."));
            return -1;
          }
        int[] newDims = new int[dims.length-1];
        for (int i = 1; i < dims.length; i++)
          newDims[i-1] = dims[i];
        if (contents.listp())
          {
            for (int i = contents.length();i-- > 0;)
              {
                LispObject content = contents.car();
                index =
                  setInitialContents(axis + 1, newDims, content, index);
                contents = contents.cdr();
              }
          }
        else
          {
            AbstractVector v = checkVector(contents);
            final int length = v.length();
            for (int i = 0; i < length; i++)
              {
                LispObject content = v.AREF(i);
                index =
                  setInitialContents(axis + 1, newDims, content, index);
              }
          }
      }
    return index;
  }

  @Override
  public LispObject typeOf()
  {
    return list(Symbol.SIMPLE_ARRAY, elementType, getDimensions());
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.SIMPLE_ARRAY;
  }

  @Override
  public LispObject typep(LispObject typeSpecifier)
  {
    if (typeSpecifier == Symbol.SIMPLE_ARRAY)
      return T;
    if (typeSpecifier == BuiltInClass.SIMPLE_ARRAY)
      return T;
    return super.typep(typeSpecifier);
  }

  @Override
  public int getRank()
  {
    return dimv.length;
  }

  @Override
  public LispObject getDimensions()
  {
    LispObject result = NIL;
    for (int i = dimv.length; i-- > 0;)
      result = new Cons(Fixnum.getInstance(dimv[i]), result);
    return result;
  }

  @Override
  public int getDimension(int n)
  {
    try
      {
        return dimv[n];
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        error(new TypeError("Bad array dimension " + n + "."));
        return -1;
      }
  }

  @Override
  public LispObject getElementType()
  {
    return elementType;
  }

  @Override
  public int getTotalSize()
  {
    return totalSize;
  }

  @Override
  public boolean isAdjustable()
  {
    return false;
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
        return error(new TypeError("Bad row major index " + index + "."));
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
        error(new TypeError("Bad row major index " + index + "."));
      }
  }

  @Override
  public int getRowMajorIndex(int[] subscripts)
  {
    final int rank = dimv.length;
    if (rank != subscripts.length)
      {
        StringBuilder sb = new StringBuilder("Wrong number of subscripts (");
        sb.append(subscripts.length);
        sb.append(") for array of rank ");
        sb.append(rank);
        sb.append('.');
        error(new ProgramError(sb.toString()));
      }
    int sum = 0;
    int size = 1;
    for (int i = rank; i-- > 0;)
      {
        final int dim = dimv[i];
        final int lastSize = size;
        size *= dim;
        int n = subscripts[i];
        if (n < 0 || n >= dim)
          {
            StringBuilder sb = new StringBuilder("Invalid index ");
            sb.append(n);
            sb.append(" for array ");
            sb.append(this);
            sb.append('.');
            error(new ProgramError(sb.toString()));
          }
        sum += n * lastSize;
      }
    return sum;
  }

  @Override
  public LispObject get(int[] subscripts)
  {
    try
      {
        return data[getRowMajorIndex(subscripts)];
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        return error(new TypeError("Bad row major index " +
                                   getRowMajorIndex(subscripts) + "."));
      }
  }

  @Override
  public void set(int[] subscripts, LispObject newValue)

  {
    try
      {
        data[getRowMajorIndex(subscripts)] = newValue;
      }
    catch (ArrayIndexOutOfBoundsException e)
      {
        error(new TypeError("Bad row major index " +
                            getRowMajorIndex(subscripts) + "."));
      }
  }

  @Override
  public void fill(LispObject obj)
  {
    for (int i = totalSize; i-- > 0;)
      data[i] = obj;
  }

  @Override
  public String printObject()
  {
    return printObject(dimv);
  }

  @Override
  public AbstractArray adjustArray(int[] dimv, LispObject initialElement,
                                   LispObject initialContents)

  {
    if (initialContents != null)
      return new SimpleArray_T(dimv, elementType, initialContents);
    for (int i = 0; i < dimv.length; i++)
      {
        if (dimv[i] != this.dimv[i])
          {
            SimpleArray_T newArray = new SimpleArray_T(dimv, elementType);
            if (initialElement != null)
                newArray.fill(initialElement);
            copyArray(this, newArray);
            return newArray;
          }
      }
    // New dimensions are identical to old dimensions, yet
    // we're not mutable, so, we need to return a new array
    return new SimpleArray_T(dimv, data.clone(), elementType);
  }

  // Copy a1 to a2 for index tuples that are valid for both arrays.
  static void copyArray(AbstractArray a1, AbstractArray a2)

  {
    Debug.assertTrue(a1.getRank() == a2.getRank());
    int[] subscripts = new int[a1.getRank()];
    int axis = 0;
    copySubArray(a1, a2, subscripts, axis);
  }

  private static void copySubArray(AbstractArray a1, AbstractArray a2,
                                   int[] subscripts, int axis)

  {
    if (axis < subscripts.length)
      {
        final int limit =
          Math.min(a1.getDimension(axis), a2.getDimension(axis));
        for (int i = 0; i < limit; i++)
          {
            subscripts[axis] = i;
            copySubArray(a1, a2, subscripts, axis + 1);
          }
      }
    else
      {
        int i1 = a1.getRowMajorIndex(subscripts);
        int i2 = a2.getRowMajorIndex(subscripts);
        a2.aset(i2, a1.AREF(i1));
      }
  }

  @Override
  public AbstractArray adjustArray(int[] dimv, AbstractArray displacedTo,
                                   int displacement)
  {
    return new ComplexArray(dimv, displacedTo, displacement);
  }
}
