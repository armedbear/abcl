/*
 * ComplexArray_ByteBuffer.java
 *
 * Copyright (C) 2020 easye
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

import java.nio.ByteBuffer;

public final class ComplexArray_ByteBuffer
  extends AbstractArray
{
  private final int[] dimv;
  private int totalSize;
  
  // For non-displaced arrays.
  private ByteBuffer data;
  private boolean directAllocation;

  // For displaced arrays.
  private AbstractArray array;
  private int displacement;

  public ComplexArray_ByteBuffer(int[] dimv) {
    this(dimv, false);
  }

  public ComplexArray_ByteBuffer(int[] dimv, boolean directAllocation) {
    this.dimv = dimv;
    this.directAllocation = directAllocation;
    totalSize = computeTotalSize(dimv);
    if (directAllocation) {
      data = ByteBuffer.allocateDirect(totalSize);
    } else {
      data = ByteBuffer.allocate(totalSize);
    }
  }

  public ComplexArray_ByteBuffer(int[] dimv, LispObject initialContents) {
    this(dimv, initialContents, false);
  }

  public ComplexArray_ByteBuffer(int[] dimv, LispObject initialContents, boolean directAllocation) {
    this.dimv = dimv;
    final int rank = dimv.length;
    LispObject rest = initialContents;
    this.directAllocation = directAllocation;
    for (int i = 0; i < rank; i++) {
      dimv[i] = rest.length();
      rest = rest.elt(0);
    }
    totalSize = computeTotalSize(dimv);
    if (directAllocation) {
      data = ByteBuffer.allocateDirect(totalSize);
    } else {
      data = ByteBuffer.allocate(totalSize);
    }
    setInitialContents(0, dimv, initialContents, 0);
  }

  public ComplexArray_ByteBuffer(int[] dimv, AbstractArray array, int displacement) {
    this(dimv, array, displacement, false);
  }
  
  public ComplexArray_ByteBuffer(int[] dimv, AbstractArray array, int displacement, boolean directAllocation) {
    this.dimv = dimv;
    this.array = array;
    this.displacement = displacement;
    this.directAllocation = directAllocation;
    totalSize = computeTotalSize(dimv);
  }

  private int setInitialContents(int axis, int[] dims, LispObject contents,
                                 int index)

  {
    if (dims.length == 0) {
      try {
        data.put(index, coerceLispObjectToJavaByte(contents));
      }
      catch (IndexOutOfBoundsException e) {
        error(new LispError("Bad initial contents for array."));
        return -1;
      }
      ++index;
    } else {
      int dim = dims[0];
      if (dim != contents.length()) {
        error(new LispError("Bad initial contents for array."));
        return -1;
      }
      int[] newDims = new int[dims.length-1];
      for (int i = 1; i < dims.length; i++)
        newDims[i-1] = dims[i];
      if (contents.listp()) {
        for (int i = contents.length();i-- > 0;) {
          LispObject content = contents.car();
          index =
            setInitialContents(axis + 1, newDims, content, index);
          contents = contents.cdr();
        }
      } else {
        AbstractVector v = checkVector(contents);
        final int length = v.length();
        for (int i = 0; i < length; i++) {
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
    return list(Symbol.ARRAY, UNSIGNED_BYTE_8, getDimensions());
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.ARRAY;
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
    try {
      return dimv[n];
    }
    catch (ArrayIndexOutOfBoundsException e) {
      error(new TypeError("Bad array dimension " + n + "."));
      return -1;
    }
  }

  @Override
  public LispObject getElementType()
  {
    return UNSIGNED_BYTE_8;
  }

  @Override
  public int getTotalSize()
  {
    return totalSize;
  }

  @Override
  public LispObject arrayDisplacement()
  {
    LispObject value1, value2;
    if (array != null) {
      value1 = array;
      value2 = Fixnum.getInstance(displacement);
    } else {
      value1 = NIL;
      value2 = Fixnum.ZERO;
    }
    return LispThread.currentThread().setValues(value1, value2);
  }

  @Override
  public LispObject AREF(int index)
  {
    if (data != null) {
      try {
        return coerceJavaByteToLispObject(data.get(index));
      }
      catch (IndexOutOfBoundsException e) {
        return error(new TypeError("Bad row major index " + index + "."));
      }
    } else
      return array.AREF(index + displacement);
  }

  @Override
  public void aset(int index, LispObject newValue)
  {
    if (data != null) {
      try {
        data.put(index, coerceLispObjectToJavaByte(newValue));
      }
      catch (IndexOutOfBoundsException e) {
        error(new TypeError("Bad row major index " + index + "."));
      }
    } else
      array.aset(index + displacement, newValue);
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
    if (data != null) {
      for (int i = data.limit(); i-- > 0;)
        data.put(i, (byte) n); // FIXME Faster!!
    } else {
      for (int i = totalSize; i-- > 0;)
        aset(i, obj);
    }
  }

  @Override
  public String printObject()
  {
    if (Symbol.PRINT_READABLY.symbolValue() != NIL) {
      error(new PrintNotReadable(list(Keyword.OBJECT, this)));
      // Not reached.
      return null;
    }
    return printObject(dimv);
  }

  int arrayTotalSize(int[] dims) {
    int result = 0;
    for (int dim : dims) {
      result += dim;
    }
    return result;
  }

  // FIXME move me to someplace more general
  public static void fill(ByteBuffer buffer, byte value) {
    for (int i = 0; i < buffer.limit(); i++) {
      buffer.put(value);
    }
  }

  @Override
  public AbstractArray adjustArray(int[] dims,
                                   LispObject initialElement,
                                   LispObject initialContents)
  {
    if (isAdjustable()) {
      if (initialContents != null)
        setInitialContents(0, dims, initialContents, 0);
      else {
        //### FIXME Take the easy way out: we don't want to reorganize
        // all of the array code yet
        //        SimpleArray_ByteBuffer newBuffer = new SimpleArray_ByteBuffer(dims);
        ByteBuffer newBuffer;
        if (directAllocation) {
          newBuffer = ByteBuffer.allocateDirect(computeTotalSize(dims));
        } else {
          newBuffer = ByteBuffer.allocate(computeTotalSize(dims));
        }
        if (initialElement != null) {
          fill(newBuffer, coerceLispObjectToJavaByte(initialElement));           
        }
        this.data = newBuffer;

        for (int i = 0; i < dims.length; i++)
          dimv[i] = dims[i];
      }
      return this;
    } else {
      if (initialContents != null)
        return new ComplexArray_ByteBuffer(dims, initialContents);
      else {
        ComplexArray_ByteBuffer newArray = new ComplexArray_ByteBuffer(dims);
        if (initialElement != null)
          newArray.fill(initialElement);
        return newArray;
      }
    }
  }

  @Override
  public AbstractArray adjustArray(int[] dims,
                                   AbstractArray displacedTo,
                                   int displacement)
  {
    if (isAdjustable()) {
      for (int i = 0; i < dims.length; i++)
        dimv[i] = dims[i];

      this.data = null;
      this.array = displacedTo;
      this.displacement = displacement;
      this.totalSize = computeTotalSize(dims);

      return this;
    } else {
      ComplexArray_ByteBuffer a = new ComplexArray_ByteBuffer(dims, displacedTo, displacement);

      return a;
    }
  }
}
