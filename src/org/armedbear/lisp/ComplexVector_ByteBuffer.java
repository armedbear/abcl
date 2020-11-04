/*
 * ComplexVector_ByteBuffer.java
 *
 * Copyright (C) 2020 @easye
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

// A specialized vector of element type (UNSIGNED-BYTE 8) that is displaced to
// another array, has a fill pointer, and/or is expressly adjustable.
public final class ComplexVector_ByteBuffer extends AbstractVector
{
  private int capacity; 
  private int fillPointer = -1; // -1 indicates no fill pointer.
  private boolean isDisplaced;

  // For non-displaced arrays.
  private ByteBuffer elements;
  private boolean directAllocation;

  // For displaced arrays.
  private AbstractArray array;
  private int displacement;

  public ComplexVector_ByteBuffer(int capacity) {
    this(capacity, false);
  }
  
  public ComplexVector_ByteBuffer(int capacity, boolean directAllocation) {
    this.capacity = capacity;
    this.directAllocation = directAllocation;
    if (directAllocation) {
      elements = ByteBuffer.allocateDirect(capacity);
    } else {
      elements = ByteBuffer.allocate(capacity);
    }
  }    

  public ComplexVector_ByteBuffer(int capacity, AbstractArray array,
                                  int displacement) {
    this(capacity, array, displacement, false);
  }

  public ComplexVector_ByteBuffer(int capacity, AbstractArray array,
                                  int displacement,
                                  boolean directAllocation) {
    this.capacity = capacity;
    this.array = array;
    this.displacement = displacement;
    this.directAllocation = directAllocation;
    isDisplaced = true;
  }

  @Override
  public LispObject typeOf() {
    return list(Symbol.VECTOR, UNSIGNED_BYTE_8, Fixnum.getInstance(capacity));
  }

  @Override
  public LispObject classOf() {
    return BuiltInClass.VECTOR;
  }

  @Override
  public boolean hasFillPointer() {
    return fillPointer >= 0;
  }

  @Override
  public int getFillPointer()
  {
    return fillPointer;
  }

  @Override
  public void setFillPointer(int n)
  {
    fillPointer = n;
  }

  @Override
  public void setFillPointer(LispObject obj)
  {
    if (obj == T)
      fillPointer = capacity();
    else {
      int n = Fixnum.getValue(obj);
      if (n > capacity()) {
        StringBuffer sb = new StringBuffer("The new fill pointer (");
        sb.append(n);
        sb.append(") exceeds the capacity of the vector (");
        sb.append(capacity());
        sb.append(").");
        error(new LispError(sb.toString()));
      } else if (n < 0) {
        StringBuffer sb = new StringBuffer("The new fill pointer (");
        sb.append(n);
        sb.append(") is negative.");
        error(new LispError(sb.toString()));
      } else
        fillPointer = n;
    }
  }

  @Override
  public boolean isDisplaced()
  {
    return isDisplaced;
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
  public int capacity()
  {
    return capacity;
  }

  @Override
  public int length() {
    return fillPointer >= 0 ? fillPointer : capacity;
  }

  @Override
  public LispObject elt(int index)
  {
    final int limit = length();
    if (index < 0 || index >= limit)
      badIndex(index, limit);
    return AREF(index);
  }

  // Ignores fill pointer.
  @Override
  public LispObject AREF(int index) {
    if (elements != null) {
      try {
        return coerceFromJavaByte(elements.get(index));
      } catch (ArrayIndexOutOfBoundsException e) {
        badIndex(index, ((java.nio.Buffer)elements).limit());
        return NIL; // Not reached.
      }
    } else {
      // Displaced array.
      if (index < 0 || index >= capacity) {
        badIndex(index, capacity);
      }
      return array.AREF(index + displacement);
    }
  }

  @Override
  public void aset(int index, int n) {
    if (elements != null) {
      try {
        elements.put(index, (byte) n);
      } catch (IndexOutOfBoundsException e) {
        badIndex(index, capacity);
      }
    } else {
      // Displaced array.
      if (index < 0 || index >= capacity) {
        badIndex(index, capacity);
      } else {
        array.aset(index + displacement, n);
      }
    }
  }

  @Override
  public void aset(int index, LispObject newValue)
  {
    if (elements != null) {
      try {
        elements.put(index, coerceToJavaByte(newValue));
      } catch (IndexOutOfBoundsException e) {
        badIndex(index, ((java.nio.Buffer)elements).limit());
      }
    } else {
      array.aset(index + displacement, newValue);
    }
  }

  @Override
  public LispObject subseq(int start, int end) {
    SimpleVector v = new SimpleVector(end - start);
    int i = start, j = 0;
    try {
      while (i < end)
        v.aset(j++, AREF(i++));
      return v;
    }
    catch (IndexOutOfBoundsException e) {
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
      elements.put(i, (byte) n);
  }

  @Override
  public void shrink(int n) {
    // One cannot shrink the underlying ByteBuffer physically, so
    // use the limit marker to denote the length
    if (n < length()) {
      ((java.nio.Buffer)elements).limit(n);
      this.capacity = n;
      return;
    }
    if (n == ((java.nio.Buffer)elements).limit()) { 
      return;
    }
    error(new LispError());
  }

  @Override
  public LispObject reverse()
  {
    int length = length();
    BasicVector_ByteBuffer result = new BasicVector_ByteBuffer(length, directAllocation);
    int i, j;
    for (i = 0, j = length - 1; i < length; i++, j--)
      result.aset(i, AREF(j));
    return result;
  }

  @Override
  public LispObject nreverse() {
    if (elements != null) {
      int i = 0;
      int j = length() - 1;
      while (i < j) {
        byte temp = elements.get(i);
        elements.put(i, elements.get(j));
        elements.put(j, temp);
        ++i;
        --j;
      }
    } else {
      // Displaced array.
      int length = length();
      ByteBuffer data = null;
      if (directAllocation) {
        data = ByteBuffer.allocateDirect(length);
      } else {
        data = ByteBuffer.allocate(length);
      }
      int i, j;
      for (i = 0, j = length - 1; i < length; i++, j--) {
        data.put(i, coerceToJavaByte(AREF(j)));
      }
      elements = data;
      capacity = length;
      array = null;
      displacement = 0;
      isDisplaced = false;
      fillPointer = -1;
    }
    return this;
  }

  @Override
  public void vectorPushExtend(LispObject element)
  {
    if (fillPointer < 0)
      noFillPointer();
    if (fillPointer >= capacity) {
      // Need to extend vector.
      ensureCapacity(capacity * 2 + 1);
    }
    aset(fillPointer, element);
    ++fillPointer;
  }

  @Override
  public LispObject VECTOR_PUSH_EXTEND(LispObject element)

  {
    vectorPushExtend(element);
    return Fixnum.getInstance(fillPointer - 1);
  }

  @Override
  public LispObject VECTOR_PUSH_EXTEND(LispObject element, LispObject extension)

  {
    int ext = Fixnum.getValue(extension);
    if (fillPointer < 0)
      noFillPointer();
    if (fillPointer >= capacity) {
      // Need to extend vector.
      ext = Math.max(ext, capacity + 1);
      ensureCapacity(capacity + ext);
    }
    aset(fillPointer, element);
    return Fixnum.getInstance(fillPointer++);
  }

  private final void ensureCapacity(int minCapacity) {
    if (elements != null) {
      if (capacity < minCapacity) {
        ByteBuffer newBuffer = null;
        if (directAllocation) {
          newBuffer = ByteBuffer.allocateDirect(minCapacity);
        } else { 
          newBuffer = ByteBuffer.allocate(minCapacity);
        }
        elements.position(0);
        newBuffer.put(elements);
        newBuffer.position(0);
        elements = newBuffer;
        capacity = minCapacity;
      }
    } else {
      // Displaced array.
      Debug.assertTrue(array != null);
      if (capacity < minCapacity
          || array.getTotalSize() - displacement < minCapacity) {
          // Copy array.
        if (directAllocation) {
          elements = ByteBuffer.allocateDirect(minCapacity);
        } else {
          elements = ByteBuffer.allocate(minCapacity);
        }
        final int limit
          = Math.min(length(), array.getTotalSize() - displacement);
        for (int i = 0; i < limit; i++) {
          elements.put(i, coerceToJavaByte(array.AREF(displacement + i)));
        }
        capacity = minCapacity;
        array = null;
        displacement = 0;
        isDisplaced = false;
      }
    }
  }

  @Override
  public AbstractVector adjustArray(int newCapacity,
                                    LispObject initialElement,
                                    LispObject initialContents) {
    if (initialContents != null) {
      // "If INITIAL-CONTENTS is supplied, it is treated as for MAKE-
      // ARRAY. In this case none of the original contents of array
      // appears in the resulting array."
      ByteBuffer newElements = null;
      if (directAllocation) {
        newElements = ByteBuffer.allocateDirect(newCapacity);
      } else {
        newElements = ByteBuffer.allocate(newCapacity);
      }

      if (initialContents.listp()) {
        LispObject list = initialContents;
          for (int i = 0; i < newCapacity; i++) {
            newElements.put(i, coerceToJavaByte(list.car()));
            list = list.cdr();
          }
      } else if (initialContents.vectorp()) {
        for (int i = 0; i < newCapacity; i++) {
          newElements.put(i, coerceToJavaByte(initialContents.elt(i)));
        }
      } else {
          type_error(initialContents, Symbol.SEQUENCE);
      }
      elements = newElements;

    } else {
      if (elements == null) {
        // Displaced array. Copy existing elements.
        if (directAllocation) {
          elements = ByteBuffer.allocateDirect(newCapacity);
        } else {
          elements = ByteBuffer.allocate(newCapacity);
        }
        final int limit = Math.min(capacity, newCapacity);
        for (int i = 0; i < limit; i++) {
          elements.put(i, coerceToJavaByte(array.AREF(displacement + i)));
        }
      } else if (capacity != newCapacity) {
        ByteBuffer newElements = null;
        if (directAllocation) {
          newElements = ByteBuffer.allocateDirect(newCapacity);
        } else {
          newElements = ByteBuffer.allocate(newCapacity);
        }
        newElements.put(elements.array(), 0, 
                        Math.min(capacity, newCapacity));
        elements = newElements;
      }
      // Initialize new elements (if applicable).
      if (initialElement != null) {
        byte b = coerceToJavaByte(initialElement);
        for (int i = capacity; i < newCapacity; i++) {
          elements.put(i, b);
        }
      }
    }
    capacity = newCapacity;
    array = null;
    displacement = 0;
    isDisplaced = false;
    return this;
  }

    @Override
      public AbstractVector adjustArray(int newCapacity,
                                        AbstractArray displacedTo,
                                        int displacement) {
      capacity = newCapacity;
      array = displacedTo;
      this.displacement = displacement;
      elements = null;
      isDisplaced = true;
      return this;
    }
  }
