/*
 * BasicVector_ByteBuffer.java
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
import java.nio.BufferOverflowException;


// A basic vector is a specialized vector that is not displaced to another
// array, has no fill pointer, and is not expressly adjustable.
public final class BasicVector_ByteBuffer
  extends AbstractVector
{
  private int capacity;
  private ByteBuffer elements;
  private boolean directAllocation;

  public BasicVector_ByteBuffer(int capacity) {
    this(capacity, false);
  }
  
  public BasicVector_ByteBuffer(int capacity, boolean directAllocation) {
    this.directAllocation = directAllocation;
    if (directAllocation) {
      elements = ByteBuffer.allocateDirect(capacity);
    } else {
      elements = ByteBuffer.allocate(capacity);
    }
    this.capacity = capacity;
  }

  public BasicVector_ByteBuffer(byte[] array, boolean directAllocation) {
    capacity = array.length;
    this.directAllocation = directAllocation; 
    elements = ByteBuffer.wrap(array);
    // ??? Note somehow that we were constructed from a wrapped primitive array 
  }
  
  public BasicVector_ByteBuffer(LispObject[] array, boolean directAllocation) {
    // FIXME: for now we assume that we're being handled an array of
    // primitive bytes
    this.directAllocation = directAllocation;
    capacity = array.length;
    if (directAllocation) {
      elements = ByteBuffer.allocateDirect(array.length);
    } else {
      elements = ByteBuffer.allocate(array.length);
    }
    for (int i = array.length; i-- > 0;) {
      // Faster please!
      elements.put(i, (byte)coerceToJavaByte(array[i]));
    }
  }

  public BasicVector_ByteBuffer(ByteBuffer buffer, boolean directAllocation) {
    elements = buffer;
    this.directAllocation = directAllocation;
    capacity = ((java.nio.Buffer)buffer).limit();  
  }

  @Override
  public LispObject typeOf() {
    return list(Symbol.SIMPLE_ARRAY, UNSIGNED_BYTE_8,
                new Cons(Fixnum.getInstance(capacity)));
  }

  @Override
  public LispObject classOf() {
    return BuiltInClass.VECTOR;
  }

  @Override
  public LispObject typep(LispObject type) {
    if (type == Symbol.SIMPLE_ARRAY)
      return T;
    if (type == BuiltInClass.SIMPLE_ARRAY)
      return T;
    return super.typep(type);
  }

  @Override
  public LispObject getElementType() {
    return UNSIGNED_BYTE_8;
  }

  @Override
  public boolean isSimpleVector() {
    return false;
  }

  @Override
  public boolean hasFillPointer() {
    return false;
  }

  @Override
  public boolean isAdjustable() {
    return false;
  }

  @Override
  public int capacity() {
    return capacity;
  }

  @Override
  public int length() {
    return capacity;
  }

  @Override
  public LispObject elt(int index) {
    try {
      return coerceFromJavaByte(elements.get(index));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, capacity);
      return NIL; // Not reached.
    }
  }

  @Override
  public int aref(int index) {
    try {
      return (((int)elements.get(index) & 0xff)); // XXX Hmmm
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, ((java.nio.Buffer)elements).limit()); 
      // Not reached.
      return 0;
    }
  }

  @Override
  public LispObject AREF(int index) {
    try {
      return coerceFromJavaByte(elements.get(index));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, ((java.nio.Buffer)elements).limit()); 
      return NIL; // Not reached.
    }
  }

  @Override
  public void aset(int index, int n) {
    try {
      elements.put(index, (byte) n);
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, capacity);
    }
  }

  @Override
  public void aset(int index, LispObject value) {
    try {
        elements.put(index, coerceToJavaByte(value));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, capacity);
    }
  }

  @Override
  public LispObject subseq(int start, int end) {
    // ??? Do we need to check that start, end are valid?
    BasicVector_ByteBuffer v = new BasicVector_ByteBuffer(end - start, directAllocation);
    ByteBuffer view = elements.asReadOnlyBuffer();
    ((java.nio.Buffer)view).position(start);
    ((java.nio.Buffer)view).limit(end);
    try {
      v.elements.put(view);
      v.elements.position(0);
      return v;
    } catch (BufferOverflowException e) {
      return error(new TypeError("Could not form a subseq from " + start + " to " + end));
    }
  }

  @Override
  public void fill(LispObject obj) {
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

    for (int i = length(); i-- > 0;) {
      elements.put(i, (byte) n);
    }
  }

  @Override
  public void shrink(int n) {
    // One cannot shrink a ByteBuffer physically, and the elements
    // field may refer to malloc()d memory that we shouldn't touch, so
    // use the java.nio.Buffer limit pointer.  Not totally sure that
    // this strategy will work out…
    if (n < length()) {
        ((java.nio.Buffer)elements).limit(n);
        capacity = n;
        return;
    }
    if (n == length()) {
      return;
    }
    error(new LispError("Attempted to shrink an array to a size greater than its capacity"));
  }

  @Override
  public LispObject reverse() {
    BasicVector_ByteBuffer result = new BasicVector_ByteBuffer(length(), directAllocation);
    int i, j;
    for (i = 0, j = length() - 1; i < length(); i++, j--) {
      result.elements.put(i, elements.get(j));
    }
    return result;
  }

  @Override
  public LispObject nreverse() {
    int i = 0;
    int j = capacity() - 1;
    while (i < j) {
      byte temp = elements.get(i);
      elements.put(i, elements.get(j));
      elements.put(j, temp);
      ++i;
      --j;
    }
    return this;
  }

  @Override
  public AbstractVector adjustArray(int newCapacity,
                                    LispObject initialElement,
                                    LispObject initialContents) {
    if (initialContents != null) {
      ByteBuffer newElements;
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
        for (int i = 0; i < newCapacity; i++)
          newElements.put(i, coerceToJavaByte(initialContents.elt(i)));
      } else
        type_error(initialContents, Symbol.SEQUENCE);
      return new BasicVector_ByteBuffer(newElements, directAllocation);
    }
    if (length() != newCapacity) {
      ByteBuffer newElements;
      if (directAllocation) {
        newElements = ByteBuffer.allocateDirect(newCapacity);
      } else {
        newElements = ByteBuffer.allocate(newCapacity);
      }

      if (elements.hasArray()) {
        newElements.put(elements.array(), 0, Math.min(length(), newCapacity));
      } else {
        // FIXME: a more efficient version when we don't have a backing array
        int limit = Math.min(length(), newCapacity);
        for (int i = 0; i < limit; i++) {
          newElements.put(i, elements.get(i));
        }
      }
        
      if (initialElement != null) {
        byte initValue = (byte)(initialElement.intValue() & 0xFF);
        for (int i = length(); i < newCapacity; i++)
          newElements.put(i, initValue);
      }
      return new BasicVector_ByteBuffer(newElements, directAllocation);
    }
    // No change.
    return this;
  }

  @Override
  public AbstractVector adjustArray(int newCapacity,
                                    AbstractArray displacedTo,
                                    int displacement) {
    return new ComplexVector(newCapacity, displacedTo, displacement);
  }
}
