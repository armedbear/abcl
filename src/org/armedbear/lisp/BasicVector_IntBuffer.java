/*
 * BasicVector_IntBuffer.java
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

import java.nio.IntBuffer;
import java.nio.ByteBuffer;

// A basic vector is a specialized vector that is not displaced to another
// array, has no fill pointer, and is not expressly adjustable.
public final class BasicVector_IntBuffer
  extends AbstractVector
{
  private int capacity;
  private IntBuffer elements;
  private boolean directAllocation;

  public BasicVector_IntBuffer(int capacity) {
    this(capacity, false);
  }

  public BasicVector_IntBuffer(int capacity, boolean directAllocation) {
    this.directAllocation = directAllocation;
    if (directAllocation) {
      ByteBuffer b = ByteBuffer.allocateDirect(capacity * 4);
      elements = b.asIntBuffer();
    } else {
      elements = IntBuffer.allocate(capacity);
    }
    this.capacity = capacity;
  }

  public BasicVector_IntBuffer(LispObject[] array, boolean directAllocation) {
    capacity = array.length;
    this.directAllocation = directAllocation;
    if (directAllocation) {
      ByteBuffer b = ByteBuffer.allocateDirect(capacity * 4);
      elements = b.asIntBuffer();
    } else {
      elements = IntBuffer.allocate(capacity);
    }
    for (int i = array.length; i-- > 0;) {
      // FIXME: if  LispObeject is a number that can't fit into an int
      elements.put(i, (int)(array[i].longValue() & 0xffffffffL));  
    }
  }

  public BasicVector_IntBuffer(ByteBuffer buffer, boolean directAllocation) {
    this.directAllocation = directAllocation;
    elements = buffer.asIntBuffer();
    capacity = ((java.nio.Buffer)buffer).limit() / 4;
  }

  public BasicVector_IntBuffer(IntBuffer buffer) {
    this(buffer, false);
  }

  public BasicVector_IntBuffer(IntBuffer buffer, boolean directAllocation) {
    this.directAllocation = directAllocation;
    elements = buffer;
    capacity = ((java.nio.Buffer)buffer).limit();
  }

  @Override
  public LispObject typeOf() {
    return list(Symbol.SIMPLE_ARRAY, UNSIGNED_BYTE_32,
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
    return UNSIGNED_BYTE_32;
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
      return number(((long)elements.get(index)) & 0xffffffffL);
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, capacity);
      return NIL; // Not reached.
    }
  }

  @Override
  public int aref(int index) {
    try {
      // FIXME: this shouldn't be used?  
      return number(((long)elements.get(index)) & 0xffffffffL).intValue(); 
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, ((java.nio.Buffer)elements).limit()); 
      return -1; // Not reached.
    }
  }

  @Override
  public long aref_long(int index) {
    try {
      return ((long)elements.get(index)) & 0xffffffffL;
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, ((java.nio.Buffer)elements).limit());
      return -1; // Not reached.
    }
  }

  @Override
  public LispObject AREF(int index) {
    try {
      return number(((long)elements.get(index)) & 0xffffffffL);
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, ((java.nio.Buffer)elements).limit());
      return NIL; // Not reached.
    }
  }

  @Override
  public void aset(int index, LispObject newValue) {
    try {
      if (newValue.isLessThan(Fixnum.ZERO) || newValue.isGreaterThan(UNSIGNED_BYTE_32_MAX_VALUE)) {
        type_error(newValue, UNSIGNED_BYTE_32);
      }
      elements.put(index, (int)(newValue.longValue() & 0xffffffffL));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, capacity);
    }
  }

  @Override
  public LispObject subseq(int start, int end) {
    BasicVector_IntBuffer v = new BasicVector_IntBuffer(end - start);
    int i = start, j = 0;
    try {
      while (i < end) {
        v.elements.put(j++, elements.get(i++));
      }
      return v;
    } catch (IndexOutOfBoundsException e) {
      // FIXME
      return error(new TypeError("Array index out of bounds: " + i + "."));
    }
  }

  @Override
  public void fill(LispObject obj) {
    if (!(obj instanceof LispInteger)) {
      type_error(obj, Symbol.INTEGER);
      // Not reached.
      return;
    }
    if (obj.isLessThan(Fixnum.ZERO) || obj.isGreaterThan(UNSIGNED_BYTE_32_MAX_VALUE)) {
      type_error(obj, UNSIGNED_BYTE_32);
    }
    for (int i = capacity; i-- > 0;) {
      elements.put(i, (int)(obj.longValue() & 0xffffffffL));
    }
  }

  @Override
  public void shrink(int n) {
    if (n < length()) {
      // One cannot shrink the underlying ByteBuffer physically, and
      // the elements field may refer to malloc()d memory that we
      // shouldn't touch, so use the java.nio.Buffer limit pointer.
      // Not totally sure that this strategy will work out…
      ((java.nio.Buffer)elements).limit(n);
      capacity = n;
      return;
    }
    if (n == capacity) {
      return;
    }
    error(new LispError());
  }

  @Override
  public LispObject reverse() {
    BasicVector_IntBuffer result = new BasicVector_IntBuffer(capacity);
    int i, j;
    for (i = 0, j = capacity - 1; i < capacity; i++, j--) {
      result.elements.put(i, elements.get(j));
    }
    return result;
  }

  @Override
  public LispObject nreverse() {
    int i = 0;
    int j = capacity - 1;
    while (i < j) {
      int temp = elements.get(i);
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
      LispObject[] newElements = new LispObject[newCapacity];
      if (initialContents.listp()) {
        LispObject list = initialContents;
        for (int i = 0; i < newCapacity; i++) {
          newElements[i] = list.car();
          list = list.cdr();
        }
      } else if (initialContents.vectorp()) {
        for (int i = 0; i < newCapacity; i++) {
          newElements[i] = initialContents.elt(i);
        }
      } else {
        type_error(initialContents, Symbol.SEQUENCE);
      }
      return new BasicVector_IntBuffer(newElements, directAllocation);
    }
    if (capacity != newCapacity) {
      LispObject[] newElements = new LispObject[newCapacity];
      System.arraycopy(elements.array(), 0, newElements, 0,
                       Math.min(capacity, newCapacity));
      if (initialElement != null) {
        for (int i = capacity; i < newCapacity; i++) {
          newElements[i] = initialElement;
        }
      }
      return new BasicVector_IntBuffer(newElements, directAllocation);
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
