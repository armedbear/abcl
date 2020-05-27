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
public final class BasicVector_ByteBuffer extends AbstractVector
{
  private ByteBuffer elements;

  public BasicVector_ByteBuffer(int capacity) {
    elements = ByteBuffer.allocate(capacity);
  }

  public BasicVector_ByteBuffer(byte[] array) {
    elements = ByteBuffer.wrap(array);
  }
  
  public BasicVector_ByteBuffer(ByteBuffer buffer) {
    elements = buffer;
  }

  public BasicVector_ByteBuffer(LispObject[] array) {
    // FIXME: for now we assume that we're being handled an array of
    // primitive bytes
    
    elements = ByteBuffer.allocate(array.length);
    for (int i = array.length; i-- > 0;)
      // Faster please!
      elements.put((byte)coerceLispObjectToJavaByte(array[i]));
  }

  @Override
  public LispObject typeOf() {
    return list(Symbol.SIMPLE_ARRAY, UNSIGNED_BYTE_8,
                new Cons(Fixnum.getInstance(elements.limit())));
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
    return elements.capacity();
  }

  // In order to "shrink" a ByteBuffer without allocating new memory,
  // we use the the limit field to mark the end of the usable portion
  // of the vector
  @Override
  public int length() {
    return elements.limit();
  }

  @Override
  public LispObject elt(int index) {
    try {
      return coerceJavaByteToLispObject(elements.get(index));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, length());
      return NIL; // Not reached.
    }
  }

  @Override
  public int aref(int index) {
    try {
      return ((elements.get(index) & 0xff)); // XXX Hmmm
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, length());
      // Not reached.
      return 0;
    }
  }

  @Override
  public LispObject AREF(int index) {
    try {
      return coerceJavaByteToLispObject(elements.get(index));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, length());
      return NIL; // Not reached.
    }
  }

  @Override
  public void aset(int index, int n) {
    try {
      elements.put(index,  (byte) n);
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, length());
    }
  }

  @Override
  public void aset(int index, LispObject value) {
    try {
        elements.put(index, coerceLispObjectToJavaByte(value));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, length());
    }
  }

  @Override
  public LispObject subseq(int start, int end) {
    BasicVector_ByteBuffer v = new BasicVector_ByteBuffer(end - start);
    ByteBuffer view = elements.asReadOnlyBuffer();
    view.position(start);
    view.limit(end);
    try {
      v.elements.put(view);
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
    // One cannot shrink a ByteBuffer physically, so 
    if (n < length()) {
        elements.limit(n);
        return;
    }
    if (n == length()) {
      return;
    }
    error(new LispError("Attempted to shrink an array to a size greater than its capacity"));
  }

  @Override
  public LispObject reverse() {
    BasicVector_ByteBuffer result = new BasicVector_ByteBuffer(length());
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
      ByteBuffer newElements = ByteBuffer.allocate(newCapacity);
      if (initialContents.listp()) {
        LispObject list = initialContents;
        for (int i = 0; i < newCapacity; i++) {
          newElements.put(i, coerceLispObjectToJavaByte(list.car()));
          list = list.cdr();
        }
      } else if (initialContents.vectorp()) {
        for (int i = 0; i < newCapacity; i++)
          newElements.put(i, coerceLispObjectToJavaByte(initialContents.elt(i)));
      } else
        type_error(initialContents, Symbol.SEQUENCE);
      return new BasicVector_ByteBuffer(newElements);
    }
    if (length() != newCapacity) {
      ByteBuffer newElements = ByteBuffer.allocate(newCapacity);
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
      return new BasicVector_ByteBuffer(newElements);
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
