/*
 * BasicVector_ShortBuffer.java
 *
 * Copyright (C) 2020 @easye
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
import java.nio.CharBuffer;

/** Underlying storage for (unsigned-byte 16) */
public final class BasicVector_CharBuffer 
  extends AbstractVector
{
  private int capacity;
  private CharBuffer elements;

  public BasicVector_CharBuffer(int capacity) {
    elements = CharBuffer.allocate(capacity);
    this.capacity = capacity;  // TODO use elements.limit()?
  }

  public BasicVector_CharBuffer(LispObject[] array) {
    capacity = array.length;
    elements = CharBuffer.allocate(capacity);
    for (int i = array.length; i-- > 0;) {
      elements.put(i, (char)Fixnum.getValue(array[i])); // FIXME bulk copy
    }
  }

  public BasicVector_CharBuffer(ByteBuffer buffer) {
    elements = buffer.asCharBuffer();
    capacity = buffer.limit();
  }

  public BasicVector_CharBuffer(CharBuffer buffer) {
    elements = buffer;
    capacity = buffer.limit();
  }

  // ### ext:make-charbuffer-byte-vector BYTEBUFFER
  public static final Primitive MAKE_CHARBUFFER_BYTE_VECTOR
    = new pf_make_charbuffer_byte_vector();
  private static final class pf_make_charbuffer_byte_vector extends Primitive {
    pf_make_charbuffer_byte_vector() {
      super(Symbol.MAKE_CHARBUFFER_BYTE_VECTOR, "bytebuffer",
            "Construct a simple vector with element type (unsigned 16) from a java.nio BYTEBUFFER");
    }
    @Override
    public LispObject execute(LispObject arg) {
      return new BasicVector_CharBuffer(coerceToCharBuffer(arg));
    }
  }

  static public CharBuffer coerceToCharBuffer(LispObject arg) {
    JavaObject javaObject = (JavaObject) arg;
    Object o = javaObject.getObject();
    if (o instanceof ByteBuffer) {
      return ((ByteBuffer)o).asCharBuffer();
    } else {
      return (CharBuffer) o;
    }
  }


  @Override
  public LispObject typeOf() {
    return list(Symbol.SIMPLE_ARRAY, UNSIGNED_BYTE_16,
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
    return UNSIGNED_BYTE_16;
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
      return Fixnum.getInstance(elements.get(index));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, capacity);
      return NIL; // Not reached.
    }
  }

  // Ignores fill pointer.
  @Override
  public int aref(int index) {
    try {
      return elements.get(index);
    } catch (ArrayIndexOutOfBoundsException e) {
      badIndex(index, elements.limit()); // FIXME should implement method for length() contract
      // Not reached.
      return 0;
    }
  }

  // Ignores fill pointer.
  @Override
  public LispObject AREF(int index) {
    try {
      return Fixnum.getInstance(elements.get(index));
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, elements.limit());  // FIXME limit() --> capacity?
      return NIL; // Not reached.
    }
  }

  @Override
  public void aset(int index, int n) {
    try {
      elements.put(index, (char)n);
    } catch (IndexOutOfBoundsException e) {
      badIndex(index, capacity);
    }
  }

  @Override
  public void aset(int index, LispObject obj) {
    if (obj instanceof Fixnum) {
      try {
        elements.put(index, (char)((Fixnum)obj).value);
      } catch (ArrayIndexOutOfBoundsException e) {
        badIndex(index, capacity);
      }
    } else {
      type_error(obj, UNSIGNED_BYTE_16);
    }
  }

  @Override
  public LispObject subseq(int start, int end) {
    BasicVector_CharBuffer v = new BasicVector_CharBuffer(end - start);
    int i = start, j = 0;
    try {
      while (i < end) {
        v.elements.put(j++, elements.get(i++));
      }
      return v;
    } catch (IndexOutOfBoundsException e) {
      return error(new TypeError("Array index out of bounds: " + i + "."));
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
    if (n < 0 || n > 65535) {
      type_error(obj, UNSIGNED_BYTE_16);
      // Not reached.
      return;
    }
    char m = (char) n;
    for (int i = capacity; i-- > 0;) {
      elements.put(i, m); // FASTER!!!
    }
  }

  @Override
  public void shrink(int n) {
    if (n < capacity) {
      CharBuffer newBuffer = CharBuffer.allocate(n);
      newBuffer.put(elements.array(), 0, n);
      elements = newBuffer;
      capacity = n;
      return;
    }
    if (n == capacity) {
      return;
    }
    error(new LispError("End of native shrink routine:  shouldn't be reachable."));
  }

  @Override
  public LispObject reverse() {
    BasicVector_CharBuffer result = new BasicVector_CharBuffer(capacity);
    int i, j;
    for (i = 0, j = capacity - 1; i < capacity; i++, j--){
      result.elements.put(i, elements.get(j));
    }
    return result;
  }

  @Override
  public LispObject nreverse() {
    int i = 0;
    int j = capacity - 1;
    while (i < j) {
      char temp = elements.get(i);
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
        for (int i = 0; i < newCapacity; i++)
          newElements[i] = initialContents.elt(i);
      } else
        type_error(initialContents, Symbol.SEQUENCE);
      return new BasicVector_CharBuffer(newElements);
    }
    if (capacity != newCapacity) { // FIXME: more efficient
      LispObject[] newElements = new LispObject[newCapacity];
      System.arraycopy(elements.array(), 0, newElements, 0,
                       Math.min(capacity, newCapacity));
      if (initialElement != null) {
        for (int i = capacity; i < newCapacity; i++)
          newElements[i] = initialElement;
      }
      return new BasicVector_CharBuffer(newElements);
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



