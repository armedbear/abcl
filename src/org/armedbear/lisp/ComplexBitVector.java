/*
 * ComplexBitVector.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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

public final class ComplexBitVector extends AbstractBitVector
{
    private int fillPointer = -1; // -1 indicates no fill pointer.
    private boolean isDisplaced;

    // For displaced bit vectors.
    private AbstractArray array;
    private int displacement;

    public ComplexBitVector(int capacity)
    {
        this.capacity = capacity;
        int size = capacity >>> 6;
        if ((capacity & LONG_MASK) != 0)
            ++size;
        bits = new long[size];
    }

    public ComplexBitVector(int capacity, AbstractArray array, int displacement)
    {
        this.capacity = capacity;
        this.array = array;
        this.displacement = displacement;
        isDisplaced = true;
    }

    @Override
    public LispObject typeOf()
    {
        return list(Symbol.BIT_VECTOR, Fixnum.getInstance(capacity));
    }

    @Override
    public boolean hasFillPointer()
    {
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
    public int length()
    {
        return fillPointer >= 0 ? fillPointer : capacity;
    }

    @Override
    public LispObject elt(int index)
    {
        if (index >= length())
            badIndex(index, length());
        return AREF(index);
    }

    @Override
    public LispObject AREF(int index)
    {
        if (index < 0 || index >= capacity)
            badIndex(index, capacity);
        if (bits != null) {
            int offset = index >> 6;
            return (bits[offset] & (1L << index)) != 0 ? Fixnum.ONE : Fixnum.ZERO;
        } else {
            // Displaced bit vector.
            return array.AREF(index + displacement);
        }
    }

    @Override
    protected int getBit(int index)
    {
        if (bits != null) {
            int offset = index >> 6;
            return (bits[offset] & (1L << index)) != 0 ? 1 : 0;
        } else
            return Fixnum.getValue(array.AREF(index + displacement));
    }

    @Override
    public void aset(int index, LispObject newValue)
    {
        if (index < 0 || index >= capacity)
            badIndex(index, capacity);
        if (newValue instanceof Fixnum) {
            switch (((Fixnum)newValue).value) {
                case 0:
                    if (bits != null) {
                        final int offset = index >> 6;
                        bits[offset] &= ~(1L << index);
                    } else
                        clearBit(index);
                    return;
                case 1:
                    if (bits != null) {
                        final int offset = index >> 6;
                        bits[offset] |= 1L << index;
                    } else
                        setBit(index);
                    return;
            }
        }
            // Fall through...
        type_error(newValue, Symbol.BIT);
    }

    @Override
    protected void setBit(int index)
    {
        if (bits != null) {
            int offset = index >> 6;
            bits[offset] |= 1L << index;
        } else
            array.aset(index + displacement, Fixnum.ONE);
    }

    @Override
    protected void clearBit(int index)
    {
        if (bits != null) {
            int offset = index >> 6;
            bits[offset] &= ~(1L << index);
        } else
            array.aset(index + displacement, Fixnum.ZERO);
    }

    @Override
    public void shrink(int n)
    {
        if (bits != null) {
            if (n < capacity) {
                int size = n >>> 6;
                if ((n & LONG_MASK) != 0)
                    ++size;
                if (size < bits.length) {
                    long[] newbits = new long[size];
                    System.arraycopy(bits, 0, newbits, 0, size);
                    bits = newbits;
                }
                capacity = n;
                return;
            }
            if (n == capacity)
                return;
        }
        error(new LispError());
    }

    @Override
    public boolean isSimpleVector()
    {
        return false;
    }

    // FIXME
    @Override
    public void vectorPushExtend(LispObject element)
    {
        final int fp = getFillPointer();
        if (fp < 0)
            noFillPointer();
        if (fp >= capacity()) {
            // Need to extend vector.
            ensureCapacity(capacity() * 2 + 1);
        }
        aset(fp, element);
        setFillPointer(fp + 1);
    }

    // FIXME
    @Override
    public LispObject VECTOR_PUSH_EXTEND(LispObject element)

    {
        vectorPushExtend(element);
        return Fixnum.getInstance(getFillPointer() - 1);
    }

    // FIXME
    @Override
    public LispObject VECTOR_PUSH_EXTEND(LispObject element, LispObject extension)

    {
        int ext = Fixnum.getValue(extension);
        final int fp = getFillPointer();
        if (fp < 0)
            noFillPointer();
        if (fp >= capacity()) {
            // Need to extend vector.
            ext = Math.max(ext, capacity() + 1);
            ensureCapacity(capacity() + ext);
        }
        aset(fp, element);
        setFillPointer(fp + 1);
        return Fixnum.getInstance(fp);
    }

    private final void ensureCapacity(int minCapacity)
    {
        if (bits != null) {
            if (capacity < minCapacity) {
                int size = minCapacity >>> 6;
                if ((minCapacity & LONG_MASK) != 0)
                    ++size;
                long[] newBits = new long[size];
                System.arraycopy(bits, 0, newBits, 0, bits.length);
                bits = newBits;
                capacity = minCapacity;
            }
        } else {
            Debug.assertTrue(array != null);
            if (capacity < minCapacity ||
                array.getTotalSize() - displacement < minCapacity)
            {
                // Copy array.
                int size = minCapacity >>> 6;
                if ((minCapacity & LONG_MASK) != 0)
                    ++size;
                bits = new long[size];
                final int limit =
                    Math.min(capacity, array.getTotalSize() - displacement);
                for (int i = 0; i < limit; i++) {
                    int n = Fixnum.getValue(array.AREF(displacement + i));
                    if (n == 1)
                        setBit(i);
                    else
                        clearBit(i);
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
                                       LispObject initialContents)

    {
        if (bits == null) {
            // Copy array.
            int size = capacity >>> 6;
            if ((capacity & LONG_MASK) != 0)
                ++size;
            bits = new long[size];
            for (int i = 0; i < capacity; i++) {
                int n = Fixnum.getValue(array.AREF(displacement + i));
                if (n == 1)
                    setBit(i);
                else
                    clearBit(i);
            }
            array = null;
            displacement = 0;
            isDisplaced = false;
        }
        if (capacity != newCapacity) {
            int size = newCapacity >>> 6;
            if ((newCapacity & LONG_MASK) != 0)
                ++size;
            if (initialContents != null) {
                bits = new long[size];
                capacity = newCapacity;
                if (initialContents.listp()) {
                    LispObject list = initialContents;
                    for (int i = 0; i < newCapacity; i++) {
                        aset(i, list.car());
                        list = list.cdr();
                    }
                } else if (initialContents.vectorp()) {
                    for (int i = 0; i < newCapacity; i++)
                        aset(i, initialContents.elt(i));
                } else
                    type_error(initialContents, Symbol.SEQUENCE);
            } else {
                long[] newBits = new long[size];
                System.arraycopy(bits, 0, newBits, 0,
                                 Math.min(bits.length, newBits.length));
                bits = newBits;
                if (newCapacity > capacity && initialElement != null) {
                    int n = Fixnum.getValue(initialElement);
                    if (n == 1)
                        for (int i = capacity; i < newCapacity; i++)
                            setBit(i);
                    else
                        for (int i = capacity; i < newCapacity; i++)
                            clearBit(i);
                }
            }
            capacity = newCapacity;
        }
        return this;
    }

    @Override
    public AbstractVector adjustArray(int size, AbstractArray displacedTo,
                                       int displacement)

    {
        capacity = size;
        array = displacedTo;
        this.displacement = displacement;
        bits = null;
        isDisplaced = true;
        return this;
    }
}
