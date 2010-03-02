/*
 * SimpleBitVector.java
 *
 * Copyright (C) 2004-2005 Peter Graves
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

// "The type of a bit vector that is not displaced to another array, has no
// fill pointer, and is not expressly adjustable is a subtype of type SIMPLE-
// BIT-VECTOR."
public final class SimpleBitVector extends AbstractBitVector
{
    public SimpleBitVector(int capacity)
    {
        this.capacity = capacity;
        int size = capacity >>> 6; // 64 bits in a long
        // If the capacity is not an integral multiple of 64, we'll need one
        // more long.
        if ((capacity & LONG_MASK) != 0)
            ++size;
        bits = new long[size];
    }

    public SimpleBitVector(String s)
    {
        this(s.length());
        for (int i = capacity; i-- > 0;) {
            char c = s.charAt(i);
            if (c == '0') {
            } else if (c == '1')
                setBit(i);
            else
                Debug.assertTrue(false);
        }
    }

    @Override
    public LispObject typeOf()
    {
        return list(Symbol.SIMPLE_BIT_VECTOR, Fixnum.getInstance(capacity));
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.SIMPLE_BIT_VECTOR;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.SIMPLE_BIT_VECTOR)
            return T;
        if (type == Symbol.SIMPLE_ARRAY)
            return T;
        if (type == BuiltInClass.SIMPLE_BIT_VECTOR)
            return T;
        if (type == BuiltInClass.SIMPLE_ARRAY)
            return T;
        return super.typep(type);
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
    public boolean isSimpleVector()
    {
        return true;
    }

    @Override
    public int length()
    {
        return capacity;
    }

    @Override
    public LispObject elt(int index)
    {
        if (index < 0 || index >= length())
            badIndex(index, length());
        int offset = index >> 6; // Divide by 64.
        return (bits[offset] & (1L << (index & LONG_MASK))) != 0 ? Fixnum.ONE : Fixnum.ZERO;
    }

    @Override
    public LispObject AREF(int index)
    {
        if (index < 0 || index >= capacity)
            badIndex(index, capacity);
        int offset = index >> 6;
        return (bits[offset] & (1L << (index & LONG_MASK))) != 0 ? Fixnum.ONE : Fixnum.ZERO;
    }

    @Override
    public void aset(int index, LispObject newValue)
    {
        if (index < 0 || index >= capacity)
            badIndex(index, capacity);
        final int offset = index >> 6;
        if (newValue instanceof Fixnum) {
            switch (((Fixnum)newValue).value) {
                case 0:
                    bits[offset] &= ~(1L << (index & LONG_MASK));
                    return;
                case 1:
                    bits[offset] |= 1L << (index & LONG_MASK);
                    return;
            }
        }
        // Fall through...
        type_error(newValue, Symbol.BIT);
    }

    @Override
    protected int getBit(int index)
    {
        int offset = index >> 6;
        return (bits[offset] & (1L << (index & LONG_MASK))) != 0 ? 1 : 0;
    }

    @Override
    protected void setBit(int index)
    {
        int offset = index >> 6;
        bits[offset] |= 1L << (index & LONG_MASK);
    }

    @Override
    protected void clearBit(int index)
    {
        int offset = index >> 6;
        bits[offset] &= ~(1L << (index & LONG_MASK));
    }

    @Override
    public void shrink(int n)
    {
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
        error(new LispError());
    }

    @Override
    public AbstractVector adjustArray(int newCapacity,
                                       LispObject initialElement,
                                       LispObject initialContents)

    {
        if (initialContents != null) {
            SimpleBitVector v = new SimpleBitVector(newCapacity);
            if (initialContents.listp()) {
                LispObject list = initialContents;
                for (int i = 0; i < newCapacity; i++) {
                    v.aset(i, list.car());
                    list = list.cdr();
                }
            } else if (initialContents.vectorp()) {
                for (int i = 0; i < newCapacity; i++)
                    v.aset(i, initialContents.elt(i));
            } else
                error(new TypeError(initialContents, Symbol.SEQUENCE));
            return v;
        }
        if (capacity != newCapacity) {
            SimpleBitVector v = new SimpleBitVector(newCapacity);
            final int limit = Math.min(capacity, newCapacity);
            for (int i = limit; i-- > 0;) {
                if (getBit(i) == 1)
                    v.setBit(i);
                else
                    v.clearBit(i);
            }
            if (initialElement != null && capacity < newCapacity) {
                int n = Fixnum.getValue(initialElement);
                if (n == 1)
                    for (int i = capacity; i < newCapacity; i++)
                        v.setBit(i);
                else
                    for (int i = capacity; i < newCapacity; i++)
                        v.clearBit(i);
            }
            return v;
        }
        // No change.
        return this;
    }

    @Override
    public AbstractVector adjustArray(int newCapacity,
                                       AbstractArray displacedTo,
                                       int displacement)

    {
        return new ComplexBitVector(newCapacity, displacedTo, displacement);
    }

    SimpleBitVector and(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = bits[i] & v.bits[i];
        return result;
    }

    SimpleBitVector ior(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = bits[i] | v.bits[i];
        return result;
    }

    SimpleBitVector xor(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = bits[i] ^ v.bits[i];
        return result;
    }

    SimpleBitVector eqv(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = ~(bits[i] ^ v.bits[i]);
        return result;
    }

    SimpleBitVector nand(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = ~(bits[i] & v.bits[i]);
        return result;
    }

    SimpleBitVector nor(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = ~(bits[i] | v.bits[i]);
        return result;
    }

    SimpleBitVector andc1(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = ~bits[i] & v.bits[i];
        return result;
    }

    SimpleBitVector andc2(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = bits[i] & ~v.bits[i];
        return result;
    }

    SimpleBitVector orc1(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = ~bits[i] | v.bits[i];
        return result;
    }

    SimpleBitVector orc2(SimpleBitVector v, SimpleBitVector result)
    {
        if (result == null)
            result = new SimpleBitVector(capacity);
        for (int i = bits.length; i-- > 0;)
            result.bits[i] = bits[i] | ~v.bits[i];
        return result;
    }

    // ### %simple-bit-vector-bit-and
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_AND =
        new Primitive("%simple-bit-vector-bit-and", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).and((SimpleBitVector)second,
                                                ((SimpleBitVector)third));
        }
    };

    // ### %simple-bit-vector-bit-ior
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_IOR =
        new Primitive("%simple-bit-vector-bit-ior", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).ior((SimpleBitVector)second,
                                                (SimpleBitVector)third);

        }
    };

    // ### %simple-bit-vector-bit-xor
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_XOR =
        new Primitive("%simple-bit-vector-bit-xor", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).xor((SimpleBitVector)second,
                                                (SimpleBitVector)third);

        }
    };

    // ### %simple-bit-vector-bit-eqv
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_EQV =
        new Primitive("%simple-bit-vector-bit-eqv", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).eqv((SimpleBitVector)second,
                                                (SimpleBitVector)third);
        }
    };

    // ### %simple-bit-vector-bit-nand
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_NAND =
        new Primitive("%simple-bit-vector-bit-nand", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).nand((SimpleBitVector)second,
                                                 (SimpleBitVector)third);
        }
    };

    // ### %simple-bit-vector-bit-nor
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_NOR =
        new Primitive("%simple-bit-vector-bit-nor", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).nor((SimpleBitVector)second,
                                                 (SimpleBitVector)third);
        }
    };

    // ### %simple-bit-vector-bit-andc1
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_ANDC1 =
        new Primitive("%simple-bit-vector-bit-andc1", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).andc1((SimpleBitVector)second,
                                                  (SimpleBitVector)third);
        }
    };

    // ### %simple-bit-vector-bit-andc2
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_ANDC2 =
        new Primitive("%simple-bit-vector-bit-andc2", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).andc2((SimpleBitVector)second,
                                                  (SimpleBitVector)third);
        }
    };


    // ### %simple-bit-vector-bit-orc1
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_ORC1 =
        new Primitive("%simple-bit-vector-bit-orc1", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).orc1((SimpleBitVector)second,
                                                 (SimpleBitVector)third);
        }
    };

    // ### %simple-bit-vector-bit-orc2
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_ORC2 =
        new Primitive("%simple-bit-vector-bit-orc2", PACKAGE_SYS, false,
                      "bit-vector1 bit-vector2 result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return ((SimpleBitVector)first).orc2((SimpleBitVector)second,
                                                 (SimpleBitVector)third);
        }
    };

    // ### %simple-bit-vector-bit-not
    private static final Primitive _SIMPLE_BIT_VECTOR_BIT_NOT =
        new Primitive("%simple-bit-vector-bit-not", PACKAGE_SYS, false,
                      "bit-vector result-bit-vector")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            SimpleBitVector v = (SimpleBitVector) first;
            SimpleBitVector result = (SimpleBitVector) second;
            for (int i = v.bits.length; i-- > 0;)
                result.bits[i] = ~v.bits[i];
            return result;
        }
    };
}
