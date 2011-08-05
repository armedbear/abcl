/*
 * AbstractBitVector.java
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

public abstract class AbstractBitVector extends AbstractVector
{
    protected static final int LONG_MASK = 0x3f;

    protected int capacity;

    // For non-displaced bit-vectors.
    protected long[] bits;

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.BIT_VECTOR)
            return T;
        if (type == BuiltInClass.BIT_VECTOR)
            return T;
        return super.typep(type);
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.BIT_VECTOR;
    }

    @Override
    public final int capacity()
    {
        return capacity;
    }

    @Override
    public final LispObject getElementType()
    {
        return Symbol.BIT;
    }

    @Override
    public boolean equal(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof AbstractBitVector) {
            AbstractBitVector v = (AbstractBitVector) obj;
            if (length() != v.length())
                return false;
            for (int i = length(); i-- > 0;) {
                if (getBit(i) != v.getBit(i))
                    return false;
            }
            return true;
        }
        return false;
    }

    @Override
    public boolean equalp(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof AbstractBitVector) {
            AbstractBitVector v = (AbstractBitVector) obj;
            if (length() != v.length())
                return false;
            for (int i = length(); i-- > 0;) {
                if (getBit(i) != v.getBit(i))
                    return false;
            }
            return true;
        }
        if (obj instanceof AbstractString)
            return false;
        if (obj instanceof AbstractVector)
            return ((AbstractVector)obj).equalp(this);
        return false;
    }

    @Override
    public void fill(LispObject obj)
    {
        if (obj instanceof Fixnum) {
            switch (((Fixnum)obj).value) {
                case 0:
                    if (bits != null) {
                        for (int i = bits.length; i-- > 0;)
                            bits[i] = 0;
                    } else {
                        for (int i = capacity; i-- > 0;)
                            clearBit(i);
                    }
                    return;
                case 1:
                    if (bits != null) {
                        for (int i = bits.length; i-- > 0;)
                            bits[i] = -1L;
                    } else {
                        for (int i = capacity; i-- > 0;)
                            setBit(i);
                    }
                    return;
            }
            // Fall through...
        }
        error(new TypeError(obj, Symbol.BIT));
    }

    @Override
    public LispObject subseq(int start, int end)
    {
        SimpleBitVector v = new SimpleBitVector(end - start);
        int i = start, j = 0;
        try {
            while (i < end) {
                if (getBit(i++) == 0)
                    v.clearBit(j++);
                else
                    v.setBit(j++);
            }
            return v;
        }
        catch (ArrayIndexOutOfBoundsException e) {
            return error(new TypeError("Array index out of bounds: " + i + "."));
        }
    }

    @Override
    public int hashCode()
    {
        int hashCode = 1;
        // Consider first 64 bits only.
        final int limit = Math.min(length(), 64);
        for (int i = 0; i < limit; i++)
            hashCode = hashCode * 31 + getBit(i);
        return hashCode;
    }

    @Override
    public String printObject()
    {
        final LispThread thread = LispThread.currentThread();
        final int length = length();
        if (Symbol.PRINT_READABLY.symbolValue(thread) != NIL ||
            Symbol.PRINT_ARRAY.symbolValue(thread) != NIL)
        {
            StringBuilder sb = new StringBuilder(length + 2);
            sb.append("#*");
            for (int i = 0; i < length; i++)
                sb.append(getBit(i) == 1 ? '1' : '0');
            return sb.toString();
        } else {
            final String str = "(%sBIT-VECTOR %d)";
            final String pre = (this instanceof SimpleBitVector) ? "SIMPLE-" : "";
            return unreadableString(String.format(str, pre, length));
        }
    }

    @Override
    public LispObject reverse()
    {
        int length = length();
        SimpleBitVector result = new SimpleBitVector(length);
        int i, j;
        for (i = 0, j = length - 1; i < length; i++, j--) {
            if (getBit(j) == 1)
                result.setBit(i);
            else
                result.clearBit(i);
        }
        return result;
    }

    protected abstract int getBit(int index);

    protected abstract void setBit(int index);

    protected abstract void clearBit(int index);
}
