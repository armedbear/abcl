/*
 * SimpleString.java
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

public final class SimpleString extends AbstractString
{
    private int capacity;
    private char[] chars;

    public SimpleString(LispCharacter c)
    {
        chars = new char[1];
        chars[0] = c.value;
        capacity = 1;
    }

    public SimpleString(char c)
    {
        chars = new char[1];
        chars[0] = c;
        capacity = 1;
    }

    public SimpleString(int capacity)
    {
        this.capacity = capacity;
        chars = new char[capacity];
    }

    public SimpleString(String s)
    {
        capacity = s.length();
        chars = s.toCharArray();
    }

    public SimpleString(StringBuffer sb)
    {
        chars = new char[capacity = sb.length()];
        sb.getChars(0, capacity, chars, 0);
    }

    public SimpleString(StringBuilder sb)
    {
        chars = sb.toString().toCharArray();
        capacity = chars.length;
    }

    public SimpleString(char[] chars)
    {
        this.chars = chars;
        capacity = chars.length;
    }

    @Override
    public char[] chars()
    {
        return chars;
    }

    @Override
    public char[] getStringChars()
    {
        return chars;
    }

    @Override
    public LispObject typeOf()
    {
        return list(Symbol.SIMPLE_BASE_STRING, Fixnum.getInstance(capacity));
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.SIMPLE_BASE_STRING;
    }

    @Override
    public LispObject getDescription()
    {
        StringBuilder sb = new StringBuilder("A simple-string (");
        sb.append(capacity);
        sb.append(") \"");
        sb.append(chars);
        sb.append('"');
        return new SimpleString(sb);
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.SIMPLE_STRING)
            return T;
        if (type == Symbol.SIMPLE_ARRAY)
            return T;
        if (type == Symbol.SIMPLE_BASE_STRING)
            return T;
        if (type == BuiltInClass.SIMPLE_STRING)
            return T;
        if (type == BuiltInClass.SIMPLE_ARRAY)
            return T;
        if (type == BuiltInClass.SIMPLE_BASE_STRING)
            return T;
        return super.typep(type);
    }

    @Override
    public LispObject SIMPLE_STRING_P()
    {
        return T;
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
    public boolean equal(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof SimpleString) {
            SimpleString string = (SimpleString) obj;
            if (string.capacity != capacity)
                return false;
            for (int i = capacity; i-- > 0;)
                if (string.chars[i] != chars[i])
                    return false;
            return true;
        }
        if (obj instanceof AbstractString) {
            AbstractString string = (AbstractString) obj;
            if (string.length() != capacity)
                return false;
            for (int i = length(); i-- > 0;)
                if (string.charAt(i) != chars[i])
                    return false;
            return true;
        }
        if (obj instanceof NilVector)
            return obj.equal(this);
        return false;
    }

    @Override
    public boolean equalp(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof SimpleString) {
            SimpleString string = (SimpleString) obj;
            if (string.capacity != capacity)
                return false;
            for (int i = capacity; i-- > 0;) {
                if (string.chars[i] != chars[i]) {
                    if (LispCharacter.toLowerCase(string.chars[i]) != LispCharacter.toLowerCase(chars[i]))
                        return false;
                }
            }
            return true;
        }
        if (obj instanceof AbstractString) {
            AbstractString string = (AbstractString) obj;
            if (string.length() != capacity)
                return false;
            for (int i = length(); i-- > 0;) {
                if (string.charAt(i) != chars[i]) {
                    if (LispCharacter.toLowerCase(string.charAt(i)) != LispCharacter.toLowerCase(chars[i]))
                        return false;
                }
            }
            return true;
        }
        if (obj instanceof AbstractBitVector)
            return false;
        if (obj instanceof AbstractArray)
            return obj.equalp(this);
        return false;
    }

    public final SimpleString substring(int start)
    {
        return substring(start, capacity);
    }

    public final SimpleString substring(int start, int end)

    {
        SimpleString s = new SimpleString(end - start);
        int i = start, j = 0;
        try {
            while (i < end)
                s.chars[j++] = chars[i++];
            return s;
        }
        catch (ArrayIndexOutOfBoundsException e) {
            error(new TypeError("Array index out of bounds: " + i));
            // Not reached.
            return null;
        }
    }

    @Override
    public final LispObject subseq(int start, int end)
    {
        return substring(start, end);
    }

    @Override
    public void fill(LispObject obj)
    {
        fill(LispCharacter.getValue(obj));
    }

    @Override
    public void fill(char c)
    {
        for (int i = capacity; i-- > 0;)
            chars[i] = c;
    }

    @Override
    public void shrink(int n)
    {
        if (n < capacity) {
            char[] newArray = new char[n];
            System.arraycopy(chars, 0, newArray, 0, n);
            chars = newArray;
            capacity = n;
            return;
        }
        if (n == capacity)
            return;
        error(new LispError());
    }

    @Override
    public LispObject reverse()
    {
        SimpleString result = new SimpleString(capacity);
        int i, j;
        for (i = 0, j = capacity - 1; i < capacity; i++, j--)
            result.chars[i] = chars[j];
        return result;
    }

    @Override
    public LispObject nreverse()
    {
        int i = 0;
        int j = capacity - 1;
        while (i < j) {
            char temp = chars[i];
            chars[i] = chars[j];
            chars[j] = temp;
            ++i;
            --j;
        }
        return this;
    }

    @Override
    public String getStringValue()
    {
        return String.valueOf(chars);
    }

    @Override
    public Object javaInstance()
    {
        return String.valueOf(chars);
    }

    @Override
    public Object javaInstance(Class c)
    {
        return javaInstance();
    }

    @Override
    public final int capacity()
    {
        return capacity;
    }

    @Override
    public final int length()
    {
        return capacity;
    }

    @Override
    public char charAt(int index)
    {
        try {
            return chars[index];
        }
        catch (ArrayIndexOutOfBoundsException e) {
            badIndex(index, capacity);
            return 0; // Not reached.
        }
    }

    @Override
    public void setCharAt(int index, char c)
    {
        try {
            chars[index] = c;
        }
        catch (ArrayIndexOutOfBoundsException e) {
            badIndex(index, capacity);
        }
    }

    @Override
    public LispObject elt(int index)
    {
        try {
            return LispCharacter.getInstance(chars[index]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            badIndex(index, capacity);
            return NIL; // Not reached.
        }
    }

    @Override
    public LispObject CHAR(int index)
    {
        try {
            return LispCharacter.getInstance(chars[index]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            badIndex(index, capacity);
            return NIL; // Not reached.
        }
    }

    @Override
    public LispObject SCHAR(int index)
    {
        try {
            return LispCharacter.getInstance(chars[index]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            badIndex(index, capacity);
            return NIL; // Not reached.
        }
    }

    @Override
    public LispObject AREF(int index)
    {
        try {
            return LispCharacter.getInstance(chars[index]);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            badIndex(index, capacity);
            return NIL; // Not reached.
        }
    }


    @Override
    public void aset(int index, LispObject obj)
    {
        try {
            chars[index] = LispCharacter.getValue(obj);
        }
        catch (ArrayIndexOutOfBoundsException e) {
            badIndex(index, capacity);
        }
    }

    @Override
    public int sxhash()
    {
        int hashCode = randomStringHashBase;
        for (int i = 0; i < capacity; i++) {
            hashCode += chars[i];
            hashCode += (hashCode << 10);
            hashCode ^= (hashCode >> 6);
        }
        hashCode += (hashCode << 3);
        hashCode ^= (hashCode >> 11);
        hashCode += (hashCode << 15);
        return (hashCode & 0x7fffffff);
        }

    // For EQUALP hash tables.
    @Override
    public int psxhash()
    {
        int hashCode = randomStringHashBase;
        for (int i = 0; i < capacity; i++) {
            hashCode += Character.toUpperCase(chars[i]);
            hashCode += (hashCode << 10);
            hashCode ^= (hashCode >> 6);
        }
        hashCode += (hashCode << 3);
        hashCode ^= (hashCode >> 11);
        hashCode += (hashCode << 15);
        return (hashCode & 0x7fffffff);
    }

    @Override
    public AbstractVector adjustArray(int newCapacity,
                                       LispObject initialElement,
                                       LispObject initialContents)

    {
        if (initialContents != null) {
            char[] newChars = new char[newCapacity];
            if (initialContents.listp()) {
                LispObject list = initialContents;
                for (int i = 0; i < newCapacity; i++) {
                    newChars[i] = LispCharacter.getValue(list.car());
                    list = list.cdr();
                }
            } else if (initialContents.vectorp()) {
                for (int i = 0; i < newCapacity; i++)
                    newChars[i] = LispCharacter.getValue(initialContents.elt(i));
            } else
                type_error(initialContents, Symbol.SEQUENCE);
            return new SimpleString(newChars);
        }
        if (capacity != newCapacity) {
            char[] newChars = new char[newCapacity];
            System.arraycopy(chars, 0, newChars, 0, Math.min(newCapacity, capacity));
            if (initialElement != null && capacity < newCapacity) {
                final char c = LispCharacter.getValue(initialElement);
                for (int i = capacity; i < newCapacity; i++)
                    newChars[i] = c;
            }
            return new SimpleString(newChars);
        }
        // No change.
        return this;
    }

    @Override
    public AbstractVector adjustArray(int newCapacity,
                                       AbstractArray displacedTo,
                                       int displacement)

    {
        return new ComplexString(newCapacity, displacedTo, displacement);
    }

    @Override
    public String toString()  {
        return String.valueOf(chars);
    }
}
