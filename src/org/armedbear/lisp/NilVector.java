/*
 * NilVector.java
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

public final class NilVector extends AbstractString
{
    private int capacity;

    public NilVector(int capacity)
    {
        this.capacity = capacity;
    }

    @Override
    public char[] chars()
    {
        if (capacity != 0)
            accessError();
        return new char[0];
    }

    @Override
    public char[] getStringChars()
    {
        if (capacity != 0)
            accessError();
        return new char[0];
    }

    @Override
    public String getStringValue()
    {
        if (capacity != 0)
            accessError();
        return "";
    }

    @Override
    public LispObject typeOf()
    {
        return list(Symbol.NIL_VECTOR, Fixnum.getInstance(capacity));
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.NIL_VECTOR;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.NIL_VECTOR)
            return T;
        if (type == Symbol.SIMPLE_STRING)
            return T;
        if (type == Symbol.SIMPLE_ARRAY)
            return T;
        if (type == BuiltInClass.NIL_VECTOR)
            return T;
        if (type == BuiltInClass.SIMPLE_STRING)
            return T;
        if (type == BuiltInClass.SIMPLE_ARRAY)
            return T;
        return super.typep(type);
    }

    @Override
    public LispObject SIMPLE_STRING_P()
    {
        return T;
    }

    @Override
    public boolean equal(LispObject obj)
    {
        if (obj instanceof NilVector) {
            if (capacity != ((NilVector)obj).capacity)
                return false;
            if (capacity != 0) {
                accessError();
                // Not reached.
                return false;
            }
            return true;
        }
        if (obj instanceof AbstractString) {
            if (capacity != obj.length())
                return false;
            if (capacity != 0) {
                accessError();
                // Not reached.
                return false;
            }
            return true;
        }
        return false;
    }

    public String getValue()
    {
        if (capacity == 0)
            return "";
        accessError();
        // Not reached.
        return null;
    }

    @Override
    public int length()
    {
        return capacity;
    }

    @Override
    public int capacity()
    {
        return capacity;
    }

    @Override
    public LispObject getElementType()
    {
        return NIL;
    }

    @Override
    public LispObject CHAR(int index)
    {
        return accessError();
    }

    @Override
    public LispObject SCHAR(int index)
    {
        return accessError();
    }

    @Override
    public LispObject AREF(int index)
    {
        return accessError();
    }

    @Override
    public void aset(int index, LispObject newValue)
    {
        storeError(newValue);
    }

    @Override
    public char charAt(int index)
    {
        accessError();
        // Not reached.
        return 0;
    }

    @Override
    public void setCharAt(int index, char c)
    {
        storeError(LispCharacter.getInstance(c));
    }

    @Override
    public LispObject subseq(int start, int end)
    {
        if (capacity == 0 && start == 0 && end == 0)
            return this;
        return accessError();
    }

    @Override
    public void fill(LispObject obj)
    {
        storeError(obj);
    }

    @Override
    public void fill(char c)
    {
        storeError(LispCharacter.getInstance(c));
    }

    @Override
    public void shrink(int n)
    {
    }

    @Override
    public LispObject reverse()
    {
        return accessError();
    }

    public LispObject accessError()
    {
        return error(new TypeError("Attempt to access an array of element type NIL."));
    }

    private void storeError(LispObject obj)
    {
        error(new TypeError(String.valueOf(obj) + " is not of type NIL."));
    }

    @Override
    public int sxhash()
    {
        return 0;
    }

    @Override
    public AbstractVector adjustArray(int newCapacity,
                                       LispObject initialElement,
                                       LispObject initialContents)

    {
        accessError();
        // Not reached.
        return null;
    }

    @Override
    public AbstractVector adjustArray(int size, AbstractArray displacedTo,
                                       int displacement)

    {
        accessError();
        // Not reached.
        return null;
    }
}
