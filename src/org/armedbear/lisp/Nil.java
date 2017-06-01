/*
 * Nil.java
 *
 * Copyright (C) 2002-2006 Peter Graves
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

public final class Nil extends Symbol
{
    final public static Symbol NIL = new Nil(PACKAGE_CL);

    public Nil(Package pkg)
    {
        super("NIL", pkg);
        pkg.addSymbol(this);
        initializeConstant(this);
    }

    @Override
    public Object javaInstance()
    {
        return null;
    }

    @Override
    public Object javaInstance(Class c)
    {
        if (c == Boolean.class || c == boolean.class)
            return Boolean.FALSE;
        return javaInstance();
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.NULL;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.NULL;
    }

    @Override
    public LispObject getDescription()
    {
        return new SimpleString("The symbol NIL");
    }

    @Override
    public boolean getBooleanValue()
    {
        return false;
    }

    @Override
    public LispObject typep(LispObject typeSpecifier)
    {
        if (typeSpecifier == Symbol.NULL)
            return T;
        if (typeSpecifier == Symbol.LIST)
            return T;
        if (typeSpecifier == Symbol.SEQUENCE)
            return T;
        if (typeSpecifier == Symbol.SYMBOL)
            return T;
        if (typeSpecifier == Symbol.BOOLEAN)
            return T;
        if (typeSpecifier == BuiltInClass.NULL)
            return T;
        if (typeSpecifier == BuiltInClass.LIST)
            return T;
        if (typeSpecifier == BuiltInClass.SEQUENCE)
            return T;
        if (typeSpecifier == BuiltInClass.SYMBOL)
            return T;
        return super.typep(typeSpecifier);
    }

    @Override
    public boolean constantp()
    {
        return true;
    }

    @Override
    public final LispObject getSymbolValue()
    {
        return this;
    }

    @Override
    public int length()
    {
        return 0;
    }

    @Override
    public LispObject NTH(int index)
    {
        if (index < 0)
            error(new TypeError(String.valueOf(index) +
                                 " is not of type UNSIGNED-BYTE."));
        return NIL;
    }

    @Override
    public LispObject elt(int index)
    {
        return error(new TypeError("ELT: invalid index " + index + " for " + this + "."));
    }

    @Override
    public LispObject reverse()
    {
        return this;
    }

    @Override
    public LispObject nreverse()
    {
        return this;
    }

    @Override
    public LispObject[] copyToArray()
    {
        return new LispObject[0];
    }

    @Override
    public LispObject NOT()
    {
        return T;
    }

    @Override
    public final LispObject getSymbolFunction()
    {
        return null;
    }

    public Object readResolve() throws java.io.ObjectStreamException {
       return NIL;
    }

}
