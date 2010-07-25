/*
 * Primitive.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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

public class Primitive extends Function
{
    public Primitive(LispObject name)
    {
        super(name);
    }

    public Primitive(String name)
    {
        super(name);
    }

    public Primitive(Symbol symbol)
    {
        super(symbol);
    }

    public Primitive(Symbol symbol, String arglist)
    {
        super(symbol, arglist);
    }

    public Primitive(Symbol symbol, String arglist, String docstring)
    {
        super(symbol, arglist, docstring);
    }

    public Primitive(String name, String arglist)
    {
        super(name, arglist);
    }

    public Primitive(LispObject name, LispObject lambdaList)
    {
        super(name, lambdaList);
    }

    public Primitive(String name, Package pkg)
    {
        super(name, pkg);
    }

    public Primitive(String name, Package pkg, boolean exported)
    {
        super(name, pkg, exported);
    }

    public Primitive(String name, Package pkg, boolean exported,
                     String arglist)
    {
        super(name, pkg, exported, arglist);
    }

    public Primitive(String name, Package pkg, boolean exported,
                     String arglist, String docstring)
    {
        super(name, pkg, exported, arglist, docstring);
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.COMPILED_FUNCTION;
    }

    @Override
    public LispObject execute()
    {
        LispObject[] args = new LispObject[0];
        return execute(args);
    }

    @Override
    public LispObject execute(LispObject arg)
    {
        LispObject[] args = new LispObject[1];
        args[0] = arg;
        return execute(args);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second)

    {
        LispObject[] args = new LispObject[2];
        args[0] = first;
        args[1] = second;
        return execute(args);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third)

    {
        LispObject[] args = new LispObject[3];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        return execute(args);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth)

    {
        LispObject[] args = new LispObject[4];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        return execute(args);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth,
                              LispObject fifth)

    {
        LispObject[] args = new LispObject[5];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        args[4] = fifth;
        return execute(args);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth,
                              LispObject fifth, LispObject sixth)

    {
        LispObject[] args = new LispObject[6];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        args[4] = fifth;
        args[5] = sixth;
        return execute(args);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth,
                              LispObject fifth, LispObject sixth,
                              LispObject seventh)

    {
        LispObject[] args = new LispObject[7];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        args[4] = fifth;
        args[5] = sixth;
        args[6] = seventh;
        return execute(args);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third, LispObject fourth,
                              LispObject fifth, LispObject sixth,
                              LispObject seventh, LispObject eighth)

    {
        LispObject[] args = new LispObject[8];
        args[0] = first;
        args[1] = second;
        args[2] = third;
        args[3] = fourth;
        args[4] = fifth;
        args[5] = sixth;
        args[6] = seventh;
        args[7] = eighth;
        return execute(args);
    }
}
