/*
 * jclass_of.java
 *
 * Copyright (C) 2005 Peter Graves
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

// ### jclass-of object &optional name
public final class jclass_of extends Primitive
{
    private jclass_of()
    {
        super(Symbol.JCLASS_OF, "object &optional name",
"Returns the name of the Java class of OBJECT. If the NAME argument is\n" +
"  supplied, verifies that OBJECT is an instance of the named class. The name\n" +
"  of the class or nil is always returned as a second value.");
    }

    @Override
    public LispObject execute(LispObject arg)

    {
        final String className;
        if (arg instanceof AbstractString)
            className = "java.lang.String";
        else if (arg instanceof JavaObject)
            className = ((JavaObject)arg).getObject().getClass().getName();
        else
            className = null;
        final LispObject value =
            (className != null) ? new SimpleString(className) : NIL;
        return LispThread.currentThread().setValues(value, value);
    }

    @Override
    public LispObject execute(LispObject first, LispObject second)

    {
        final String className;
        if (first instanceof AbstractString)
            className = "java.lang.String";
        else if (first instanceof JavaObject)
            className = ((JavaObject)first).getObject().getClass().getName();
        else
            className = null;
        String name = javaString(second);
        return LispThread.currentThread().setValues(name.equals(className) ? T : NIL,
                                                    new SimpleString(className));
    }

    private static final Primitive JCLASS_OF = new jclass_of();
}
