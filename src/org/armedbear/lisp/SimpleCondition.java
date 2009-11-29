/*
 * SimpleCondition.java
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

public class SimpleCondition extends Condition
{
    public SimpleCondition()
    {
        setFormatControl(NIL);
        setFormatArguments(NIL);
    }

    public SimpleCondition(LispObject formatControl, LispObject formatArguments)

    {
        setFormatControl(formatControl);
        setFormatArguments(formatArguments);
    }

    public SimpleCondition(LispObject initArgs)
    {
        super(initArgs);
    }

    public SimpleCondition(String message)
    {
        super(message);
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.SIMPLE_CONDITION;
    }

    @Override
    public LispObject classOf()
    {
        return StandardClass.SIMPLE_CONDITION;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.SIMPLE_CONDITION)
            return T;
        if (type == StandardClass.SIMPLE_CONDITION)
            return T;
        return super.typep(type);
    }

    // ### simple-condition-format-control
    private static final Primitive SIMPLE_CONDITION_FORMAT_CONTROL =
        new Primitive(Symbol.SIMPLE_CONDITION_FORMAT_CONTROL, "condition")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return Symbol.STD_SLOT_VALUE.execute(arg, Symbol.FORMAT_CONTROL);
        }
    };

    // ### simple-condition-format-arguments
    private static final Primitive SIMPLE_CONDITION_FORMAT_ARGUMENTS =
        new Primitive(Symbol.SIMPLE_CONDITION_FORMAT_ARGUMENTS, "condition")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return Symbol.STD_SLOT_VALUE.execute(arg, Symbol.FORMAT_ARGUMENTS);
        }
    };
}
