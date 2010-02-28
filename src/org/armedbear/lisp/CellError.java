/*
 * CellError.java
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

public class CellError extends LispError
{
    protected CellError(LispClass cls)
    {
        super(cls);
    }

    public CellError(LispObject initArgs)
    {
        super(StandardClass.CELL_ERROR);
        initialize(initArgs);
    }

    @Override
    protected void initialize(LispObject initArgs)
    {
        super.initialize(initArgs);
        LispObject name = NIL;
        while (initArgs != NIL) {
            LispObject first = initArgs.car();
            initArgs = initArgs.cdr();
            if (first == Keyword.NAME) {
                name = initArgs.car();
                break;
            }
            initArgs = initArgs.cdr();
        }
        setCellName(name);
    }

    public final LispObject getCellName()
    {
        return getInstanceSlotValue(Symbol.NAME);
    }

    protected final void setCellName(LispObject name)
    {
        setInstanceSlotValue(Symbol.NAME, name);
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.CELL_ERROR;
    }

    @Override
    public LispObject classOf()
    {
        return StandardClass.CELL_ERROR;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.CELL_ERROR)
            return T;
        if (type == StandardClass.CELL_ERROR)
            return T;
        return super.typep(type);
    }

    @Override
    public String getMessage()
    {
        if (Symbol.PRINT_ESCAPE.symbolValue() == NIL)
            return super.getMessage();
        StringBuffer sb = new StringBuffer(typeOf().writeToString());
        sb.append(' ');
        sb.append(getCellName().writeToString());
        return unreadableString(sb.toString());
    }
}
