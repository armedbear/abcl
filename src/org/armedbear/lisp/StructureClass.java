/*
 * StructureClass.java
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

public class StructureClass extends SlotClass
{
    private StructureClass(Symbol symbol)
    {
        super(symbol, new Cons(BuiltInClass.STRUCTURE_OBJECT));
    }

    public StructureClass(Symbol symbol, LispObject directSuperclasses)
    {
        super(symbol, directSuperclasses);
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.STRUCTURE_CLASS;
    }

    @Override
    public LispObject classOf()
    {
        return StandardClass.STRUCTURE_CLASS;
    }

    @Override
    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.STRUCTURE_CLASS)
            return T;
        if (type == StandardClass.STRUCTURE_CLASS)
            return T;
        return super.typep(type);
    }

    @Override
    public LispObject getDescription() throws ConditionThrowable
    {
        return new SimpleString(writeToString());
    }

    @Override
    public String writeToString() throws ConditionThrowable
    {
        StringBuffer sb = new StringBuffer("#<STRUCTURE-CLASS ");
        sb.append(symbol.writeToString());
        sb.append('>');
        return sb.toString();
    }

    // ### make-structure-class name direct-slots slots include => class
    private static final Primitive MAKE_STRUCTURE_CLASS =
        new Primitive("make-structure-class", PACKAGE_SYS, false)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth)
            throws ConditionThrowable
        {
            Symbol symbol = checkSymbol(first);
            LispObject directSlots = checkList(second);
            LispObject slots = checkList(third);
            Symbol include = checkSymbol(fourth);
            LispClass existingClass = LispClass.findClass(symbol);
            StructureClass c;

            if (existingClass instanceof StructureClass)
                // Change the existing class definition if there is one.
                // The compiler has this scenario, where it is first loaded
                // and subsequently run through the file compiler - which
                // re-creates the same structure and breaks the inheritance
                // if we don't re-use the existing class. Reusing the
                // existing class is alright in this case, since we're
                // recreating the same class.
                // Redefinition of structures is undefined in the CLHS.
                // As per the DEFSTRUCT-REDEFINITION it is allowed, but
                // consequences are undefined.
                c = (StructureClass)existingClass;
            else
                c = new StructureClass(symbol);
            if (include != NIL) {
                LispClass includedClass = LispClass.findClass(include);
                if (includedClass == null)
                    return error(new SimpleError("Class " + include +
                                                  " is undefined."));
                c.setCPL(new Cons(c, includedClass.getCPL()));
            } else
                c.setCPL(c, BuiltInClass.STRUCTURE_OBJECT, BuiltInClass.CLASS_T);
            c.setDirectSlotDefinitions(directSlots);
            c.setSlotDefinitions(slots);
            addClass(symbol, c);
            return c;
        }
    };
}
