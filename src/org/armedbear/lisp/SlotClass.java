/*
 * SlotClass.java
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

public class SlotClass extends LispClass
{
    private LispObject directSlotDefinitions = NIL;
    private LispObject slotDefinitions = NIL;
    private LispObject directDefaultInitargs = NIL;
    private LispObject defaultInitargs = NIL;

    public SlotClass()
    {
    }

    public SlotClass(Symbol symbol, LispObject directSuperclasses)
    {
        super(symbol, directSuperclasses);
    }

    @Override
    public LispObject getParts() throws ConditionThrowable
    {
        LispObject result = super.getParts().nreverse();
        result = result.push(new Cons("DIRECT-SLOTS", directSlotDefinitions));
        result = result.push(new Cons("SLOTS", slotDefinitions));
        result = result.push(new Cons("DIRECT-DEFAULT-INITARGS", directDefaultInitargs));
        result = result.push(new Cons("DEFAULT-INITARGS", defaultInitargs));
        return result.nreverse();
    }

    @Override
    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        return super.typep(type);
    }

    public LispObject getDirectSlotDefinitions()
    {
        return directSlotDefinitions;
    }

    public void setDirectSlotDefinitions(LispObject directSlotDefinitions)
    {
        this.directSlotDefinitions = directSlotDefinitions;
    }

    public final LispObject getSlotDefinitions()
    {
        return slotDefinitions;
    }

    public void setSlotDefinitions(LispObject slotDefinitions)
    {
        this.slotDefinitions = slotDefinitions;
    }

    public LispObject getDirectDefaultInitargs()
    {
        return directDefaultInitargs;
    }

    public void setDirectDefaultInitargs(LispObject directDefaultInitargs)
    {
        this.directDefaultInitargs = directDefaultInitargs;
    }

    public void setDefaultInitargs(LispObject defaultInitargs)
    {
        this.defaultInitargs = defaultInitargs;
    }

    private LispObject computeDefaultInitargs() throws ConditionThrowable
    {
        LispObject result = NIL;
        LispObject cpl = getCPL();
        while (cpl != NIL) {
            LispClass c = (LispClass) cpl.car();
            if (c instanceof StandardClass) {
                LispObject obj = ((StandardClass)c).getDirectDefaultInitargs();
                if (obj != NIL)
                    result = Symbol.APPEND.execute(result, obj);
            }
            cpl = cpl.cdr();
        }
        return result;
    }

    public void finalizeClass()
    {
        if (isFinalized())
            return;
        try {
            Debug.assertTrue(slotDefinitions == NIL);
            LispObject cpl = getCPL();
            Debug.assertTrue(cpl != null);
            Debug.assertTrue(cpl.listp());
            cpl = cpl.reverse();
            while (cpl != NIL) {
                LispObject car = cpl.car();
                if (car instanceof StandardClass) {
                    StandardClass cls = (StandardClass) car;
                    LispObject defs = cls.getDirectSlotDefinitions();
                    Debug.assertTrue(defs != null);
                    Debug.assertTrue(defs.listp());
                    while (defs != NIL) {
                        slotDefinitions = slotDefinitions.push(defs.car());
                        defs = defs.cdr();
                    }
                }
                cpl = cpl.cdr();
            }
            slotDefinitions = slotDefinitions.nreverse();
            LispObject[] instanceSlotNames = new LispObject[slotDefinitions.length()];
            int i = 0;
            LispObject tail = slotDefinitions;
            while (tail != NIL) {
                SlotDefinition slotDefinition = (SlotDefinition) tail.car();
                slotDefinition.setLocation(i);
                instanceSlotNames[i++] = slotDefinition.getName();
                tail = tail.cdr();
            }
            setClassLayout(new Layout(this, instanceSlotNames, NIL));
            setDefaultInitargs(computeDefaultInitargs());
            setFinalized(true);
        }
        catch (Throwable t) {
            Debug.trace(t);
        }
    }

    // ### class-direct-slots
    private static final Primitive CLASS_DIRECT_SLOTS =
        new Primitive("class-direct-slots", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
            throws ConditionThrowable
        {
            if (arg instanceof SlotClass)
                return ((SlotClass)arg).directSlotDefinitions;
            if (arg instanceof BuiltInClass)
                return NIL;
            return type_error(arg, Symbol.STANDARD_CLASS);
        }
    };

    // ### %set-class-direct-slots
    private static final Primitive _SET_CLASS_DIRECT_SLOTS =
        new Primitive("%set-class-direct-slots", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            try {
                ((SlotClass)first).directSlotDefinitions = second;
                return second;
            }
            catch (ClassCastException e) {
                return type_error(first, Symbol.STANDARD_CLASS);
            }
        }
    };

    // ### %class-slots
    private static final Primitive _CLASS_SLOTS =
        new Primitive(Symbol._CLASS_SLOTS, "class")
    {
        @Override
        public LispObject execute(LispObject arg)
            throws ConditionThrowable
        {
            if (arg instanceof SlotClass)
                return ((SlotClass)arg).slotDefinitions;
            if (arg instanceof BuiltInClass)
                return NIL;
            return type_error(arg, Symbol.STANDARD_CLASS);
        }
    };

    // ### set-class-slots
    private static final Primitive SET_CLASS_SLOTS =
        new Primitive(Symbol.SET_CLASS_SLOTS, "class slot-definitions")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            try {
                ((SlotClass)first).slotDefinitions = second;
                return second;
            }
            catch (ClassCastException e) {
                return type_error(first, Symbol.STANDARD_CLASS);
            }
        }
    };

    // ### class-direct-default-initargs
    private static final Primitive CLASS_DIRECT_DEFAULT_INITARGS =
        new Primitive("class-direct-default-initargs", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
            throws ConditionThrowable
        {
            if (arg instanceof SlotClass)
                return ((SlotClass)arg).directDefaultInitargs;
            if (arg instanceof BuiltInClass)
                return NIL;
            return type_error(arg, Symbol.STANDARD_CLASS);
        }
    };

    // ### %set-class-direct-default-initargs
    private static final Primitive _SET_CLASS_DIRECT_DEFAULT_INITARGS =
        new Primitive("%set-class-direct-default-initargs", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            try {
                ((SlotClass)first).directDefaultInitargs = second;
                return second;
            }
            catch (ClassCastException e) {
                return type_error(first, Symbol.STANDARD_CLASS);
            }
        }
    };

    // ### class-default-initargs
    private static final Primitive CLASS_DEFAULT_INITARGS =
        new Primitive("class-default-initargs", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
            throws ConditionThrowable
        {
            if (arg instanceof SlotClass)
                return ((SlotClass)arg).defaultInitargs;
            if (arg instanceof BuiltInClass)
                return NIL;
            return type_error(arg, Symbol.STANDARD_CLASS);
        }
    };

    // ### %set-class-default-initargs
    private static final Primitive _SET_CLASS_DEFAULT_INITARGS =
        new Primitive("%set-class-default-initargs", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            if (first instanceof SlotClass) {
                ((SlotClass)first).defaultInitargs = second;
                return second;
            }
            return type_error(first, Symbol.STANDARD_CLASS);
        }
    };

    // ### compute-class-default-initargs
    private static final Primitive COMPUTE_CLASS_DEFAULT_INITARGS =
        new Primitive("compute-class-default-initargs", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
            throws ConditionThrowable
        {
            final SlotClass c;
            try {
                c = (SlotClass) arg;
            }
            catch (ClassCastException e) {
                return type_error(arg, Symbol.STANDARD_CLASS);
            }
            return c.computeDefaultInitargs();
        }
    };
}
