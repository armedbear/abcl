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

import static org.armedbear.lisp.Lisp.*;

public class SlotClass extends LispClass
{
    private LispObject directSlotDefinitions = NIL;
    private LispObject slotDefinitions = NIL;
    private LispObject directDefaultInitargs = NIL;
    private LispObject defaultInitargs = NIL;

    public SlotClass(Layout layout)
    {
      super(layout);
    }

    public SlotClass(Symbol symbol, LispObject directSuperclasses)


    {
        this(null, symbol, directSuperclasses);
    }

    public SlotClass(Layout layout,
                     Symbol symbol, LispObject directSuperclasses)
    {
        super(layout, symbol, directSuperclasses);
    }

    @Override
    public LispObject getParts()
    {
        LispObject result = super.getParts().nreverse();
        result = result.push(new Cons("DIRECT-SLOTS",
                                      getDirectSlotDefinitions()));
        result = result.push(new Cons("SLOTS", getSlotDefinitions()));
        result = result.push(new Cons("DIRECT-DEFAULT-INITARGS",
                                      getDirectDefaultInitargs()));
        result = result.push(new Cons("DEFAULT-INITARGS",
                                      getDefaultInitargs()));
        return result.nreverse();
    }

    @Override
    public LispObject typep(LispObject type)
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

    public LispObject getSlotDefinitions()
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

    public LispObject getDefaultInitargs()
    {
        return defaultInitargs;
    }

    public void setDefaultInitargs(LispObject defaultInitargs)
    {
        this.defaultInitargs = defaultInitargs;
    }

    LispObject computeDefaultInitargs()
    {
      // KLUDGE (rudi 2012-06-02): duplicate initargs are not removed
      // here, but this does not hurt us since no Lisp class we define
      // Java-side has non-nil direct default initargs.
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

        LispObject defs = getSlotDefinitions();
        Debug.assertTrue(defs == NIL);
        LispObject cpl = getCPL();
        Debug.assertTrue(cpl != null);
        Debug.assertTrue(cpl.listp());
        cpl = cpl.reverse();
        while (cpl != NIL) {
            LispObject car = cpl.car();
            if (car instanceof StandardClass) {
                StandardClass cls = (StandardClass) car;
                LispObject directDefs = cls.getDirectSlotDefinitions();
                Debug.assertTrue(directDefs != null);
                Debug.assertTrue(directDefs.listp());
                while (directDefs != NIL) {
                    defs = defs.push(directDefs.car());
                    directDefs = directDefs.cdr();
                }
            }
            cpl = cpl.cdr();
        }
        setSlotDefinitions(defs.nreverse());
        LispObject[] instanceSlotNames = new LispObject[defs.length()];
        int i = 0;
        LispObject tail = getSlotDefinitions();
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

    @DocString(name="%class-direct-slots")
    private static final Primitive CLASS_DIRECT_SLOTS 
        = new pf__class_direct_slots();
    private static final class pf__class_direct_slots extends Primitive
    {
        pf__class_direct_slots() 
        {
            super("%class-direct-slots", PACKAGE_SYS, true);
        }
        @Override
        public LispObject execute(LispObject arg)

        {
            if (arg instanceof SlotClass)
                return ((SlotClass)arg).getDirectSlotDefinitions();
            if (arg instanceof BuiltInClass)
                return NIL;
            return type_error(arg, Symbol.STANDARD_CLASS);
        }
    };

    @DocString(name="%set-class-direct-slots")
    private static final Primitive _SET_CLASS_DIRECT_SLOT
        = new pf__set_class_direct_slots();
    private static final class pf__set_class_direct_slots extends Primitive
    {
        pf__set_class_direct_slots() 
        {
            super("%set-class-direct-slots", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof SlotClass) {
                  ((SlotClass)second).setDirectSlotDefinitions(first);
                return first;
            } else {
                return type_error(second, Symbol.STANDARD_CLASS);
            }
        }
    };

    @DocString(name="%class-slots",
               args="class")
    private static final Primitive _CLASS_SLOTS 
        = new pf__class_slots();
    private static final class pf__class_slots extends Primitive
    {
        pf__class_slots() 
        {
            super(Symbol._CLASS_SLOTS, "class");
        }

        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof SlotClass)
                return ((SlotClass)arg).getSlotDefinitions();
            if (arg instanceof BuiltInClass)
                return NIL;
            return type_error(arg, Symbol.STANDARD_CLASS);
        }
    };

    @DocString(name="%set-class-slots",
               args="class slot-definitions")
    private static final Primitive _SET_CLASS_SLOTS 
        = new pf__set_class_slots();
    private static final class pf__set_class_slots extends Primitive
    {
        pf__set_class_slots()
        {
            super(Symbol._SET_CLASS_SLOTS, "class slot-definitions");
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof SlotClass) {
              ((SlotClass)second).setSlotDefinitions(first);
              return first;
            } else {
              return type_error(second, Symbol.STANDARD_CLASS);
            }
        }
    };

    @DocString(name="%class-direct-default-initargs")
    private static final Primitive CLASS_DIRECT_DEFAULT_INITARGS 
        = new pf__class_direct_default_initargs();
    private static final class pf__class_direct_default_initargs extends Primitive
    {
        pf__class_direct_default_initargs() 
        {
            super("%class-direct-default-initargs", PACKAGE_SYS, true);
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof SlotClass)
                return ((SlotClass)arg).getDirectDefaultInitargs();
            if (arg instanceof BuiltInClass)
                return NIL;
            return type_error(arg, Symbol.STANDARD_CLASS);
        }
    };

    @DocString(name="%set-class-direct-default-initargs")
    private static final Primitive _SET_CLASS_DIRECT_DEFAULT_INITARGS 
        = new pf__set_class_direct_default_initargs();
    private static final class pf__set_class_direct_default_initargs extends Primitive
    {
        pf__set_class_direct_default_initargs()
        {
            super("%set-class-direct-default-initargs", PACKAGE_SYS, true);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof SlotClass) {
                ((SlotClass)second).setDirectDefaultInitargs(first);
                return first;
            }
            return type_error(second, Symbol.STANDARD_CLASS);
        }
    };

    @DocString(name="%class-default-initargs")
    private static final Primitive CLASS_DEFAULT_INITARGS 
        = new pf__class_default_initargs();
    private static final class pf__class_default_initargs extends Primitive 
    {
        pf__class_default_initargs() 
        {
            super("%class-default-initargs", PACKAGE_SYS, true);
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof SlotClass)
                return ((SlotClass)arg).getDefaultInitargs();
            if (arg instanceof BuiltInClass)
                return NIL;
            return type_error(arg, Symbol.STANDARD_CLASS);
        }
    };

    @DocString(name="%set-class-default-initargs")
    private static final Primitive _SET_CLASS_DEFAULT_INITARGS 
        = new pf__set_class_default_initargs();

    private static final class pf__set_class_default_initargs extends Primitive
    {
        pf__set_class_default_initargs()
        {
            super("%set-class-default-initargs", PACKAGE_SYS, true);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof SlotClass) {
                ((SlotClass)second).setDefaultInitargs(first);
                return first;
            }
            return type_error(second, Symbol.STANDARD_CLASS);
        }
    };
}
