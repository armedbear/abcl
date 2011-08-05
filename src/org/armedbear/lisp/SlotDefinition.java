/*
 * SlotDefinition.java
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

public final class SlotDefinition extends StandardObject
{
  public SlotDefinition()
  {
    super(StandardClass.STANDARD_SLOT_DEFINITION,
          StandardClass.STANDARD_SLOT_DEFINITION.getClassLayout().getLength());
    slots[SlotDefinitionClass.SLOT_INDEX_LOCATION] = NIL;
  }

    public SlotDefinition(StandardClass clazz) {
        super(clazz, clazz.getClassLayout().getLength());
        slots[SlotDefinitionClass.SLOT_INDEX_LOCATION] = NIL;
    }

    public SlotDefinition(StandardClass clazz, LispObject name) {
        super(clazz, clazz.getClassLayout().getLength());
        slots[SlotDefinitionClass.SLOT_INDEX_NAME] = name;
        slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION] = NIL;
        slots[SlotDefinitionClass.SLOT_INDEX_INITARGS] =
            new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName()));
        slots[SlotDefinitionClass.SLOT_INDEX_READERS] = NIL;
        slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION] = Keyword.INSTANCE;
        slots[SlotDefinitionClass.SLOT_INDEX_LOCATION] = NIL;
    }

  public SlotDefinition(LispObject name, LispObject readers)
  {
    this();
    Debug.assertTrue(name instanceof Symbol);
    slots[SlotDefinitionClass.SLOT_INDEX_NAME] = name;
    slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION] = NIL;
    slots[SlotDefinitionClass.SLOT_INDEX_INITARGS] =
      new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName()));
    slots[SlotDefinitionClass.SLOT_INDEX_READERS] = readers;
    slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION] = Keyword.INSTANCE;
  }

  public SlotDefinition(LispObject name, LispObject readers,
                        LispObject initForm)
  {
    this();
    Debug.assertTrue(name instanceof Symbol);
    slots[SlotDefinitionClass.SLOT_INDEX_NAME] = name;
    slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION] = NIL;
    slots[SlotDefinitionClass.SLOT_INDEX_INITFORM] = initForm;
    slots[SlotDefinitionClass.SLOT_INDEX_INITARGS] =
      new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName()));
    slots[SlotDefinitionClass.SLOT_INDEX_READERS] = readers;
    slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION] = Keyword.INSTANCE;
  }

  public SlotDefinition(LispObject name, LispObject readers,
                        Function initFunction)
  {
    this();
    Debug.assertTrue(name instanceof Symbol);
    slots[SlotDefinitionClass.SLOT_INDEX_NAME] = name;
    slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION] = initFunction;
    slots[SlotDefinitionClass.SLOT_INDEX_INITFORM] = NIL;
    slots[SlotDefinitionClass.SLOT_INDEX_INITARGS] =
      new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName()));
    slots[SlotDefinitionClass.SLOT_INDEX_READERS] = readers;
    slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION] = Keyword.INSTANCE;
  }

  public static StandardObject checkSlotDefinition(LispObject obj) {
          if (obj instanceof StandardObject) return (StandardObject)obj;
      return (StandardObject)type_error(obj, Symbol.SLOT_DEFINITION);
  }

  public final LispObject getName()
  {
    return slots[SlotDefinitionClass.SLOT_INDEX_NAME];
  }

  public final void setLocation(int i)
  {
    slots[SlotDefinitionClass.SLOT_INDEX_LOCATION] = Fixnum.getInstance(i);
  }

  @Override
  public String printObject()
  {
    StringBuilder sb =
      new StringBuilder(Symbol.SLOT_DEFINITION.printObject());
    LispObject name = slots[SlotDefinitionClass.SLOT_INDEX_NAME];
    if (name != null && name != NIL)
      {
        sb.append(' ');
        sb.append(name.printObject());
      }
    return unreadableString(sb.toString());
  }

  // ### make-slot-definition &optional class
  private static final Primitive MAKE_SLOT_DEFINITION =
    new Primitive("make-slot-definition", PACKAGE_SYS, true, "&optional class")
    {
      @Override
      public LispObject execute()
      {
        return new SlotDefinition();
      }
      @Override
      public LispObject execute(LispObject slotDefinitionClass)
      {
          return new SlotDefinition((StandardClass) slotDefinitionClass);
      }
    };

  // ### %slot-definition-name
  private static final Primitive _SLOT_DEFINITION_NAME =
    new Primitive(Symbol._SLOT_DEFINITION_NAME, "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_NAME];
      }
    };

  // ### set-slot-definition-name
  private static final Primitive SET_SLOT_DEFINITION_NAME =
    new Primitive("set-slot-definition-name", PACKAGE_SYS, true,
                  "slot-definition name")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_NAME] = second;
          return second;
      }
    };

  // ### %slot-definition-initfunction
  private static final Primitive _SLOT_DEFINITION_INITFUNCTION =
    new Primitive(Symbol._SLOT_DEFINITION_INITFUNCTION, "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION];
      }
    };

  // ### set-slot-definition-initfunction
  static final Primitive SET_SLOT_DEFINITION_INITFUNCTION =
    new Primitive("set-slot-definition-initfunction", PACKAGE_SYS, true,
                  "slot-definition initfunction")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION] = second;
          return second;
      }
    };

  // ### %slot-definition-initform
  private static final Primitive _SLOT_DEFINITION_INITFORM =
    new Primitive("%slot-definition-initform", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_INITFORM];
      }
    };

  // ### set-slot-definition-initform
  static final Primitive SET_SLOT_DEFINITION_INITFORM =
    new Primitive("set-slot-definition-initform", PACKAGE_SYS, true,
                  "slot-definition initform")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_INITFORM] = second;
          return second;
      }
    };

  // ### %slot-definition-initargs
  private static final Primitive _SLOT_DEFINITION_INITARGS =
    new Primitive(Symbol._SLOT_DEFINITION_INITARGS, "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_INITARGS];
      }
    };

  // ### set-slot-definition-initargs
  private static final Primitive SET_SLOT_DEFINITION_INITARGS =
    new Primitive("set-slot-definition-initargs", PACKAGE_SYS, true,
                  "slot-definition initargs")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_INITARGS] = second;
          return second;
      }
    };

  // ### %slot-definition-readers
  private static final Primitive _SLOT_DEFINITION_READERS =
    new Primitive("%slot-definition-readers", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_READERS];
      }
    };

  // ### set-slot-definition-readers
  private static final Primitive SET_SLOT_DEFINITION_READERS =
    new Primitive("set-slot-definition-readers", PACKAGE_SYS, true,
                  "slot-definition readers")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_READERS] = second;
          return second;
      }
    };

  // ### %slot-definition-writers
  private static final Primitive _SLOT_DEFINITION_WRITERS =
    new Primitive("%slot-definition-writers", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_WRITERS];
      }
    };

  // ### set-slot-definition-writers
  private static final Primitive SET_SLOT_DEFINITION_WRITERS =
    new Primitive("set-slot-definition-writers", PACKAGE_SYS, true,
                  "slot-definition writers")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_WRITERS] = second;
          return second;
      }
    };

  // ### %slot-definition-allocation
  private static final Primitive _SLOT_DEFINITION_ALLOCATION =
    new Primitive("%slot-definition-allocation", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION];
      }
    };

  // ### set-slot-definition-allocation
  private static final Primitive SET_SLOT_DEFINITION_ALLOCATION =
    new Primitive("set-slot-definition-allocation", PACKAGE_SYS, true,
                  "slot-definition allocation")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION] = second;
          return second;
      }
    };

  // ### %slot-definition-allocation-class
  private static final Primitive _SLOT_DEFINITION_ALLOCATION_CLASS =
    new Primitive("%slot-definition-allocation-class", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION_CLASS];
      }
    };

  // ### set-slot-definition-allocation-class
  private static final Primitive SET_SLOT_DEFINITION_ALLOCATION_CLASS =
    new Primitive("set-slot-definition-allocation-class", PACKAGE_SYS, true,
                  "slot-definition allocation-class")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION_CLASS] = second;
          return second;
      }
    };

  // ### %slot-definition-location
  private static final Primitive _SLOT_DEFINITION_LOCATION =
    new Primitive("%slot-definition-location", PACKAGE_SYS, true, "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkSlotDefinition(arg).slots[SlotDefinitionClass.SLOT_INDEX_LOCATION];
      }
    };

  // ### set-slot-definition-location
  private static final Primitive SET_SLOT_DEFINITION_LOCATION =
    new Primitive("set-slot-definition-location", PACKAGE_SYS, true, "slot-definition location")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkSlotDefinition(first).slots[SlotDefinitionClass.SLOT_INDEX_LOCATION] = second;
          return second;
      }
    };
}
