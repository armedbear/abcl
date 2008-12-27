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

public final class SlotDefinition extends StandardObject
{
  public SlotDefinition()
  {
    super(StandardClass.SLOT_DEFINITION,
          StandardClass.SLOT_DEFINITION.getClassLayout().getLength());
    slots[SlotDefinitionClass.SLOT_INDEX_LOCATION] = NIL;
  }

  public SlotDefinition(LispObject name, LispObject readers)
  {
    this();
    try
      {
        Debug.assertTrue(name instanceof Symbol);
        slots[SlotDefinitionClass.SLOT_INDEX_NAME] = name;
        slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION] = NIL;
        slots[SlotDefinitionClass.SLOT_INDEX_INITARGS] =
          new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName()));
        slots[SlotDefinitionClass.SLOT_INDEX_READERS] = readers;
        slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION] = Keyword.INSTANCE;
      }
    catch (Throwable t)
      {
        Debug.trace(t);
      }
  }

  public SlotDefinition(LispObject name, LispObject readers,
                        LispObject initForm)
  {
    this();
    try
      {
        Debug.assertTrue(name instanceof Symbol);
        slots[SlotDefinitionClass.SLOT_INDEX_NAME] = name;
        slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION] = NIL;
        slots[SlotDefinitionClass.SLOT_INDEX_INITFORM] = initForm;
        slots[SlotDefinitionClass.SLOT_INDEX_INITARGS] =
          new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName()));
        slots[SlotDefinitionClass.SLOT_INDEX_READERS] = readers;
        slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION] = Keyword.INSTANCE;
      }
    catch (Throwable t)
      {
        Debug.trace(t);
      }
  }

  public final LispObject getName()
  {
    return slots[SlotDefinitionClass.SLOT_INDEX_NAME];
  }

  public final void setLocation(int i)
  {
    slots[SlotDefinitionClass.SLOT_INDEX_LOCATION] = new Fixnum(i);
  }

  @Override
  public String writeToString() throws ConditionThrowable
  {
    FastStringBuffer sb =
      new FastStringBuffer(Symbol.SLOT_DEFINITION.writeToString());
    LispObject name = slots[SlotDefinitionClass.SLOT_INDEX_NAME];
    if (name != null && name != NIL)
      {
        sb.append(' ');
        sb.append(name.writeToString());
      }
    return unreadableString(sb.toString());
  }

  // ### make-slot-definition
  private static final Primitive MAKE_SLOT_DEFINITION =
    new Primitive("make-slot-definition", PACKAGE_SYS, true, "")
    {
      @Override
      public LispObject execute() throws ConditionThrowable
      {
        return new SlotDefinition();
      }
    };

  // ### %slot-definition-name
  private static final Primitive _SLOT_DEFINITION_NAME =
    new Primitive(Symbol._SLOT_DEFINITION_NAME, "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_NAME];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-name
  private static final Primitive SET_SLOT_DEFINITION_NAME =
    new Primitive("set-slot-definition-name", PACKAGE_SYS, true,
                  "slot-definition name")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_NAME] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### %slot-definition-initfunction
  private static final Primitive _SLOT_DEFINITION_INITFUNCTION =
    new Primitive(Symbol._SLOT_DEFINITION_INITFUNCTION, "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-initfunction
  private static final Primitive SET_SLOT_DEFINITION_INITFUNCTION =
    new Primitive("set-slot-definition-initfunction", PACKAGE_SYS, true,
                  "slot-definition initfunction")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_INITFUNCTION] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### %slot-definition-initform
  private static final Primitive _SLOT_DEFINITION_INITFORM =
    new Primitive("%slot-definition-initform", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_INITFORM];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-initform
  private static final Primitive SET_SLOT_DEFINITION_INITFORM =
    new Primitive("set-slot-definition-initform", PACKAGE_SYS, true,
                  "slot-definition initform")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_INITFORM] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### %slot-definition-initargs
  private static final Primitive _SLOT_DEFINITION_INITARGS =
    new Primitive(Symbol._SLOT_DEFINITION_INITARGS, "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_INITARGS];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-initargs
  private static final Primitive SET_SLOT_DEFINITION_INITARGS =
    new Primitive("set-slot-definition-initargs", PACKAGE_SYS, true,
                  "slot-definition initargs")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_INITARGS] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### %slot-definition-readers
  private static final Primitive _SLOT_DEFINITION_READERS =
    new Primitive("%slot-definition-readers", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_READERS];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-readers
  private static final Primitive SET_SLOT_DEFINITION_READERS =
    new Primitive("set-slot-definition-readers", PACKAGE_SYS, true,
                  "slot-definition readers")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_READERS] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### %slot-definition-writers
  private static final Primitive _SLOT_DEFINITION_WRITERS =
    new Primitive("%slot-definition-writers", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_WRITERS];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-writers
  private static final Primitive SET_SLOT_DEFINITION_WRITERS =
    new Primitive("set-slot-definition-writers", PACKAGE_SYS, true,
                  "slot-definition writers")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_WRITERS] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### %slot-definition-allocation
  private static final Primitive _SLOT_DEFINITION_ALLOCATION =
    new Primitive("%slot-definition-allocation", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-allocation
  private static final Primitive SET_SLOT_DEFINITION_ALLOCATION =
    new Primitive("set-slot-definition-allocation", PACKAGE_SYS, true,
                  "slot-definition allocation")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### %slot-definition-allocation-class
  private static final Primitive _SLOT_DEFINITION_ALLOCATION_CLASS =
    new Primitive("%slot-definition-allocation-class", PACKAGE_SYS, true,
                  "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION_CLASS];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-allocation-class
  private static final Primitive SET_SLOT_DEFINITION_ALLOCATION_CLASS =
    new Primitive("set-slot-definition-allocation-class", PACKAGE_SYS, true,
                  "slot-definition allocation-class")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_ALLOCATION_CLASS] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### %slot-definition-location
  private static final Primitive _SLOT_DEFINITION_LOCATION =
    new Primitive("%slot-definition-location", PACKAGE_SYS, true, "slot-definition")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((SlotDefinition)arg).slots[SlotDefinitionClass.SLOT_INDEX_LOCATION];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.SLOT_DEFINITION);
          }
      }
    };

  // ### set-slot-definition-location
  private static final Primitive SET_SLOT_DEFINITION_LOCATION =
    new Primitive("set-slot-definition-location", PACKAGE_SYS, true, "slot-definition location")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((SlotDefinition)first).slots[SlotDefinitionClass.SLOT_INDEX_LOCATION] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.SLOT_DEFINITION);
          }
      }
    };
}
