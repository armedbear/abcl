/*
 * Layout.java
 *
 * Copyright (C) 2003-2006 Peter Graves
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

public final class Layout extends LispObject
{
  public final LispClass lispClass;
  public final EqHashTable slotTable;

  private final LispObject[] slotNames;
  private final LispObject sharedSlots;

  private boolean invalid;

  public Layout(LispClass lispClass, LispObject instanceSlots, LispObject sharedSlots)
  {
    this.lispClass = lispClass;
    Debug.assertTrue(instanceSlots.listp());
    int length = 0;
    try
      {
        length = instanceSlots.length();
      }
    catch (Throwable t)
      {
        // Shouldn't happen.
        Debug.trace(t);
      }
    slotNames = new LispObject[length];
    int i = 0;
    try
      {
        while (instanceSlots != NIL)
          {
            slotNames[i++] = instanceSlots.car();
            instanceSlots = instanceSlots.cdr();
          }
      }
    catch (Throwable t)
      {
        // Shouldn't happen.
        Debug.trace(t);
      }
    Debug.assertTrue(i == length);
    this.sharedSlots = sharedSlots;
    slotTable = initializeSlotTable(slotNames);
  }

  public Layout(LispClass lispClass, LispObject[] instanceSlotNames,
                LispObject sharedSlots)
  {
    this.lispClass = lispClass;
    this.slotNames = instanceSlotNames;
    this.sharedSlots = sharedSlots;
    slotTable = initializeSlotTable(slotNames);
  }

  // Copy constructor.
  private Layout(Layout oldLayout)
  {
    lispClass = oldLayout.lispClass;
    slotNames = oldLayout.slotNames;
    sharedSlots = oldLayout.sharedSlots;
    slotTable = initializeSlotTable(slotNames);
  }

  private EqHashTable initializeSlotTable(LispObject[] slotNames)
  {
    EqHashTable ht = new EqHashTable(slotNames.length, NIL, NIL);
    for (int i = slotNames.length; i-- > 0;)
      ht.put(slotNames[i], i < 256 ? Fixnum.constants[i] : new Fixnum(i));
    return ht;
  }

  public LispObject getParts() throws ConditionThrowable
  {
    LispObject result = NIL;
    result = result.push(new Cons("class", lispClass));
    for (int i = 0; i < slotNames.length; i++)
      {
        result = result.push(new Cons("slot " + i, slotNames[i]));
      }
    result = result.push(new Cons("shared slots", sharedSlots));
    return result.nreverse();
  }

  public boolean isInvalid()
  {
    return invalid;
  }

  public void invalidate()
  {
    invalid = true;
  }

  public LispObject[] getSlotNames()
  {
    return slotNames;
  }

  public int getLength()
  {
    return slotNames.length;
  }

  public LispObject getSharedSlots()
  {
    return sharedSlots;
  }

  public String writeToString() throws ConditionThrowable
  {
    return unreadableString(Symbol.LAYOUT);
  }

  // Generates a list of slot definitions for the slot names in this layout.
  protected LispObject generateSlotDefinitions()
  {
    LispObject list = NIL;
    try
      {
        for (int i = slotNames.length; i-- > 0;)
          list = list.push(new SlotDefinition(slotNames[i], NIL));
      }
    catch (Throwable t)
      {
        // Shouldn't happen.
        Debug.trace(t);
      }
    return list;
  }

  // ### make-layout
  private static final Primitive MAKE_LAYOUT =
    new Primitive("make-layout", PACKAGE_SYS, true,
                  "class instance-slots class-slots")
    {
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        try
          {
            return new Layout((LispClass)first, checkList(second),
                              checkList(third));
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.CLASS);
          }
      }

    };

  // ### layout-class
  private static final Primitive LAYOUT_CLASS =
    new Primitive("layout-class", PACKAGE_SYS, true, "layout")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((Layout)arg).lispClass;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.LAYOUT);
          }
      }
    };

  // ### layout-length
  private static final Primitive LAYOUT_LENGTH =
    new Primitive("layout-length", PACKAGE_SYS, true, "layout")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return new Fixnum(((Layout)arg).slotNames.length);
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.LAYOUT);
          }
      }
    };

  public int getSlotIndex(LispObject slotName)
  {
    LispObject index = slotTable.get(slotName);
    if (index != null)
      return ((Fixnum)index).value;
    return -1;
  }

  public LispObject getSharedSlotLocation(LispObject slotName)
    throws ConditionThrowable
  {
    LispObject rest = sharedSlots;
    while (rest != NIL)
      {
        LispObject location = rest.car();
        if (location.car() == slotName)
          return location;
        rest = rest.cdr();
      }
    return null;
  }

  // ### layout-slot-index layout slot-name => index
  private static final Primitive LAYOUT_SLOT_INDEX =
    new Primitive("layout-slot-index", PACKAGE_SYS, true)
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            final LispObject slotNames[] = ((Layout)first).slotNames;
            for (int i = slotNames.length; i-- > 0;)
              {
                if (slotNames[i] == second)
                  return new Fixnum(i);
              }
            return NIL;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.LAYOUT);
          }
      }
    };

  // ### layout-slot-location layout slot-name => location
  private static final Primitive LAYOUT_SLOT_LOCATION =
    new Primitive("layout-slot-location", PACKAGE_SYS, true, "layout slot-name")
    {
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            final LispObject slotNames[] = ((Layout)first).slotNames;
            final int limit = slotNames.length;
            for (int i = 0; i < limit; i++)
              {
                if (slotNames[i] == second)
                  return new Fixnum(i);
              }
            // Reaching here, it's not an instance slot.
            LispObject rest = ((Layout)first).sharedSlots;
            while (rest != NIL)
              {
                LispObject location = rest.car();
                if (location.car() == second)
                  return location;
                rest = rest.cdr();
              }
            return NIL;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.LAYOUT);
          }
      }
    };

  // ### %make-instances-obsolete class => class
  private static final Primitive _MAKE_INSTANCES_OBSOLETE =
    new Primitive("%make-instances-obsolete", PACKAGE_SYS, true, "class")
    {
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final LispClass lispClass;
        try
          {
            lispClass = (LispClass) arg;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.CLASS);
          }
        Layout oldLayout = lispClass.getClassLayout();
        Layout newLayout = new Layout(oldLayout);
        lispClass.setClassLayout(newLayout);
        oldLayout.invalidate();
        return arg;
      }
    };
}
