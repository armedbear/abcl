/*
 * StandardObject.java
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

public class StandardObject extends LispObject
{
  protected Layout layout;
  protected LispObject[] slots;

  protected StandardObject()
  {
    layout = new Layout(StandardClass.STANDARD_OBJECT, NIL, NIL);
  }

  protected StandardObject(LispClass cls, int length)
  {
    layout = cls.getClassLayout();
    slots = new LispObject[length];
    for (int i = slots.length; i-- > 0;)
      slots[i] = UNBOUND_VALUE;
  }

  protected StandardObject(LispClass cls)
  {
    layout = cls.getClassLayout();
    slots = new LispObject[layout.getLength()];
    for (int i = slots.length; i-- > 0;)
      slots[i] = UNBOUND_VALUE;
  }

  @Override
  public LispObject getParts() throws ConditionThrowable
  {
    LispObject parts = NIL;
    if (layout != null)
      {
        if (layout.isInvalid())
          {
            // Update instance.
            layout = updateLayout();
          }
      }
    parts = parts.push(new Cons("LAYOUT", layout));
    if (layout != null)
      {
        LispObject[] slotNames = layout.getSlotNames();
        if (slotNames != null)
          {
            for (int i = 0; i < slotNames.length; i++)
              {
                parts = parts.push(new Cons(slotNames[i], slots[i]));
              }
          }
      }
    return parts.nreverse();
  }

  public final LispClass getLispClass()
  {
    return layout.lispClass;
  }

  @Override
  public LispObject typeOf()
  {
    // "For objects of metaclass STRUCTURE-CLASS or STANDARD-CLASS, and for
    // conditions, TYPE-OF returns the proper name of the class returned by
    // CLASS-OF if it has a proper name, and otherwise returns the class
    // itself."
    final LispClass c1 = layout.lispClass;
    // The proper name of a class is "a symbol that names the class whose
    // name is that symbol".
    final Symbol symbol = c1.getSymbol();
    if (symbol != NIL)
      {
        // TYPE-OF.9
        final LispObject c2 = LispClass.findClass(symbol);
        if (c2 == c1)
          return symbol;
      }
    return c1;
  }

  @Override
  public LispObject classOf()
  {
    return layout.lispClass;
  }

  @Override
  public LispObject typep(LispObject type) throws ConditionThrowable
  {
    if (type == Symbol.STANDARD_OBJECT)
      return T;
    if (type == StandardClass.STANDARD_OBJECT)
      return T;
    LispClass cls = layout != null ? layout.lispClass : null;
    if (cls != null)
      {
        if (type == cls)
          return T;
        if (type == cls.getSymbol())
          return T;
        LispObject cpl = cls.getCPL();
        while (cpl != NIL)
          {
            if (type == cpl.car())
              return T;
            if (type == ((LispClass)cpl.car()).getSymbol())
              return T;
            cpl = cpl.cdr();
          }
      }
    return super.typep(type);
  }

  @Override
  public String writeToString() throws ConditionThrowable
  {
    final LispThread thread = LispThread.currentThread();
    int maxLevel = Integer.MAX_VALUE;
    LispObject printLevel = Symbol.PRINT_LEVEL.symbolValue(thread);
    if (printLevel instanceof Fixnum)
      maxLevel = ((Fixnum)printLevel).value;
    LispObject currentPrintLevel =
      _CURRENT_PRINT_LEVEL_.symbolValue(thread);
    int currentLevel = Fixnum.getValue(currentPrintLevel);
    if (currentLevel >= maxLevel)
      return "#";
    if (typep(Symbol.CONDITION) != NIL)
      {
        StringOutputStream stream = new StringOutputStream();
        Symbol.PRINT_OBJECT.execute(this, stream);
        return stream.getString().getStringValue();
      }
    return unreadableString(typeOf().writeToString());
  }

  private Layout updateLayout() throws ConditionThrowable
  {
    Debug.assertTrue(layout.isInvalid());
    Layout oldLayout = layout;
    LispClass cls = oldLayout.lispClass;
    Layout newLayout = cls.getClassLayout();
    Debug.assertTrue(!newLayout.isInvalid());
    StandardObject newInstance = new StandardObject(cls);
    Debug.assertTrue(newInstance.layout == newLayout);
    LispObject added = NIL;
    LispObject discarded = NIL;
    LispObject plist = NIL;
    // Old local slots.
    LispObject[] oldSlotNames = oldLayout.getSlotNames();
    for (int i = 0; i < oldSlotNames.length; i++)
      {
        LispObject slotName = oldSlotNames[i];
        int j = newLayout.getSlotIndex(slotName);
        if (j >= 0)
          newInstance.slots[j] = slots[i];
        else
          {
            discarded = discarded.push(slotName);
            if (slots[i] != UNBOUND_VALUE)
              {
                plist = plist.push(slotName);
                plist = plist.push(slots[i]);
              }
          }
      }
    // Old shared slots.
    LispObject rest = oldLayout.getSharedSlots(); // A list.
    if (rest != null)
      {
        while (rest != NIL)
          {
            LispObject location = rest.car();
            LispObject slotName = location.car();
            int i = newLayout.getSlotIndex(slotName);
            if (i >= 0)
              newInstance.slots[i] = location.cdr();
            rest = rest.cdr();
          }
      }
    // Go through all the new local slots to compute the added slots.
    LispObject[] newSlotNames = newLayout.getSlotNames();
    for (int i = 0; i < newSlotNames.length; i++)
      {
        LispObject slotName = newSlotNames[i];
        int j = oldLayout.getSlotIndex(slotName);
        if (j >= 0)
          continue;
        LispObject location = oldLayout.getSharedSlotLocation(slotName);
        if (location != null)
          continue;
        // Not found.
        added = added.push(slotName);
      }
    // Swap slots.
    LispObject[] tempSlots = slots;
    slots = newInstance.slots;
    newInstance.slots = tempSlots;
    // Swap layouts.
    Layout tempLayout = layout;
    layout = newInstance.layout;
    newInstance.layout = tempLayout;
    Debug.assertTrue(!layout.isInvalid());
    // Call UPDATE-INSTANCE-FOR-REDEFINED-CLASS.
    Symbol.UPDATE_INSTANCE_FOR_REDEFINED_CLASS.execute(this, added,
                                                       discarded, plist);
    return newLayout;
  }

  // Only handles instance slots (not shared slots).
  public LispObject getInstanceSlotValue(LispObject slotName)
    throws ConditionThrowable
  {
    Debug.assertTrue(layout != null);
    if (layout.isInvalid())
      {
        // Update instance.
        layout = updateLayout();
      }
    Debug.assertTrue(layout != null);
    int index = layout.getSlotIndex(slotName);
    Debug.assertTrue(index >= 0);
    return slots[index];
  }

  // Only handles instance slots (not shared slots).
  public void setInstanceSlotValue(LispObject slotName, LispObject newValue)
    throws ConditionThrowable
  {
    Debug.assertTrue(layout != null);
    if (layout.isInvalid())
      {
        // Update instance.
        layout = updateLayout();
      }
    Debug.assertTrue(layout != null);
    int index = layout.getSlotIndex(slotName);
    Debug.assertTrue(index >= 0);
    slots[index] = newValue;
  }

  // ### swap-slots instance-1 instance-2 => nil
  private static final Primitive SWAP_SLOTS =
    new Primitive("swap-slots", PACKAGE_SYS, true, "instance-1 instance-2")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        StandardObject obj1, obj2;
        try
          {
            obj1 = (StandardObject) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_OBJECT);
          }
        try
          {
            obj2 = (StandardObject) second;
          }
        catch (ClassCastException e)
          {
            return type_error(second, Symbol.STANDARD_OBJECT);
          }
        LispObject[] temp = obj1.slots;
        obj1.slots = obj2.slots;
        obj2.slots = temp;
        return NIL;
      }
    };

  // ### std-instance-layout
  private static final Primitive STD_INSTANCE_LAYOUT =
    new Primitive("std-instance-layout", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final StandardObject instance;
        try
          {
              instance = (StandardObject) arg;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_OBJECT);
          }
        Layout layout = instance.layout;
        if (layout.isInvalid())
          {
            // Update instance.
            layout = instance.updateLayout();
          }
        return layout;
      }
    };

  // ### %set-std-instance-layout
  private static final Primitive _SET_STD_INSTANCE_LAYOUT =
    new Primitive("%set-std-instance-layout", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardObject)first).layout = (Layout) second;
            return second;
          }
        catch (ClassCastException e)
          {
            if (!(first instanceof StandardObject))
              return type_error(first, Symbol.STANDARD_OBJECT);
            if (!(second instanceof Layout))
              return type_error(second, Symbol.LAYOUT);
            // Not reached.
            return NIL;
          }
      }
    };

  // ### std-instance-class
  private static final Primitive STD_INSTANCE_CLASS =
    new Primitive("std-instance-class", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardObject)arg).layout.lispClass;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_OBJECT);
          }
      }
    };

  // ### standard-instance-access instance location => value
  private static final Primitive STANDARD_INSTANCE_ACCESS =
    new Primitive("standard-instance-access", PACKAGE_SYS, true,
                  "instance location")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final StandardObject instance;
        try
          {
            instance = (StandardObject) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_OBJECT);
          }
        final int index;
        try
          {
            index = ((Fixnum)second).value;
          }
        catch (ClassCastException e)
          {
            return type_error(second,
                                   list(Symbol.INTEGER, Fixnum.ZERO,
                                         new Fixnum(instance.slots.length)));
          }
        LispObject value;
        try
          {
            value = instance.slots[index];
          }
        catch (ArrayIndexOutOfBoundsException e)
          {
            return type_error(second,
                                   list(Symbol.INTEGER, Fixnum.ZERO,
                                         new Fixnum(instance.slots.length)));
          }
        if (value == UNBOUND_VALUE)
          {
            LispObject slotName = instance.layout.getSlotNames()[index];
            value = Symbol.SLOT_UNBOUND.execute(instance.getLispClass(),
                                                instance, slotName);
            LispThread.currentThread()._values = null;
          }
        return value;
      }
    };

  // ### %set-standard-instance-access instance location new-value => new-value
  private static final Primitive _SET_STANDARD_INSTANCE_ACCESS =
    new Primitive("%set-standard-instance-access", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        try
          {
            ((StandardObject)first).slots[Fixnum.getValue(second)] = third; // FIXME
            return third;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_OBJECT);
          }
      }
    };

  // ### std-slot-boundp
  private static final Primitive STD_SLOT_BOUNDP =
    new Primitive(Symbol.STD_SLOT_BOUNDP, "instance slot-name")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final StandardObject instance;
        try
          {
            instance = (StandardObject) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_OBJECT);
          }
        Layout layout = instance.layout;
        if (layout.isInvalid())
          {
            // Update instance.
            layout = instance.updateLayout();
          }
        final LispObject index = layout.slotTable.get(second);
        if (index != null)
          {
            // Found instance slot.
            return instance.slots[((Fixnum)index).value] != UNBOUND_VALUE ? T : NIL;
          }
        // Check for shared slot.
        final LispObject location = layout.getSharedSlotLocation(second);
        if (location != null)
          return location.cdr() != UNBOUND_VALUE ? T : NIL;
        // Not found.
        final LispThread thread = LispThread.currentThread();
        LispObject value =
          thread.execute(Symbol.SLOT_MISSING, instance.getLispClass(),
                         instance, second, Symbol.SLOT_BOUNDP);
        // "If SLOT-MISSING is invoked and returns a value, a boolean
        // equivalent to its primary value is returned by SLOT-BOUNDP."
        thread._values = null;
        return value != NIL ? T : NIL;
      }
    };

  @Override
  public LispObject SLOT_VALUE(LispObject slotName) throws ConditionThrowable
  {
    if (layout.isInvalid())
      {
        // Update instance.
        layout = updateLayout();
      }
    LispObject value;
    final LispObject index = layout.slotTable.get(slotName);
    if (index != null)
      {
        // Found instance slot.
        value = slots[((Fixnum)index).value];
      }
    else
      {
        // Check for shared slot.
        LispObject location = layout.getSharedSlotLocation(slotName);
        if (location == null)
          return Symbol.SLOT_MISSING.execute(getLispClass(), this, slotName,
                                             Symbol.SLOT_VALUE);
        value = location.cdr();
      }
    if (value == UNBOUND_VALUE)
      {
        value = Symbol.SLOT_UNBOUND.execute(getLispClass(), this, slotName);
        LispThread.currentThread()._values = null;
      }
    return value;
  }

  // ### std-slot-value
  private static final Primitive STD_SLOT_VALUE =
    new Primitive(Symbol.STD_SLOT_VALUE, "instance slot-name")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        return first.SLOT_VALUE(second);
      }
    };

  @Override
  public void setSlotValue(LispObject slotName, LispObject newValue)
    throws ConditionThrowable
  {
    if (layout.isInvalid())
      {
        // Update instance.
        layout = updateLayout();
      }
    final LispObject index = layout.slotTable.get(slotName);
    if (index != null)
      {
        // Found instance slot.
        slots[((Fixnum)index).value] = newValue;
        return;
      }
    // Check for shared slot.
    LispObject location = layout.getSharedSlotLocation(slotName);
    if (location != null)
      {
        location.setCdr(newValue);
        return;
      }
    LispObject[] args = new LispObject[5];
    args[0] = getLispClass();
    args[1] = this;
    args[2] = slotName;
    args[3] = Symbol.SETF;
    args[4] = newValue;
    Symbol.SLOT_MISSING.execute(args);
  }

  // ### set-std-slot-value
  private static final Primitive SET_STD_SLOT_VALUE =
    new Primitive(Symbol.SET_STD_SLOT_VALUE, "instance slot-name new-value")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        first.setSlotValue(second, third);
        return third;
      }
    };
}
