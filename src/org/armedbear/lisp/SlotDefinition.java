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
  private SlotDefinition()
  {
    super(StandardClass.STANDARD_SLOT_DEFINITION,
          StandardClass.STANDARD_SLOT_DEFINITION.getClassLayout().getLength());
    setInstanceSlotValue(Symbol.LOCATION, NIL);
    setInstanceSlotValue(Symbol._TYPE, T);
    setInstanceSlotValue(Symbol._DOCUMENTATION, NIL);
  }

  private SlotDefinition(StandardClass clazz) {
    // clazz layout needs to have SlotDefinitionClass layout as prefix
    // or indexed slot access won't work
    super(clazz, clazz.getClassLayout().getLength());
    setInstanceSlotValue(Symbol.LOCATION, NIL);
  }

  public SlotDefinition(StandardClass clazz, LispObject name) {
    // clazz layout needs to have SlotDefinitionClass layout as prefix
    // or indexed slot access won't work
    super(clazz, clazz.getClassLayout().getLength());
    Debug.assertTrue(name instanceof Symbol);
    setInstanceSlotValue(Symbol.NAME, name);
    setInstanceSlotValue(Symbol.INITFUNCTION, NIL);
    setInstanceSlotValue(Symbol.INITARGS,
                         new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName())));
    setInstanceSlotValue(Symbol.READERS, NIL);
    setInstanceSlotValue(Symbol.ALLOCATION, Keyword.INSTANCE);
    setInstanceSlotValue(Symbol.LOCATION, NIL);
    setInstanceSlotValue(Symbol._TYPE, T);
    setInstanceSlotValue(Symbol._DOCUMENTATION, NIL);
  }
  
  public SlotDefinition(LispObject name, LispObject readers)
  {
    this();
    Debug.assertTrue(name instanceof Symbol);
    setInstanceSlotValue(Symbol.NAME, name);
    setInstanceSlotValue(Symbol.INITFUNCTION, NIL);
    setInstanceSlotValue(Symbol.INITARGS,
                         new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName())));
    setInstanceSlotValue(Symbol.READERS, readers);
    setInstanceSlotValue(Symbol.ALLOCATION, Keyword.INSTANCE);
  }

  public SlotDefinition(LispObject name, LispObject readers,
                        LispObject initForm)
  {
    this();
    Debug.assertTrue(name instanceof Symbol);
    setInstanceSlotValue(Symbol.NAME, name);
    setInstanceSlotValue(Symbol.INITFUNCTION, NIL);
    setInstanceSlotValue(Symbol.INITFORM, initForm);
    setInstanceSlotValue(Symbol.INITARGS,
                         new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName())));
    setInstanceSlotValue(Symbol.READERS, readers);
    setInstanceSlotValue(Symbol.ALLOCATION, Keyword.INSTANCE);
  }

  public SlotDefinition(LispObject name, LispObject readers,
                        Function initFunction)
  {
    this();
    Debug.assertTrue(name instanceof Symbol);
    setInstanceSlotValue(Symbol.NAME, name);
    setInstanceSlotValue(Symbol.INITFUNCTION, initFunction);
    setInstanceSlotValue(Symbol.INITFORM, NIL);
    setInstanceSlotValue(Symbol.INITARGS,
                         new Cons(PACKAGE_KEYWORD.intern(((Symbol)name).getName())));
    setInstanceSlotValue(Symbol.READERS, readers);
    setInstanceSlotValue(Symbol.ALLOCATION, Keyword.INSTANCE);
  }

  public SlotDefinition(LispObject name, LispObject readers,
                        Function initFunction, LispObject initargs)
  {
    this();
    Debug.assertTrue(name instanceof Symbol);
    setInstanceSlotValue(Symbol.NAME, name);
    setInstanceSlotValue(Symbol.INITFUNCTION, initFunction);
    setInstanceSlotValue(Symbol.INITFORM, NIL);
    setInstanceSlotValue(Symbol.INITARGS, initargs);
    setInstanceSlotValue(Symbol.READERS, readers);
    setInstanceSlotValue(Symbol.ALLOCATION, Keyword.INSTANCE);
  }

  @Override
  public String printObject()
  {
    StringBuilder sb =
      new StringBuilder(Symbol.SLOT_DEFINITION.printObject());
    LispObject name = getInstanceSlotValue(Symbol.NAME);
    if (name != null && name != NIL) {
      sb.append(' ');
      sb.append(name.printObject());
    }
    return unreadableString(sb.toString());
  }

  private static final Primitive MAKE_SLOT_DEFINITION 
    = new pf_make_slot_definition();
  @DocString(name="%make-slot-definition",
             args="slot-class",
             doc="Argument must be a subclass of standard-slot-definition")
  private static final class pf_make_slot_definition extends Primitive
  {
    pf_make_slot_definition()
    {
      super("%make-slot-definition", PACKAGE_SYS, true, "slot-class");
    }
    @Override
    public LispObject execute(LispObject slotDefinitionClass)
    {
      if (!(slotDefinitionClass instanceof StandardClass))
        return type_error(slotDefinitionClass,
                          StandardClass.STANDARD_SLOT_DEFINITION);
      // we could check whether slotClass is a subtype of
      // standard-slot-definition here, but subtypep doesn't work early
      // in the build process
      return new SlotDefinition((StandardClass)slotDefinitionClass);
    }
  };

}
