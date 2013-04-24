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
    setInstanceSlotValue(Symbol.LOCATION, NIL);
    setInstanceSlotValue(Symbol._TYPE, T);
    setInstanceSlotValue(Symbol._DOCUMENTATION, NIL);
  }

  public SlotDefinition(StandardClass clazz) {
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

  public static StandardObject checkSlotDefinition(LispObject obj) {
    if (obj instanceof StandardObject) return (StandardObject)obj;
    return (StandardObject)type_error(obj, Symbol.SLOT_DEFINITION);
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
  @DocString(name="make-slot-definition",
             args="&optional class",
             doc="Cannot be called with user-defined subclasses of standard-slot-definition.")
  private static final class pf_make_slot_definition extends Primitive
  {
    pf_make_slot_definition()
    {
      super("make-slot-definition", PACKAGE_SYS, true, "&optional class");
    }
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

  static final Primitive _SLOT_DEFINITION_NAME
    = new pf__slot_definition_name(); 
  @DocString(name="%slot-definition-name")
  private static final class pf__slot_definition_name extends Primitive
  {
    pf__slot_definition_name()
    {
      super(Symbol._SLOT_DEFINITION_NAME, "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.NAME);
    }
  };

  private static final Primitive SET_SLOT_DEFINITION_NAME 
    = new pf_set_slot_definition_name(); 
  @DocString(name="set-slot-definition-name",
             args="slot-definition name")
  private static final class pf_set_slot_definition_name extends Primitive
  {
    pf_set_slot_definition_name()
    {
      super("set-slot-definition-name", PACKAGE_SYS, true,
            "slot-definition name");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.NAME, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_INITFUNCTION 
    = new pf__slot_definition_initfunction(); 
  @DocString(name="%slot-definition-initfunction")
  private static final class pf__slot_definition_initfunction extends Primitive
  {
    pf__slot_definition_initfunction()
    {
      super(Symbol._SLOT_DEFINITION_INITFUNCTION, "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.INITFUNCTION);
    }
  };

  static final Primitive SET_SLOT_DEFINITION_INITFUNCTION 
    = new pf_set_slot_definition_initfunction();
  @DocString(name="set-slot-definition-initfunction",
             args="slot-definition initfunction")
  static final class pf_set_slot_definition_initfunction extends Primitive
  {
    pf_set_slot_definition_initfunction()
    {
      super("set-slot-definition-initfunction", PACKAGE_SYS, true,
            "slot-definition initfunction");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.INITFUNCTION, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_INITFORM
    = new pf__slot_definition_initform();
  @DocString(name="%slot-definition-initform",
             args="slot-definition")
  private static final class pf__slot_definition_initform extends Primitive
  {
    pf__slot_definition_initform()
    {
      super("%slot-definition-initform", PACKAGE_SYS, true, "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.INITFORM);
    }
  };

  static final Primitive SET_SLOT_DEFINITION_INITFORM
    = new pf_set_slot_definition_initform();
  @DocString(name="set-slot-definition-initform",
             args="slot-definition initform")
  static final class pf_set_slot_definition_initform extends Primitive
  {
    pf_set_slot_definition_initform() 
    {
      super("set-slot-definition-initform", PACKAGE_SYS, true,
            "slot-definition initform");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.INITFORM, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_INITARGS
    = new pf__slot_definition_initargs();
  @DocString(name="%slot-definition-initargs")
  private static final class pf__slot_definition_initargs extends Primitive
  {
    pf__slot_definition_initargs()
    {
      super(Symbol._SLOT_DEFINITION_INITARGS, "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.INITARGS);
    }
  };

  static final Primitive SET_SLOT_DEFINITION_INITARGS
    = new pf_set_slot_definition_initargs();
  @DocString(name="set-slot-definition-initargs",
             args="slot-definition initargs")
  private static final class pf_set_slot_definition_initargs extends Primitive
  {
    pf_set_slot_definition_initargs()
    {
      super("set-slot-definition-initargs", PACKAGE_SYS, true,
            "slot-definition initargs");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.INITARGS, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_READERS
    = new pf__slot_definition_readers();
  @DocString(name="%slot-definition-readers",
             args="slot-definition")
  private static final class pf__slot_definition_readers extends Primitive {
    pf__slot_definition_readers()
    {
      super("%slot-definition-readers", PACKAGE_SYS, true,
            "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.READERS);
    }
  };

  private static final Primitive SET_SLOT_DEFINITION_READERS
    = new pf_set_slot_definition_readers();
  @DocString(name="set-slot-definition-readers",
             args="slot-definition readers")
  private static final class pf_set_slot_definition_readers extends Primitive
  {
    pf_set_slot_definition_readers()
    {
      super("set-slot-definition-readers", PACKAGE_SYS, true,
            "slot-definition readers");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.READERS, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_WRITERS
    = new pf__slot_definition_writers();
  @DocString(name="%slot-definition-writers",
             args="slot-definition")
  private static final class pf__slot_definition_writers extends Primitive
  {
    pf__slot_definition_writers()
    {
      super("%slot-definition-writers", PACKAGE_SYS, true,
            "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.WRITERS);
    }
  };

  private static final Primitive SET_SLOT_DEFINITION_WRITERS
    = new pf_set_slot_definition_writers();
  @DocString(name="set-slot-definition-writers",
             args="slot-definition writers")
  private static final class pf_set_slot_definition_writers extends Primitive
  {
    pf_set_slot_definition_writers()
    {
      super("set-slot-definition-writers", PACKAGE_SYS, true,
            "slot-definition writers");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.WRITERS, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_ALLOCATION
    = new pf__slot_definition_allocation();
  @DocString(name="%slot-definition-allocation",
             args="slot-definition")
  private static final class pf__slot_definition_allocation extends Primitive 
  {
    pf__slot_definition_allocation()
    {
      super("%slot-definition-allocation", PACKAGE_SYS, true,
            "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.ALLOCATION);
    }
  };

  private static final Primitive SET_SLOT_DEFINITION_ALLOCATION
    = new pf_set_slot_definition_allocation();
  @DocString(name="set-slot-definition-allocation",
             args="slot-definition allocation")
  private static final class pf_set_slot_definition_allocation extends Primitive
  {
    pf_set_slot_definition_allocation()
    {
      super("set-slot-definition-allocation", PACKAGE_SYS, true,
            "slot-definition allocation");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.ALLOCATION, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_ALLOCATION_CLASS 
    = new pf__slot_definition_allocation_class(); 
  @DocString(name="%slot-definition-allocation-class",
             args="slot-definition")
  private static final class pf__slot_definition_allocation_class extends Primitive
  {
    pf__slot_definition_allocation_class()
    {
      super("%slot-definition-allocation-class", PACKAGE_SYS, true,
            "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.ALLOCATION_CLASS);
    }
  };

  private static final Primitive SET_SLOT_DEFINITION_ALLOCATION_CLASS
    = new pf_set_slot_definition_allocation_class();
  @DocString(name="set-slot-definition-allocation-class",
             args="slot-definition allocation-class")
  private static final class pf_set_slot_definition_allocation_class extends Primitive
  {
    pf_set_slot_definition_allocation_class()
    {
      super("set-slot-definition-allocation-class", PACKAGE_SYS, true,
            "slot-definition allocation-class");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.ALLOCATION_CLASS, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_LOCATION
    = new pf__slot_definition_location();
  @DocString(name="%slot-definition-location")
  private static final class pf__slot_definition_location extends Primitive
  {
    pf__slot_definition_location()
    {
      super("%slot-definition-location", PACKAGE_SYS, true, "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol.LOCATION);
    }
  };

  static final Primitive SET_SLOT_DEFINITION_LOCATION
    = new pf_set_slot_definition_location();
  @DocString(name="set-slot-definition-location",
             args="slot-definition location")
  private static final class pf_set_slot_definition_location extends Primitive
  {
    pf_set_slot_definition_location()
    {
      super("set-slot-definition-location", PACKAGE_SYS, true, 
            "slot-definition location");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol.LOCATION, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_TYPE
    = new pf__slot_definition_type();
  @DocString(name="%slot-definition-type")
  private static final class pf__slot_definition_type extends Primitive
  {
    pf__slot_definition_type()
    {
      super("%slot-definition-type", PACKAGE_SYS, true, "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol._TYPE);
    }
  };

  private static final Primitive SET_SLOT_DEFINITION_TYPE
    = new pf_set_slot_definition_type();
  @DocString(name="set-slot-definition-type",
             args="slot-definition type")
  private static final class pf_set_slot_definition_type extends Primitive
  {
    pf_set_slot_definition_type()
    {
      super("set-slot-definition-type", PACKAGE_SYS, true, 
            "slot-definition type");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol._TYPE, second);
      return second;
    }
  };

  private static final Primitive _SLOT_DEFINITION_DOCUMENTATION
    = new pf__slot_definition_documentation();
  @DocString(name="%slot-definition-documentation")
  private static final class pf__slot_definition_documentation extends Primitive
  {
    pf__slot_definition_documentation()
    {
      super("%slot-definition-documentation", PACKAGE_SYS, true, "slot-definition");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardObject o = checkSlotDefinition(arg);
      return o.getInstanceSlotValue(Symbol._DOCUMENTATION);
    }
  };

  private static final Primitive SET_SLOT_DEFINITION_DOCUMENTATION
    = new pf_set_slot_definition_documentation();
  @DocString(name="set-slot-definition-documentation",
             args="slot-definition documentation")
  private static final class pf_set_slot_definition_documentation extends Primitive
  {
    pf_set_slot_definition_documentation()
    {
      super("set-slot-definition-documentation", PACKAGE_SYS, true, 
            "slot-definition documentation");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      StandardObject o = checkSlotDefinition(first);
      o.setInstanceSlotValue(Symbol._DOCUMENTATION, second);
      return second;
    }
  };

}
