/*
 * StandardClass.java
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

public class StandardClass extends SlotClass
{
  // Slot names for standard-class.  Must agree with
  // redefine-class-forwarder calls in clos.lisp.
  public static Symbol symName = Symbol.NAME;
  public static Symbol symLayout = Symbol.LAYOUT;
  public static Symbol symDirectSuperclasses = Symbol.DIRECT_SUPERCLASSES;
  public static Symbol symDirectSubclasses = Symbol.DIRECT_SUBCLASSES;
  public static Symbol symPrecedenceList = Symbol.PRECEDENCE_LIST;
  public static Symbol symDirectMethods = Symbol.DIRECT_METHODS;
  public static Symbol symDirectSlots = Symbol.DIRECT_SLOTS; 
  public static Symbol symSlots = Symbol.SLOTS;
  public static Symbol symDirectDefaultInitargs
    = Symbol.DIRECT_DEFAULT_INITARGS;
  public static Symbol symDefaultInitargs = Symbol.DEFAULT_INITARGS;
  public static Symbol symFinalizedP = Symbol.FINALIZED_P;

  // used as init-function for slots in this file.
  static Function constantlyNil = new Function() {
    @Override
    public LispObject execute()
    {
      return NIL;
    }
  };



  static Layout layoutStandardClass =
      new Layout(null,
                 list(symName,
                      symLayout,
                      symDirectSuperclasses,
                      symDirectSubclasses,
                      symPrecedenceList,
                      symDirectMethods,
                      symDirectSlots,
                      symSlots,
                      symDirectDefaultInitargs,
                      symDefaultInitargs,
                      symFinalizedP,
                      Symbol._DOCUMENTATION),
                 NIL)
      {
        @Override
        public LispClass getLispClass()
        {
          return STANDARD_CLASS;
        }
      };

  static Layout layoutFuncallableStandardClass =
      new Layout(null,
                 list(symName,
                      symLayout,
                      symDirectSuperclasses,
                      symDirectSubclasses,
                      symPrecedenceList,
                      symDirectMethods,
                      symDirectSlots,
                      symSlots,
                      symDirectDefaultInitargs,
                      symDefaultInitargs,
                      symFinalizedP,
                      Symbol._DOCUMENTATION),
                 NIL)
      {
        @Override
        public LispClass getLispClass()
        {
          return FUNCALLABLE_STANDARD_CLASS;
        }
      };

  public StandardClass()
  {
      super(layoutStandardClass);
      setDirectSuperclasses(NIL);
      setDirectSubclasses(NIL);
      setClassLayout(layoutStandardClass);
      setCPL(NIL);
      setDirectMethods(NIL);
      setDocumentation(NIL);
      setDirectSlotDefinitions(NIL);
      setSlotDefinitions(NIL);
      setDirectDefaultInitargs(NIL);
      setDefaultInitargs(NIL);
      setFinalized(false);
  }

  public StandardClass(Symbol symbol, LispObject directSuperclasses)
  {
      super(layoutStandardClass,
            symbol, directSuperclasses);
      setDirectSubclasses(NIL);
      setClassLayout(layoutStandardClass);
      setCPL(NIL);
      setDirectMethods(NIL);
      setDocumentation(NIL);
      setDirectSlotDefinitions(NIL);
      setSlotDefinitions(NIL);
      setDirectDefaultInitargs(NIL);
      setDefaultInitargs(NIL);
      setFinalized(false);
  }

  public StandardClass(Layout layout)
  {
    super(layout);
    setDirectSuperclasses(NIL);
    setDirectSubclasses(NIL);
    setClassLayout(layout);
    setCPL(NIL);
    setDirectMethods(NIL);
    setDocumentation(NIL);
    setDirectSlotDefinitions(NIL);
    setSlotDefinitions(NIL);
    setDirectDefaultInitargs(NIL);
    setDefaultInitargs(NIL);
    setFinalized(false);
  }

  public StandardClass(Layout layout, Symbol symbol, LispObject directSuperclasses)
  {
    super(layout, symbol, directSuperclasses);
    setDirectSubclasses(NIL);
    setClassLayout(layout);
    setCPL(NIL);
    setDirectMethods(NIL);
    setDocumentation(NIL);
    setDirectSlotDefinitions(NIL);
    setSlotDefinitions(NIL);
    setDirectDefaultInitargs(NIL);
    setDefaultInitargs(NIL);
    setFinalized(false);
    
  }

  @Override
  public LispObject getName()
  {
    return getInstanceSlotValue(symName);
  }

  @Override
  public void setName(LispObject newName)
  {
    setInstanceSlotValue(symName, newName);
  }

  @Override
  public Layout getClassLayout()
  {
    LispObject layout = getInstanceSlotValue(symLayout);
    if (layout == UNBOUND_VALUE)
      return null;

    if (! (layout instanceof Layout)) {
      // (new Error()).printStackTrace();
      // LispThread.currentThread().printBacktrace();
      // System.err.println("Class: " + this.princToString());
      return (Layout)Lisp.error(Symbol.TYPE_ERROR,
              new SimpleString("The value " + layout.princToString()
                               + " is not of expected type "
                               + Symbol.LAYOUT.princToString()
                               + " in class " + this.princToString() + "."));
    }
    
    return (layout == UNBOUND_VALUE) ? null : (Layout)layout;
  }

  @Override
  public void setClassLayout(LispObject newLayout)
  {
    setInstanceSlotValue(symLayout, newLayout);
  }

  @Override
  public LispObject getDirectSuperclasses()
  {
    return getInstanceSlotValue(symDirectSuperclasses);
  }

  @Override
  public void setDirectSuperclasses(LispObject directSuperclasses)
  {
    setInstanceSlotValue(symDirectSuperclasses, directSuperclasses);
  }

  @Override
  public final boolean isFinalized()
  {
    return getInstanceSlotValue(symFinalizedP) != NIL;
  }

  @Override
  public final void setFinalized(boolean b)
  {
    setInstanceSlotValue(symFinalizedP, b ? T : NIL);
  }

  @Override
  public LispObject getDirectSubclasses()
  {
    return getInstanceSlotValue(symDirectSubclasses);
  }

  @Override
  public void setDirectSubclasses(LispObject directSubclasses)
  {
    setInstanceSlotValue(symDirectSubclasses, directSubclasses);
  }

  @Override
  public LispObject getCPL()
  {
    return getInstanceSlotValue(symPrecedenceList);
  }

  @Override
  public void setCPL(LispObject... cpl)
  {
    LispObject obj1 = cpl[0];
    if (obj1.listp() && cpl.length == 1)
      setInstanceSlotValue(symPrecedenceList, obj1);
    else
      {
        Debug.assertTrue(obj1 == this);
        LispObject l = NIL;
        for (int i = cpl.length; i-- > 0;)
            l = new Cons(cpl[i], l);
        setInstanceSlotValue(symPrecedenceList, l);
      }
  }

  @Override
  public LispObject getDirectMethods()
  {
    return getInstanceSlotValue(symDirectMethods);
  }

  @Override
  public void setDirectMethods(LispObject methods)
  {
    setInstanceSlotValue(symDirectMethods, methods);
  }

  @Override
  public LispObject getDocumentation()
  {
    return getInstanceSlotValue(Symbol._DOCUMENTATION);
  }

  @Override
  public void setDocumentation(LispObject doc)
  {
    setInstanceSlotValue(Symbol._DOCUMENTATION, doc);
  }

  @Override
  public LispObject getDirectSlotDefinitions()
  {
    return getInstanceSlotValue(symDirectSlots);
  }

  @Override
  public void setDirectSlotDefinitions(LispObject directSlotDefinitions)
  {
    setInstanceSlotValue(symDirectSlots, directSlotDefinitions);
  }

  @Override
  public LispObject getSlotDefinitions()
  {
    return getInstanceSlotValue(symSlots);
  }

  @Override
  public void setSlotDefinitions(LispObject slotDefinitions)
  {
     setInstanceSlotValue(symSlots, slotDefinitions);
  }

  @Override
  public LispObject getDirectDefaultInitargs()
  {
    return getInstanceSlotValue(symDirectDefaultInitargs);
  }

  @Override
  public void setDirectDefaultInitargs(LispObject directDefaultInitargs)
  {
    setInstanceSlotValue(symDirectDefaultInitargs, directDefaultInitargs);
  }

  @Override
  public LispObject getDefaultInitargs()
  {
    return getInstanceSlotValue(symDefaultInitargs);
  }

  @Override
  public void setDefaultInitargs(LispObject defaultInitargs)
  {
    setInstanceSlotValue(symDefaultInitargs, defaultInitargs);
  }

  @Override
  public LispObject typeOf()
  {
    return Symbol.STANDARD_CLASS;
  }

  @Override
  public LispObject classOf()
  {
    return STANDARD_CLASS;
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type == Symbol.STANDARD_CLASS)
      return T;
    if (type == STANDARD_CLASS)
      return T;
    return super.typep(type);
  }

  @Override
  public String printObject()
  {
    StringBuilder sb =
      new StringBuilder(Symbol.STANDARD_CLASS.printObject());
    if (getName() != null)
      {
        sb.append(' ');
        sb.append(getName().printObject());
      }
    return unreadableString(sb.toString());
  }

  private static final LispObject standardClassSlotDefinitions()
  {
    return
      list(new SlotDefinition(symName, list(Symbol.CLASS_NAME), constantlyNil),
           new SlotDefinition(symLayout, list(Symbol.CLASS_LAYOUT), constantlyNil),
           new SlotDefinition(symDirectSuperclasses, list(Symbol.CLASS_DIRECT_SUPERCLASSES), constantlyNil),
           new SlotDefinition(symDirectSubclasses, list(Symbol.CLASS_DIRECT_SUBCLASSES), constantlyNil),
           new SlotDefinition(symPrecedenceList, list(Symbol.CLASS_PRECEDENCE_LIST), constantlyNil),
           new SlotDefinition(symDirectMethods, list(Symbol.CLASS_DIRECT_METHODS), constantlyNil),
           new SlotDefinition(symDirectSlots, list(Symbol.CLASS_DIRECT_SLOTS), constantlyNil),
           new SlotDefinition(symSlots, list(Symbol.CLASS_SLOTS), constantlyNil),
           new SlotDefinition(symDirectDefaultInitargs, list(Symbol.CLASS_DIRECT_DEFAULT_INITARGS), constantlyNil),
           new SlotDefinition(symDefaultInitargs, list(Symbol.CLASS_DEFAULT_INITARGS), constantlyNil),
           new SlotDefinition(symFinalizedP, list(Symbol.CLASS_FINALIZED_P), constantlyNil),
           new SlotDefinition(Symbol._DOCUMENTATION,
                              list(Symbol.CLASS_DOCUMENTATION),
                              constantlyNil, list(internKeyword("DOCUMENTATION"))));
  }

  private static final StandardClass addStandardClass(Symbol name,
                                                      LispObject directSuperclasses)
  {
    StandardClass c = new StandardClass(name, directSuperclasses);
    addClass(name, c);
    return c;
  }

  private static final FuncallableStandardClass addFuncallableStandardClass
    (Symbol name, LispObject directSuperclasses)
  {
    FuncallableStandardClass c = new FuncallableStandardClass(name, directSuperclasses);
    addClass(name, c);
    return c;
  }

  // At this point, BuiltInClass.java has not been completely loaded yet, and
  // BuiltInClass.CLASS_T is null. So we need to call setDirectSuperclass()
  // for STANDARD_CLASS and STANDARD_OBJECT in initializeStandardClasses()
  // below.
  public static final StandardClass STANDARD_CLASS =
    addStandardClass(Symbol.STANDARD_CLASS, list(BuiltInClass.CLASS_T));
  public static final StandardClass STANDARD_OBJECT =
    addStandardClass(Symbol.STANDARD_OBJECT, list(BuiltInClass.CLASS_T));
  public static final StandardClass METAOBJECT =
    addStandardClass(Symbol.METAOBJECT, list(STANDARD_OBJECT));
  public static final StandardClass SPECIALIZER =
    addStandardClass(Symbol.SPECIALIZER, list(METAOBJECT));

    public static final StandardClass SLOT_DEFINITION =
        addStandardClass(Symbol.SLOT_DEFINITION, list(METAOBJECT));
    public static final StandardClass STANDARD_SLOT_DEFINITION =
        addClass(Symbol.STANDARD_SLOT_DEFINITION, new SlotDefinitionClass(Symbol.STANDARD_SLOT_DEFINITION, list(SLOT_DEFINITION)));

  static
  {
      SLOT_DEFINITION.finalizeClass();

    STANDARD_CLASS.setClassLayout(layoutStandardClass);
    STANDARD_CLASS.setDirectSlotDefinitions(standardClassSlotDefinitions());
  }

    public static final StandardClass DIRECT_SLOT_DEFINITION =
      addStandardClass(Symbol.DIRECT_SLOT_DEFINITION, list(SLOT_DEFINITION));
    public static final StandardClass EFFECTIVE_SLOT_DEFINITION =
        addStandardClass(Symbol.EFFECTIVE_SLOT_DEFINITION, list(SLOT_DEFINITION));
    //      addStandardClass(Symbol.STANDARD_SLOT_DEFINITION, list(SLOT_DEFINITION));
    public static final StandardClass STANDARD_DIRECT_SLOT_DEFINITION =
        addClass(Symbol.STANDARD_DIRECT_SLOT_DEFINITION,
                 new SlotDefinitionClass(Symbol.STANDARD_DIRECT_SLOT_DEFINITION,
                                         list(STANDARD_SLOT_DEFINITION, DIRECT_SLOT_DEFINITION)));
    public static final StandardClass STANDARD_EFFECTIVE_SLOT_DEFINITION =
        addClass(Symbol.STANDARD_EFFECTIVE_SLOT_DEFINITION,
                 new SlotDefinitionClass(Symbol.STANDARD_EFFECTIVE_SLOT_DEFINITION,
                                         list(STANDARD_SLOT_DEFINITION, EFFECTIVE_SLOT_DEFINITION)));


  // BuiltInClass.FUNCTION is also null here (see previous comment).
  // Following SBCL's lead, we make funcallable-standard-object a
  // funcallable-standard-class.
  public static final StandardClass FUNCALLABLE_STANDARD_OBJECT =
      addFuncallableStandardClass(Symbol.FUNCALLABLE_STANDARD_OBJECT,
                       list(STANDARD_OBJECT, BuiltInClass.FUNCTION));

  public static final StandardClass GENERIC_FUNCTION =
    addFuncallableStandardClass(Symbol.GENERIC_FUNCTION,
                                list(METAOBJECT, FUNCALLABLE_STANDARD_OBJECT));

  public static final StandardClass CLASS =
    addStandardClass(Symbol.CLASS, list(SPECIALIZER));

  public static final StandardClass BUILT_IN_CLASS =
    addStandardClass(Symbol.BUILT_IN_CLASS, list(CLASS));

  public static final StandardClass FORWARD_REFERENCED_CLASS =
    addStandardClass(Symbol.FORWARD_REFERENCED_CLASS, list(CLASS));

  public static final StandardClass FUNCALLABLE_STANDARD_CLASS =
    addStandardClass(Symbol.FUNCALLABLE_STANDARD_CLASS, list(CLASS));

  public static final StandardClass CONDITION =
    addStandardClass(Symbol.CONDITION, list(STANDARD_OBJECT));

  public static final StandardClass SIMPLE_CONDITION =
    addStandardClass(Symbol.SIMPLE_CONDITION, list(CONDITION));

  public static final StandardClass WARNING =
    addStandardClass(Symbol.WARNING, list(CONDITION));

  public static final StandardClass SIMPLE_WARNING =
    addStandardClass(Symbol.SIMPLE_WARNING, list(SIMPLE_CONDITION, WARNING));

  public static final StandardClass STYLE_WARNING =
    addStandardClass(Symbol.STYLE_WARNING, list(WARNING));

  public static final StandardClass SERIOUS_CONDITION =
    addStandardClass(Symbol.SERIOUS_CONDITION, list(CONDITION));

  public static final StandardClass STORAGE_CONDITION =
    addStandardClass(Symbol.STORAGE_CONDITION, list(SERIOUS_CONDITION));

  public static final StandardClass ERROR =
    addStandardClass(Symbol.ERROR, list(SERIOUS_CONDITION));

  public static final StandardClass ARITHMETIC_ERROR =
    addStandardClass(Symbol.ARITHMETIC_ERROR, list(ERROR));

  public static final StandardClass CELL_ERROR =
    addStandardClass(Symbol.CELL_ERROR, list(ERROR));

  public static final StandardClass CONTROL_ERROR =
    addStandardClass(Symbol.CONTROL_ERROR, list(ERROR));

  public static final StandardClass FILE_ERROR =
    addStandardClass(Symbol.FILE_ERROR, list(ERROR));

  public static final StandardClass DIVISION_BY_ZERO =
    addStandardClass(Symbol.DIVISION_BY_ZERO, list(ARITHMETIC_ERROR));

  public static final StandardClass FLOATING_POINT_INEXACT =
    addStandardClass(Symbol.FLOATING_POINT_INEXACT, list(ARITHMETIC_ERROR));

  public static final StandardClass FLOATING_POINT_INVALID_OPERATION =
    addStandardClass(Symbol.FLOATING_POINT_INVALID_OPERATION, list(ARITHMETIC_ERROR));

  public static final StandardClass FLOATING_POINT_OVERFLOW =
    addStandardClass(Symbol.FLOATING_POINT_OVERFLOW, list(ARITHMETIC_ERROR));

  public static final StandardClass FLOATING_POINT_UNDERFLOW =
    addStandardClass(Symbol.FLOATING_POINT_UNDERFLOW, list(ARITHMETIC_ERROR));

  public static final StandardClass PROGRAM_ERROR =
    addStandardClass(Symbol.PROGRAM_ERROR, list(ERROR));

  public static final StandardClass PACKAGE_ERROR =
    addStandardClass(Symbol.PACKAGE_ERROR, list(ERROR));

  public static final StandardClass STREAM_ERROR =
    addStandardClass(Symbol.STREAM_ERROR, list(ERROR));

  public static final StandardClass PARSE_ERROR =
    addStandardClass(Symbol.PARSE_ERROR, list(ERROR));

  public static final StandardClass PRINT_NOT_READABLE =
    addStandardClass(Symbol.PRINT_NOT_READABLE, list(ERROR));

  public static final StandardClass READER_ERROR =
    addStandardClass(Symbol.READER_ERROR, list(PARSE_ERROR, STREAM_ERROR));

  public static final StandardClass END_OF_FILE =
    addStandardClass(Symbol.END_OF_FILE, list(STREAM_ERROR));

  public static final StandardClass SIMPLE_ERROR =
    addStandardClass(Symbol.SIMPLE_ERROR, list(SIMPLE_CONDITION, ERROR));

  public static final StandardClass TYPE_ERROR =
    addStandardClass(Symbol.TYPE_ERROR, list(ERROR));

  public static final StandardClass SIMPLE_TYPE_ERROR =
    addStandardClass(Symbol.SIMPLE_TYPE_ERROR, list(SIMPLE_CONDITION,
                                                     TYPE_ERROR));

  public static final StandardClass UNBOUND_SLOT =
    addStandardClass(Symbol.UNBOUND_SLOT, list(CELL_ERROR));

  public static final StandardClass UNBOUND_VARIABLE =
    addStandardClass(Symbol.UNBOUND_VARIABLE, list(CELL_ERROR));

  public static final StandardClass UNDEFINED_FUNCTION =
    addStandardClass(Symbol.UNDEFINED_FUNCTION, list(CELL_ERROR));

  public static final StandardClass JAVA_EXCEPTION =
    addStandardClass(Symbol.JAVA_EXCEPTION, list(ERROR));

  public static final StandardClass METHOD =
    addStandardClass(Symbol.METHOD, list(METAOBJECT));

  public static final StandardClass STANDARD_METHOD =
    addStandardClass(Symbol.STANDARD_METHOD, list(METHOD));

  public static final StandardClass STANDARD_GENERIC_FUNCTION =
    addFuncallableStandardClass(Symbol.STANDARD_GENERIC_FUNCTION,
                                list(GENERIC_FUNCTION));

  public static void initializeStandardClasses()
  {
    // We need to call setDirectSuperclass() here for classes that have a
    // BuiltInClass as a superclass. See comment above (at first mention of
    // STANDARD_OBJECT).
    STANDARD_CLASS.setDirectSuperclass(CLASS);
    STANDARD_OBJECT.setDirectSuperclass(BuiltInClass.CLASS_T);
    FUNCALLABLE_STANDARD_OBJECT.setDirectSuperclasses(list(STANDARD_OBJECT, BuiltInClass.FUNCTION));
    GENERIC_FUNCTION.setDirectSuperclasses(list(METAOBJECT,
                                                FUNCALLABLE_STANDARD_OBJECT));

    ARITHMETIC_ERROR.setCPL(ARITHMETIC_ERROR, ERROR, SERIOUS_CONDITION,
                            CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    ARITHMETIC_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.OPERATION,
                               list(Symbol.ARITHMETIC_ERROR_OPERATION)),
            new SlotDefinition(Symbol.OPERANDS,
                               list(Symbol.ARITHMETIC_ERROR_OPERANDS))));
    BUILT_IN_CLASS.setCPL(BUILT_IN_CLASS, CLASS, SPECIALIZER, METAOBJECT, STANDARD_OBJECT,
                          BuiltInClass.CLASS_T);
    CELL_ERROR.setCPL(CELL_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                      STANDARD_OBJECT, BuiltInClass.CLASS_T);
    CELL_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.NAME,
                               list(Symbol.CELL_ERROR_NAME))));
    CLASS.setCPL(CLASS, SPECIALIZER, METAOBJECT, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    CONDITION.setCPL(CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    CONDITION.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.FORMAT_CONTROL,
                               list(Symbol.SIMPLE_CONDITION_FORMAT_CONTROL)),
            new SlotDefinition(Symbol.FORMAT_ARGUMENTS,
                               list(Symbol.SIMPLE_CONDITION_FORMAT_ARGUMENTS),
                               NIL)));
    CONDITION.setDirectDefaultInitargs(list(list(Keyword.FORMAT_ARGUMENTS,
                                                 NIL,
                                                 constantlyNil)));
    CONTROL_ERROR.setCPL(CONTROL_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                         STANDARD_OBJECT, BuiltInClass.CLASS_T);
    DIVISION_BY_ZERO.setCPL(DIVISION_BY_ZERO, ARITHMETIC_ERROR, ERROR,
                            SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                            BuiltInClass.CLASS_T);
    END_OF_FILE.setCPL(END_OF_FILE, STREAM_ERROR, ERROR, SERIOUS_CONDITION,
                       CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    ERROR.setCPL(ERROR, SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                 BuiltInClass.CLASS_T);
    FILE_ERROR.setCPL(FILE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                      STANDARD_OBJECT, BuiltInClass.CLASS_T);
    FILE_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.PATHNAME,
                               list(Symbol.FILE_ERROR_PATHNAME))));
    FLOATING_POINT_INEXACT.setCPL(FLOATING_POINT_INEXACT, ARITHMETIC_ERROR,
                                  ERROR, SERIOUS_CONDITION, CONDITION,
                                  STANDARD_OBJECT, BuiltInClass.CLASS_T);
    FLOATING_POINT_INVALID_OPERATION.setCPL(FLOATING_POINT_INVALID_OPERATION,
                                            ARITHMETIC_ERROR, ERROR,
                                            SERIOUS_CONDITION, CONDITION,
                                            STANDARD_OBJECT, BuiltInClass.CLASS_T);
    FLOATING_POINT_OVERFLOW.setCPL(FLOATING_POINT_OVERFLOW, ARITHMETIC_ERROR,
                                   ERROR, SERIOUS_CONDITION, CONDITION,
                                   STANDARD_OBJECT, BuiltInClass.CLASS_T);
    FLOATING_POINT_UNDERFLOW.setCPL(FLOATING_POINT_UNDERFLOW, ARITHMETIC_ERROR,
                                    ERROR, SERIOUS_CONDITION, CONDITION,
                                    STANDARD_OBJECT, BuiltInClass.CLASS_T);
    FORWARD_REFERENCED_CLASS.setCPL(FORWARD_REFERENCED_CLASS, CLASS,
                                    SPECIALIZER, METAOBJECT, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    // Not all of these slots are necessary, but for now we take the
    // standard layout.  Instances of this class will be redefined and
    // get a new layout in due course.
    FORWARD_REFERENCED_CLASS.setClassLayout(layoutStandardClass);
    FORWARD_REFERENCED_CLASS.setDirectSlotDefinitions(standardClassSlotDefinitions());
    FUNCALLABLE_STANDARD_OBJECT.setCPL(FUNCALLABLE_STANDARD_OBJECT,
                                       STANDARD_OBJECT, BuiltInClass.FUNCTION,
                                       BuiltInClass.CLASS_T);
    GENERIC_FUNCTION.setCPL(GENERIC_FUNCTION, METAOBJECT,
                            FUNCALLABLE_STANDARD_OBJECT, STANDARD_OBJECT,
                            BuiltInClass.FUNCTION,
                            BuiltInClass.CLASS_T);
    JAVA_EXCEPTION.setCPL(JAVA_EXCEPTION, ERROR, SERIOUS_CONDITION, CONDITION,
                          STANDARD_OBJECT, BuiltInClass.CLASS_T);
    JAVA_EXCEPTION.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.CAUSE, list(Symbol.JAVA_EXCEPTION_CAUSE))));
    METAOBJECT.setCPL(METAOBJECT, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    SPECIALIZER.setCPL(SPECIALIZER, METAOBJECT, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    METHOD.setCPL(METHOD, METAOBJECT, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STANDARD_METHOD.setCPL(STANDARD_METHOD, METHOD, METAOBJECT, STANDARD_OBJECT,
                           BuiltInClass.CLASS_T);
    STANDARD_METHOD.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol._GENERIC_FUNCTION, NIL, constantlyNil,
                              list(internKeyword("GENERIC-FUNCTION"))),
           new SlotDefinition(Symbol.LAMBDA_LIST, NIL, constantlyNil),
           new SlotDefinition(Symbol.KEYWORDS, NIL, constantlyNil),
           new SlotDefinition(Symbol.OTHER_KEYWORDS_P, NIL, constantlyNil),
           new SlotDefinition(Symbol.SPECIALIZERS, NIL, constantlyNil),
           new SlotDefinition(Symbol.QUALIFIERS, NIL, constantlyNil),
           new SlotDefinition(Symbol._FUNCTION, NIL, constantlyNil,
                              list(internKeyword("FUNCTION"))),
           new SlotDefinition(Symbol.FAST_FUNCTION, NIL, constantlyNil),
           new SlotDefinition(Symbol._DOCUMENTATION, NIL, constantlyNil,
                              list(internKeyword("DOCUMENTATION")))));
    PACKAGE_ERROR.setCPL(PACKAGE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                         STANDARD_OBJECT, BuiltInClass.CLASS_T);
    PACKAGE_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.PACKAGE,
                               list(Symbol.PACKAGE_ERROR_PACKAGE))));
    PARSE_ERROR.setCPL(PARSE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                       STANDARD_OBJECT, BuiltInClass.CLASS_T);
    PRINT_NOT_READABLE.setCPL(PRINT_NOT_READABLE, ERROR, SERIOUS_CONDITION,
                              CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    PRINT_NOT_READABLE.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.OBJECT,
                               list(Symbol.PRINT_NOT_READABLE_OBJECT))));
    PROGRAM_ERROR.setCPL(PROGRAM_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                         STANDARD_OBJECT, BuiltInClass.CLASS_T);
    READER_ERROR.setCPL(READER_ERROR, PARSE_ERROR, STREAM_ERROR, ERROR,
                        SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                        BuiltInClass.CLASS_T);
    SERIOUS_CONDITION.setCPL(SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                             BuiltInClass.CLASS_T);
    SIMPLE_CONDITION.setCPL(SIMPLE_CONDITION, CONDITION, STANDARD_OBJECT,
                            BuiltInClass.CLASS_T);
    SIMPLE_ERROR.setCPL(SIMPLE_ERROR, SIMPLE_CONDITION, ERROR,
                        SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                        BuiltInClass.CLASS_T);
    SIMPLE_TYPE_ERROR.setDirectSuperclasses(list(SIMPLE_CONDITION,
                                                  TYPE_ERROR));
    SIMPLE_TYPE_ERROR.setCPL(SIMPLE_TYPE_ERROR, SIMPLE_CONDITION,
                             TYPE_ERROR, ERROR, SERIOUS_CONDITION,
                             CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    SIMPLE_WARNING.setDirectSuperclasses(list(SIMPLE_CONDITION, WARNING));
    SIMPLE_WARNING.setCPL(SIMPLE_WARNING, SIMPLE_CONDITION, WARNING,
                          CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STANDARD_CLASS.setCPL(STANDARD_CLASS, CLASS, SPECIALIZER, METAOBJECT,
                          STANDARD_OBJECT, BuiltInClass.CLASS_T);
    FUNCALLABLE_STANDARD_CLASS.setCPL(FUNCALLABLE_STANDARD_CLASS, CLASS,
                                      SPECIALIZER, METAOBJECT, STANDARD_OBJECT,
                                      BuiltInClass.CLASS_T);
    // funcallable-standard-class has the same interface as
    // standard-class.
    FUNCALLABLE_STANDARD_CLASS.setClassLayout(layoutStandardClass);
    FUNCALLABLE_STANDARD_CLASS.setDirectSlotDefinitions(standardClassSlotDefinitions());
    STANDARD_OBJECT.setCPL(STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STORAGE_CONDITION.setCPL(STORAGE_CONDITION, SERIOUS_CONDITION, CONDITION,
                             STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STREAM_ERROR.setCPL(STREAM_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                        STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STREAM_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.STREAM,
                               list(Symbol.STREAM_ERROR_STREAM))));
    STYLE_WARNING.setCPL(STYLE_WARNING, WARNING, CONDITION, STANDARD_OBJECT,
                         BuiltInClass.CLASS_T);
    TYPE_ERROR.setCPL(TYPE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                      STANDARD_OBJECT, BuiltInClass.CLASS_T);
    TYPE_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.DATUM,
                               list(Symbol.TYPE_ERROR_DATUM)),
            new SlotDefinition(Symbol.EXPECTED_TYPE,
                               list(Symbol.TYPE_ERROR_EXPECTED_TYPE))));
    UNBOUND_SLOT.setCPL(UNBOUND_SLOT, CELL_ERROR, ERROR, SERIOUS_CONDITION,
                        CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    UNBOUND_SLOT.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.INSTANCE,
                               list(Symbol.UNBOUND_SLOT_INSTANCE))));
    UNBOUND_VARIABLE.setCPL(UNBOUND_VARIABLE, CELL_ERROR, ERROR,
                            SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                            BuiltInClass.CLASS_T);
    UNDEFINED_FUNCTION.setCPL(UNDEFINED_FUNCTION, CELL_ERROR, ERROR,
                              SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                              BuiltInClass.CLASS_T);
    WARNING.setCPL(WARNING, CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);

    // Condition classes.
    STANDARD_CLASS.finalizeClass();
    STANDARD_OBJECT.finalizeClass();
    FUNCALLABLE_STANDARD_OBJECT.finalizeClass();
    FUNCALLABLE_STANDARD_CLASS.finalizeClass();
    FORWARD_REFERENCED_CLASS.finalizeClass();
    GENERIC_FUNCTION.finalizeClass();
    ARITHMETIC_ERROR.finalizeClass();
    CELL_ERROR.finalizeClass();
    CONDITION.finalizeClass();
    CONTROL_ERROR.finalizeClass();
    DIVISION_BY_ZERO.finalizeClass();
    END_OF_FILE.finalizeClass();
    ERROR.finalizeClass();
    FILE_ERROR.finalizeClass();
    FLOATING_POINT_INEXACT.finalizeClass();
    FLOATING_POINT_INVALID_OPERATION.finalizeClass();
    FLOATING_POINT_OVERFLOW.finalizeClass();
    FLOATING_POINT_UNDERFLOW.finalizeClass();
    JAVA_EXCEPTION.finalizeClass();
    METAOBJECT.finalizeClass();
    METHOD.finalizeClass();
    STANDARD_METHOD.finalizeClass();
    SPECIALIZER.finalizeClass();
    CLASS.finalizeClass();
    BUILT_IN_CLASS.finalizeClass();
    PACKAGE_ERROR.finalizeClass();
    PARSE_ERROR.finalizeClass();
    PRINT_NOT_READABLE.finalizeClass();
    PROGRAM_ERROR.finalizeClass();
    READER_ERROR.finalizeClass();
    SERIOUS_CONDITION.finalizeClass();
    SIMPLE_CONDITION.finalizeClass();
    SIMPLE_ERROR.finalizeClass();
    SIMPLE_TYPE_ERROR.finalizeClass();
    SIMPLE_WARNING.finalizeClass();
    STORAGE_CONDITION.finalizeClass();
    STREAM_ERROR.finalizeClass();
    STYLE_WARNING.finalizeClass();
    TYPE_ERROR.finalizeClass();
    UNBOUND_SLOT.finalizeClass();
    UNBOUND_VARIABLE.finalizeClass();
    UNDEFINED_FUNCTION.finalizeClass();
    WARNING.finalizeClass();

    // SYS:SLOT-DEFINITION is constructed and finalized in
    // SlotDefinitionClass.java, but we need to fill in a few things here.
    Debug.assertTrue(SLOT_DEFINITION.isFinalized());
    SLOT_DEFINITION.setCPL(SLOT_DEFINITION, METAOBJECT, STANDARD_OBJECT,
                           BuiltInClass.CLASS_T);
    SLOT_DEFINITION.setDirectSlotDefinitions(SLOT_DEFINITION.getClassLayout().generateSlotDefinitions());
    // There are no inherited slots.
    SLOT_DEFINITION.setSlotDefinitions(SLOT_DEFINITION.getDirectSlotDefinitions());

    DIRECT_SLOT_DEFINITION.setCPL(DIRECT_SLOT_DEFINITION, SLOT_DEFINITION,
                                  METAOBJECT, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    DIRECT_SLOT_DEFINITION.finalizeClass();
    EFFECTIVE_SLOT_DEFINITION.setCPL(EFFECTIVE_SLOT_DEFINITION, SLOT_DEFINITION,
                                     METAOBJECT, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    EFFECTIVE_SLOT_DEFINITION.finalizeClass();
    STANDARD_SLOT_DEFINITION.setCPL(STANDARD_SLOT_DEFINITION, SLOT_DEFINITION,
                                    METAOBJECT, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STANDARD_SLOT_DEFINITION.finalizeClass();
    STANDARD_DIRECT_SLOT_DEFINITION.setCPL(STANDARD_DIRECT_SLOT_DEFINITION, STANDARD_SLOT_DEFINITION,
                                           DIRECT_SLOT_DEFINITION, SLOT_DEFINITION, METAOBJECT, STANDARD_OBJECT,
                                           BuiltInClass.CLASS_T);
    STANDARD_DIRECT_SLOT_DEFINITION.finalizeClass();
    STANDARD_EFFECTIVE_SLOT_DEFINITION.setCPL(STANDARD_EFFECTIVE_SLOT_DEFINITION, STANDARD_SLOT_DEFINITION,
                                              EFFECTIVE_SLOT_DEFINITION, SLOT_DEFINITION, METAOBJECT, STANDARD_OBJECT,
                                              BuiltInClass.CLASS_T);
    STANDARD_EFFECTIVE_SLOT_DEFINITION.finalizeClass();

    // STANDARD-GENERIC-FUNCTION
    STANDARD_GENERIC_FUNCTION.setCPL(STANDARD_GENERIC_FUNCTION,
                                     GENERIC_FUNCTION, METAOBJECT,
                                     FUNCALLABLE_STANDARD_OBJECT,
                                     STANDARD_OBJECT,
                                     BuiltInClass.FUNCTION,
                                     BuiltInClass.CLASS_T);
    STANDARD_GENERIC_FUNCTION.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.NAME, NIL, constantlyNil),
           new SlotDefinition(Symbol.LAMBDA_LIST, NIL, constantlyNil),
           new SlotDefinition(Symbol.REQUIRED_ARGS, NIL, constantlyNil),
           new SlotDefinition(Symbol.OPTIONAL_ARGS, NIL, constantlyNil),
           new SlotDefinition(Symbol.INITIAL_METHODS, NIL, constantlyNil),
           new SlotDefinition(Symbol.METHODS, NIL, constantlyNil),
           new SlotDefinition(Symbol.METHOD_CLASS, NIL, constantlyNil),
           new SlotDefinition(Symbol._METHOD_COMBINATION, NIL, constantlyNil,
                              list(internKeyword("METHOD-COMBINATION"))),
           new SlotDefinition(Symbol.ARGUMENT_PRECEDENCE_ORDER, NIL,
                              constantlyNil),
           new SlotDefinition(Symbol.DECLARATIONS, NIL, constantlyNil),
           new SlotDefinition(Symbol.CLASSES_TO_EMF_TABLE, NIL, constantlyNil),
           new SlotDefinition(Symbol._DOCUMENTATION, NIL, constantlyNil,
                              list(internKeyword("DOCUMENTATION")))));
    // There are no inherited slots.
    STANDARD_GENERIC_FUNCTION.finalizeClass();
  }
}
