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

  public static Symbol symName = PACKAGE_MOP.intern("NAME");
  public static Symbol symLayout = PACKAGE_MOP.intern("LAYOUT");
  public static Symbol symDirectSuperclasses
    = PACKAGE_MOP.intern("DIRECT-SUPERCLASSES");
  public static Symbol symDirectSubclasses
    = PACKAGE_MOP.intern("DIRECT-SUBCLASSES");
  public static Symbol symPrecedenceList
    = PACKAGE_MOP.intern("PRECEDENCE-LIST");
  public static Symbol symDirectMethods
    = PACKAGE_MOP.intern("DIRECT-METHODS");
  public static Symbol symDocumentation
    = PACKAGE_MOP.intern("DOCUMENTATION");
  public static Symbol symDirectSlots
    = PACKAGE_MOP.intern("DIRECT-SLOTS");
  public static Symbol symSlots
    = PACKAGE_MOP.intern("SLOTS");
  public static Symbol symDirectDefaultInitargs
    = PACKAGE_MOP.intern("DIRECT-DEFAULT-INITARGS");
  public static Symbol symDefaultInitargs
    = PACKAGE_MOP.intern("DEFAULT-INITARGS");
  public static Symbol symFinalizedP
    = PACKAGE_MOP.intern("FINALIZED-P");

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
                      symDocumentation),
                 NIL)
      {
        @Override
        public LispClass getLispClass()
        {
          return STANDARD_CLASS;
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
    return getInstanceSlotValue(symDocumentation);
  }

  @Override
  public void setDocumentation(LispObject doc)
  {
    setInstanceSlotValue(symDocumentation, doc);
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

  public LispObject allocateInstance()
  {
    Layout layout = getClassLayout();
    if (layout == null)
      {
        Symbol.ERROR.execute(Symbol.SIMPLE_ERROR,
                             Keyword.FORMAT_CONTROL,
                             new SimpleString("No layout for class ~S."),
                             Keyword.FORMAT_ARGUMENTS,
                             list(this));
      }
    return new StandardObject(this, layout.getLength());
  }

  @Override
  public String writeToString()
  {
    StringBuilder sb =
      new StringBuilder(Symbol.STANDARD_CLASS.writeToString());
    if (getName() != null)
      {
        sb.append(' ');
        sb.append(getName().writeToString());
      }
    return unreadableString(sb.toString());
  }

  private static final LispObject standardClassSlotDefinitions()
  {
      // (CONSTANTLY NIL)
    Function initFunction = new Function() {
      @Override
      public LispObject execute()
      {
         return NIL;
      }
    };

    return
        list(helperMakeSlotDefinition("NAME", initFunction),
             helperMakeSlotDefinition("LAYOUT", initFunction),
             helperMakeSlotDefinition("DIRECT-SUPERCLASSES", initFunction),
             helperMakeSlotDefinition("DIRECT-SUBCLASSES", initFunction),
             helperMakeSlotDefinition("PRECEDENCE-LIST", initFunction),
             helperMakeSlotDefinition("DIRECT-METHODS", initFunction),
             helperMakeSlotDefinition("DIRECT-SLOTS", initFunction),
             helperMakeSlotDefinition("SLOTS", initFunction),
             helperMakeSlotDefinition("DIRECT-DEFAULT-INITARGS", initFunction),
             helperMakeSlotDefinition("DEFAULT-INITARGS", initFunction),
             helperMakeSlotDefinition("FINALIZED-P", initFunction),
             helperMakeSlotDefinition("DOCUMENTATION", initFunction));
  }



  private static final SlotDefinition helperMakeSlotDefinition(String name,
                                                               Function init)
  {
    return
        new SlotDefinition(PACKAGE_MOP.intern(name),   // name
             list(PACKAGE_MOP.intern("CLASS-" + name)), // readers
             init);
  }

  private static final StandardClass addStandardClass(Symbol name,
                                                      LispObject directSuperclasses)
  {
    StandardClass c = new StandardClass(name, directSuperclasses);
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

  public static final StandardClass SLOT_DEFINITION =
    new SlotDefinitionClass();
  static
  {
    addClass(Symbol.SLOT_DEFINITION, SLOT_DEFINITION);

    STANDARD_CLASS.setClassLayout(layoutStandardClass);
    STANDARD_CLASS.setDirectSlotDefinitions(standardClassSlotDefinitions());
  }

    public static final StandardClass DIRECT_SLOT_DEFINITION =
      addStandardClass(Symbol.DIRECT_SLOT_DEFINITION, list(SLOT_DEFINITION));
    public static final StandardClass EFFECTIVE_SLOT_DEFINITION =
      addStandardClass(Symbol.EFFECTIVE_SLOT_DEFINITION, list(SLOT_DEFINITION));

  // BuiltInClass.FUNCTION is also null here (see previous comment).
  public static final StandardClass GENERIC_FUNCTION =
    addStandardClass(Symbol.GENERIC_FUNCTION, list(BuiltInClass.FUNCTION,
                                                    STANDARD_OBJECT));

  public static final StandardClass CLASS =
    addStandardClass(Symbol.CLASS, list(STANDARD_OBJECT));

  public static final StandardClass BUILT_IN_CLASS =
    addStandardClass(Symbol.BUILT_IN_CLASS, list(CLASS));

  public static final StandardClass FORWARD_REFERENCED_CLASS =
    addStandardClass(Symbol.FORWARD_REFERENCED_CLASS, list(CLASS));

  public static final StandardClass STRUCTURE_CLASS =
    addStandardClass(Symbol.STRUCTURE_CLASS, list(CLASS));

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

  public static final StandardClass COMPILER_ERROR =
    addStandardClass(Symbol.COMPILER_ERROR, list(CONDITION));
    
  public static final StandardClass INTERNAL_COMPILER_ERROR =
    addStandardClass(Symbol.INTERNAL_COMPILER_ERROR, list(CONDITION));

  public static final StandardClass COMPILER_UNSUPPORTED_FEATURE_ERROR =
    addStandardClass(Symbol.COMPILER_UNSUPPORTED_FEATURE_ERROR,
                     list(CONDITION));

  public static final StandardClass JAVA_EXCEPTION =
    addStandardClass(Symbol.JAVA_EXCEPTION, list(ERROR));

  public static final StandardClass METHOD =
    addStandardClass(Symbol.METHOD, list(STANDARD_OBJECT));

  public static final StandardClass STANDARD_METHOD =
    new StandardMethodClass();
  static
  {
    addClass(Symbol.STANDARD_METHOD, STANDARD_METHOD);
  }

  public static final StandardClass STANDARD_READER_METHOD =
    new StandardReaderMethodClass();
  static
  {
    addClass(Symbol.STANDARD_READER_METHOD, STANDARD_READER_METHOD);
  }

  public static final StandardClass STANDARD_GENERIC_FUNCTION =
    new StandardGenericFunctionClass();
  static
  {
    addClass(Symbol.STANDARD_GENERIC_FUNCTION, STANDARD_GENERIC_FUNCTION);
  }

  public static void initializeStandardClasses()
  {
    // We need to call setDirectSuperclass() here for classes that have a
    // BuiltInClass as a superclass. See comment above (at first mention of
    // STANDARD_OBJECT).
    STANDARD_CLASS.setDirectSuperclass(CLASS);
    STANDARD_OBJECT.setDirectSuperclass(BuiltInClass.CLASS_T);
    GENERIC_FUNCTION.setDirectSuperclasses(list(BuiltInClass.FUNCTION,
                                                 STANDARD_OBJECT));

    ARITHMETIC_ERROR.setCPL(ARITHMETIC_ERROR, ERROR, SERIOUS_CONDITION,
                            CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    ARITHMETIC_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.OPERATION,
                               list(PACKAGE_CL.intern("ARITHMETIC-ERROR-OPERATION"))),
            new SlotDefinition(Symbol.OPERANDS,
                               list(PACKAGE_CL.intern("ARITHMETIC-ERROR-OPERANDS")))));
    BUILT_IN_CLASS.setCPL(BUILT_IN_CLASS, CLASS, STANDARD_OBJECT,
                          BuiltInClass.CLASS_T);
    CELL_ERROR.setCPL(CELL_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                      STANDARD_OBJECT, BuiltInClass.CLASS_T);
    CELL_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.NAME,
                               list(Symbol.CELL_ERROR_NAME))));
    CLASS.setCPL(CLASS, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    COMPILER_ERROR.setCPL(COMPILER_ERROR, CONDITION, STANDARD_OBJECT,
                          BuiltInClass.CLASS_T);
    INTERNAL_COMPILER_ERROR.setCPL(INTERNAL_COMPILER_ERROR, CONDITION, STANDARD_OBJECT,
                                   BuiltInClass.CLASS_T);
    COMPILER_UNSUPPORTED_FEATURE_ERROR.setCPL(COMPILER_UNSUPPORTED_FEATURE_ERROR,
                                              CONDITION, STANDARD_OBJECT,
                                              BuiltInClass.CLASS_T);
    CONDITION.setCPL(CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    CONDITION.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.FORMAT_CONTROL,
                               list(Symbol.SIMPLE_CONDITION_FORMAT_CONTROL)),
            new SlotDefinition(Symbol.FORMAT_ARGUMENTS,
                               list(Symbol.SIMPLE_CONDITION_FORMAT_ARGUMENTS),
                               NIL)));
    CONDITION.setDirectDefaultInitargs(list(Keyword.FORMAT_ARGUMENTS,
                                             // FIXME
                                             new Closure(list(Symbol.LAMBDA, NIL, NIL),
                                                         new Environment())));
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
                               list(PACKAGE_CL.intern("FILE-ERROR-PATHNAME")))));
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
                                    BuiltInClass.CLASS_T);
    GENERIC_FUNCTION.setCPL(GENERIC_FUNCTION, STANDARD_OBJECT,
                            BuiltInClass.FUNCTION,
                            BuiltInClass.CLASS_T);
    JAVA_EXCEPTION.setCPL(JAVA_EXCEPTION, ERROR, SERIOUS_CONDITION, CONDITION,
                          STANDARD_OBJECT, BuiltInClass.CLASS_T);
    JAVA_EXCEPTION.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.CAUSE, list(Symbol.JAVA_EXCEPTION_CAUSE))));
    METHOD.setCPL(METHOD, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    PACKAGE_ERROR.setCPL(PACKAGE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                         STANDARD_OBJECT, BuiltInClass.CLASS_T);
    PACKAGE_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.PACKAGE,
                               list(PACKAGE_CL.intern("PACKAGE-ERROR-PACKAGE")))));
    PARSE_ERROR.setCPL(PARSE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                       STANDARD_OBJECT, BuiltInClass.CLASS_T);
    PRINT_NOT_READABLE.setCPL(PRINT_NOT_READABLE, ERROR, SERIOUS_CONDITION,
                              CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    PRINT_NOT_READABLE.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.OBJECT,
                               list(PACKAGE_CL.intern("PRINT-NOT-READABLE-OBJECT")))));
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
    STANDARD_CLASS.setCPL(STANDARD_CLASS, CLASS,
                          STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STANDARD_OBJECT.setCPL(STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STORAGE_CONDITION.setCPL(STORAGE_CONDITION, SERIOUS_CONDITION, CONDITION,
                             STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STREAM_ERROR.setCPL(STREAM_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                        STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STREAM_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.STREAM,
                               list(PACKAGE_CL.intern("STREAM-ERROR-STREAM")))));
    STRUCTURE_CLASS.setCPL(STRUCTURE_CLASS, CLASS, STANDARD_OBJECT,
                           BuiltInClass.CLASS_T);
    STYLE_WARNING.setCPL(STYLE_WARNING, WARNING, CONDITION, STANDARD_OBJECT,
                         BuiltInClass.CLASS_T);
    TYPE_ERROR.setCPL(TYPE_ERROR, ERROR, SERIOUS_CONDITION, CONDITION,
                      STANDARD_OBJECT, BuiltInClass.CLASS_T);
    TYPE_ERROR.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.DATUM,
                               list(PACKAGE_CL.intern("TYPE-ERROR-DATUM"))),
            new SlotDefinition(Symbol.EXPECTED_TYPE,
                               list(PACKAGE_CL.intern("TYPE-ERROR-EXPECTED-TYPE")))));
    UNBOUND_SLOT.setCPL(UNBOUND_SLOT, CELL_ERROR, ERROR, SERIOUS_CONDITION,
                        CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    UNBOUND_SLOT.setDirectSlotDefinitions(
      list(new SlotDefinition(Symbol.INSTANCE,
                               list(PACKAGE_CL.intern("UNBOUND-SLOT-INSTANCE")))));
    UNBOUND_VARIABLE.setCPL(UNBOUND_VARIABLE, CELL_ERROR, ERROR,
                            SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                            BuiltInClass.CLASS_T);
    UNDEFINED_FUNCTION.setCPL(UNDEFINED_FUNCTION, CELL_ERROR, ERROR,
                              SERIOUS_CONDITION, CONDITION, STANDARD_OBJECT,
                              BuiltInClass.CLASS_T);
    WARNING.setCPL(WARNING, CONDITION, STANDARD_OBJECT, BuiltInClass.CLASS_T);

    // Condition classes.
    STANDARD_CLASS.finalizeClass();
    ARITHMETIC_ERROR.finalizeClass();
    CELL_ERROR.finalizeClass();
    COMPILER_ERROR.finalizeClass();
    INTERNAL_COMPILER_ERROR.finalizeClass();
    COMPILER_UNSUPPORTED_FEATURE_ERROR.finalizeClass();
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
    SLOT_DEFINITION.setCPL(SLOT_DEFINITION, STANDARD_OBJECT,
                           BuiltInClass.CLASS_T);
    SLOT_DEFINITION.setDirectSlotDefinitions(SLOT_DEFINITION.getClassLayout().generateSlotDefinitions());
    // There are no inherited slots.
    SLOT_DEFINITION.setSlotDefinitions(SLOT_DEFINITION.getDirectSlotDefinitions());

    DIRECT_SLOT_DEFINITION.setCPL(DIRECT_SLOT_DEFINITION, SLOT_DEFINITION,
				  STANDARD_OBJECT, BuiltInClass.CLASS_T);
    DIRECT_SLOT_DEFINITION.finalizeClass();
    EFFECTIVE_SLOT_DEFINITION.setCPL(EFFECTIVE_SLOT_DEFINITION, SLOT_DEFINITION,
				     STANDARD_OBJECT, BuiltInClass.CLASS_T);
    EFFECTIVE_SLOT_DEFINITION.finalizeClass();

    // STANDARD-METHOD
    Debug.assertTrue(STANDARD_METHOD.isFinalized());
    STANDARD_METHOD.setCPL(STANDARD_METHOD, METHOD, STANDARD_OBJECT,
                           BuiltInClass.CLASS_T);
    STANDARD_METHOD.setDirectSlotDefinitions(STANDARD_METHOD.getClassLayout().generateSlotDefinitions());
    // There are no inherited slots.
    STANDARD_METHOD.setSlotDefinitions(STANDARD_METHOD.getDirectSlotDefinitions());

    // STANDARD-READER-METHOD
    Debug.assertTrue(STANDARD_READER_METHOD.isFinalized());
    STANDARD_READER_METHOD.setCPL(STANDARD_READER_METHOD, STANDARD_METHOD,
                                  METHOD, STANDARD_OBJECT, BuiltInClass.CLASS_T);
    STANDARD_READER_METHOD.setSlotDefinitions(STANDARD_READER_METHOD.getClassLayout().generateSlotDefinitions());
    // All but the last slot are inherited.
    STANDARD_READER_METHOD.setDirectSlotDefinitions(list(STANDARD_READER_METHOD.getSlotDefinitions().reverse().car()));

    // STANDARD-GENERIC-FUNCTION
    Debug.assertTrue(STANDARD_GENERIC_FUNCTION.isFinalized());
    STANDARD_GENERIC_FUNCTION.setCPL(STANDARD_GENERIC_FUNCTION,
                                     GENERIC_FUNCTION, STANDARD_OBJECT,
                                     BuiltInClass.FUNCTION,
                                     BuiltInClass.CLASS_T);
    STANDARD_GENERIC_FUNCTION.setDirectSlotDefinitions(STANDARD_GENERIC_FUNCTION.getClassLayout().generateSlotDefinitions());
    // There are no inherited slots.
    STANDARD_GENERIC_FUNCTION.setSlotDefinitions(STANDARD_GENERIC_FUNCTION.getDirectSlotDefinitions());
  }
}
