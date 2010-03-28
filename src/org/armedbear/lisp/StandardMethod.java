/*
 * StandardMethod.java
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

public class StandardMethod extends StandardObject
{
  public StandardMethod()
  {
    super(StandardClass.STANDARD_METHOD,
          StandardClass.STANDARD_METHOD.getClassLayout().getLength());
  }

  protected StandardMethod(LispClass cls, int length)
  {
    super(cls, length);
  }

  public StandardMethod(StandardGenericFunction gf,
                        Function fastFunction,
                        LispObject lambdaList,
                        LispObject specializers)
  {
    this();
    slots[StandardMethodClass.SLOT_INDEX_GENERIC_FUNCTION] = gf;
    slots[StandardMethodClass.SLOT_INDEX_LAMBDA_LIST] = lambdaList;
    slots[StandardMethodClass.SLOT_INDEX_SPECIALIZERS] = specializers;
    slots[StandardMethodClass.SLOT_INDEX_QUALIFIERS] = NIL;
    slots[StandardMethodClass.SLOT_INDEX_FUNCTION] = NIL;
    slots[StandardMethodClass.SLOT_INDEX_FAST_FUNCTION] = fastFunction;
    slots[StandardMethodClass.SLOT_INDEX_DOCUMENTATION] = NIL;
  }

  // ### method-lambda-list
  // generic function
  private static final Primitive METHOD_LAMBDA_LIST =
    new Primitive("method-lambda-list", PACKAGE_SYS, true, "method")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_LAMBDA_LIST];
      }
    };

  // ### set-method-lambda-list
  private static final Primitive SET_METHOD_LAMBDA_LIST =
    new Primitive("set-method-lambda-list", PACKAGE_SYS, true,
                  "method lambda-list")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_LAMBDA_LIST] = second;
          return second;
      }
    };

  // ### method-qualifiers
  private static final Primitive _METHOD_QUALIFIERS =
    new Primitive("%method-qualifiers", PACKAGE_SYS, true, "method")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_QUALIFIERS];
      }
    };

  // ### set-method-qualifiers
  private static final Primitive SET_METHOD_QUALIFIERS =
    new Primitive("set-method-qualifiers", PACKAGE_SYS, true,
                  "method qualifiers")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {          
          checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_QUALIFIERS] = second;
          return second;
      }
    };

  // ### method-documentation
  private static final Primitive METHOD_DOCUMENTATION =
    new Primitive("method-documentation", PACKAGE_SYS, true, "method")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_DOCUMENTATION];
      }
    };

  // ### set-method-documentation
  private static final Primitive SET_METHOD_DOCUMENTATION =
    new Primitive("set-method-documentation", PACKAGE_SYS, true,
                  "method documentation")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_DOCUMENTATION] = second;
          return second;
      }
    };

  public LispObject getFunction()
  {
    return slots[StandardMethodClass.SLOT_INDEX_FUNCTION];
  }

  @Override
  public String writeToString()
  {
    LispObject genericFunction =
      slots[StandardMethodClass.SLOT_INDEX_GENERIC_FUNCTION];
    if (genericFunction instanceof StandardGenericFunction)
      {
        LispObject name =
          ((StandardGenericFunction)genericFunction).getGenericFunctionName();
        if (name != null)
          {
            StringBuilder sb = new StringBuilder();
            LispObject className;
            LispObject lispClass = getLispClass();
            if (lispClass instanceof LispClass)
              className = ((LispClass)lispClass).getName();
            else
              className = Symbol.CLASS_NAME.execute(lispClass);

            sb.append(className.writeToString());
            sb.append(' ');
            sb.append(name.writeToString());
            LispObject specializers =
              slots[StandardMethodClass.SLOT_INDEX_SPECIALIZERS];
            if (specializers != null)
              {
                LispObject specs = specializers;
                LispObject names = NIL;
                while (specs != NIL)
                  {
                    LispObject spec = specs.car();
                    if (spec instanceof LispClass)
                      names = names.push(((LispClass)spec).getName());
                    else
                      names = names.push(spec);
                    specs = specs.cdr();
                  }
                sb.append(' ');
                sb.append(names.nreverse().writeToString());
              }
            return unreadableString(sb.toString());
          }
      }
    return super.writeToString();
  }

  // ### %method-generic-function
  private static final Primitive _METHOD_GENERIC_FUNCTION =
    new Primitive("%method-generic-function", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_GENERIC_FUNCTION];
      }
    };

  // ### %set-method-generic-function
  private static final Primitive _SET_METHOD_GENERICFUNCTION =
    new Primitive("%set-method-generic-function", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_GENERIC_FUNCTION] = second;
          return second;
      }
    };

  // ### %method-function
  private static final Primitive _METHOD_FUNCTION =
    new Primitive("%method-function", PACKAGE_SYS, true, "method")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_FUNCTION];
      }
    };

  // ### %set-method-function
  private static final Primitive _SET_METHOD_FUNCTION =
    new Primitive("%set-method-function", PACKAGE_SYS, true,
                  "method function")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_FUNCTION] = second;
          return second;
      }
    };

  // ### %method-fast-function
  private static final Primitive _METHOD_FAST_FUNCTION =
    new Primitive("%method-fast-function", PACKAGE_SYS, true, "method")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_FAST_FUNCTION];
      }
    };

  // ### %set-method-fast-function
  private static final Primitive _SET_METHOD_FAST_FUNCTION =
    new Primitive("%set-method-fast-function", PACKAGE_SYS, true,
                  "method fast-function")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_FAST_FUNCTION] = second;
          return second;
      }
    };

  // ### %method-specializers
  private static final Primitive _METHOD_SPECIALIZERS =
    new Primitive("%method-specializers", PACKAGE_SYS, true, "method")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_SPECIALIZERS];
      }
    };

  // ### %set-method-specializers
  private static final Primitive _SET_METHOD_SPECIALIZERS =
    new Primitive("%set-method-specializers", PACKAGE_SYS, true,
                  "method specializers")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
          checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_SPECIALIZERS] = second;
          return second;
      }
    };

  private static final StandardGenericFunction METHOD_SPECIALIZERS =
    new StandardGenericFunction("method-specializers",
                                PACKAGE_MOP,
                                true,
                                _METHOD_SPECIALIZERS,
                                list(Symbol.METHOD),
                                list(StandardClass.STANDARD_METHOD));

  private static final StandardGenericFunction METHOD_QUALIFIERS =
    new StandardGenericFunction("method-qualifiers",
                                PACKAGE_MOP,
                                true,
                                _METHOD_QUALIFIERS,
                                list(Symbol.METHOD),
                                list(StandardClass.STANDARD_METHOD));

        final public static StandardMethod checkStandardMethod(LispObject first)
        {
                if (first instanceof StandardMethod)
                        return (StandardMethod) first;
                return (StandardMethod) type_error(first, Symbol.METHOD);
        }

}
