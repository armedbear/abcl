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
    slots[StandardMethodClass.SLOT_INDEX_KEYWORDS] = NIL;
    slots[StandardMethodClass.SLOT_INDEX_OTHER_KEYWORDS_P] = NIL;
    slots[StandardMethodClass.SLOT_INDEX_SPECIALIZERS] = specializers;
    slots[StandardMethodClass.SLOT_INDEX_QUALIFIERS] = NIL;
    slots[StandardMethodClass.SLOT_INDEX_FUNCTION] = NIL;
    slots[StandardMethodClass.SLOT_INDEX_FAST_FUNCTION] = fastFunction;
    slots[StandardMethodClass.SLOT_INDEX_DOCUMENTATION] = NIL;
  }

  private static final Primitive METHOD_LAMBDA_LIST
    = new pf_method_lambda_list();
  @DocString(name="method-lambda-list",
             args="generic-method")
  private static final class pf_method_lambda_list extends Primitive
  {
    pf_method_lambda_list()
    {
      super("method-lambda-list", PACKAGE_SYS, true, "generic-method");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_LAMBDA_LIST];
    }
  };

  private static final Primitive SET_METHOD_LAMBDA_LIST
    = new pf_set_method_lambda_list();
  @DocString(name="set-method-lambda-list",
             args="method lambda-list")
  private static final class pf_set_method_lambda_list extends Primitive
  {
    pf_set_method_lambda_list()
    {
      super("set-method-lambda-list", PACKAGE_SYS, true,
            "method lambda-list");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_LAMBDA_LIST] = second;
      return second;
    }
  };

  private static final Primitive _FUNCTION_KEYWORDS
    = new pf__function_keywords();
  @DocString(name="%function-keywords",
             args="standard-method")
  private static final class pf__function_keywords extends Primitive
  {
    pf__function_keywords()
    {
      super("%function-keywords", PACKAGE_SYS, true, "method");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      StandardMethod method = checkStandardMethod(arg);
      LispThread thread = LispThread.currentThread();

      return thread
          .setValues(method.slots[StandardMethodClass.SLOT_INDEX_KEYWORDS],
                     method.slots[StandardMethodClass.SLOT_INDEX_OTHER_KEYWORDS_P]);
    }
  };

  private static final Primitive _SET_FUNCTION_KEYWORDS
    = new pf__set_function_keywords();
  @DocString(name="%set-function-keywords",
             args="standard-method keywords other-keywords-p")
  private static final class pf__set_function_keywords extends Primitive
  {
    pf__set_function_keywords()
    {
      super("%set-function-keywords", PACKAGE_SYS, true,
            "method keywords");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third)
    {
      StandardMethod method = checkStandardMethod(first);
      method.slots[StandardMethodClass.SLOT_INDEX_KEYWORDS] = second;
      method.slots[StandardMethodClass.SLOT_INDEX_OTHER_KEYWORDS_P] = third;
      return second;
    }
  };


  private static final Primitive _METHOD_QUALIFIERS 
    = new gf__method_qualifiers();
  @DocString(name="%method-qualifiers",
             args="method")
  private static final class gf__method_qualifiers extends Primitive
  {
    gf__method_qualifiers()
    {
      super("%method-qualifiers", PACKAGE_SYS, true, "method");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_QUALIFIERS];
    }
  };

  private static final Primitive SET_METHOD_QUALIFIERS 
    = new pf_set_method_qualifiers();
  @DocString(name="set-method-qualifiers",
             args="method qualifiers")
  private static final class pf_set_method_qualifiers extends Primitive
  {
    pf_set_method_qualifiers()
    {
      super("set-method-qualifiers", PACKAGE_SYS, true,
            "method qualifiers");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {          
      checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_QUALIFIERS] = second;
      return second;
    }
  };

  private static final Primitive METHOD_DOCUMENTATION 
    = new pf_method_documentation(); 
  @DocString(name="method-documentation",
             args="method")
  private static final class pf_method_documentation extends Primitive
  {
    pf_method_documentation()
    {
      super("method-documentation", PACKAGE_SYS, true, "method");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_DOCUMENTATION];
    }
  };

  private static final Primitive SET_METHOD_DOCUMENTATION 
    = new pf_set_method_documentation();
  @DocString(name="set-method-documentation",
             args="method documentation")
  private static final class pf_set_method_documentation extends Primitive
  {
    pf_set_method_documentation()
    {
      super("set-method-documentation", PACKAGE_SYS, true,
            "method documentation");
    }
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
  public String printObject()
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

            sb.append(className.printObject());
            sb.append(' ');
            sb.append(name.printObject());
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
                sb.append(names.nreverse().printObject());
              }
            return unreadableString(sb.toString());
          }
      }
    return super.printObject();
  }

  private static final Primitive _METHOD_GENERIC_FUNCTION 
    = new pf__method_generic_function();
  @DocString(name="%method-generic-function")
  private static final class pf__method_generic_function extends Primitive
  {
    pf__method_generic_function()
    {
      super("%method-generic-function", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_GENERIC_FUNCTION];
    }
  };

  private static final Primitive _SET_METHOD_GENERICFUNCTION 
    = new pf__set_method_genericfunction();
  @DocString(name="%set-method-generic-function")
  private static final class pf__set_method_genericfunction extends Primitive
  {
    pf__set_method_genericfunction()
    {
      super("%set-method-generic-function", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_GENERIC_FUNCTION] = second;
      return second;
    }
  };

  private static final Primitive _METHOD_FUNCTION 
    = new pf__method_function(); 
  @DocString(name="%method-function")
  private static final class pf__method_function extends Primitive
  {
    pf__method_function()
    {
      super("%method-function", PACKAGE_SYS, true, "method");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
          return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_FUNCTION];
    }
  };

  private static final Primitive _SET_METHOD_FUNCTION
    = new pf__set_method_function();
  @DocString(name="%set-method-function",
             args="method function")
  private static final class pf__set_method_function extends Primitive
  {
    pf__set_method_function()
    {
      super("%set-method-function", PACKAGE_SYS, true,
            "method function");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_FUNCTION] = second;
      return second;
    }
  };

  private static final Primitive _METHOD_FAST_FUNCTION
    = new pf__method_fast_function();
  @DocString(name="%method-fast-function",
             args="method")
  private static final class pf__method_fast_function extends Primitive
  {
    pf__method_fast_function()
    {
      super("%method-fast-function", PACKAGE_SYS, true, "method");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_FAST_FUNCTION];
    }
  };

  private static final Primitive _SET_METHOD_FAST_FUNCTION
    = new pf__set_method_fast_function();
  @DocString(name="%set-method-fast-function",
             args="method fast-function")
  private static final class pf__set_method_fast_function extends Primitive
  {
    pf__set_method_fast_function()
    {
      super("%set-method-fast-function", PACKAGE_SYS, true,
            "method fast-function");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardMethod(first).slots[StandardMethodClass.SLOT_INDEX_FAST_FUNCTION] = second;
      return second;
    }
  };

  private static final Primitive _METHOD_SPECIALIZERS
    = new pf__method_specializers();
  @DocString(name="%method-specializers")
  private static final class pf__method_specializers extends Primitive
  {
    pf__method_specializers()
    {
      super("%method-specializers", PACKAGE_SYS, true, "method");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardMethod(arg).slots[StandardMethodClass.SLOT_INDEX_SPECIALIZERS];
    }
  };

  private static final Primitive _SET_METHOD_SPECIALIZERS
    = new pf__set_method_specializers();
  @DocString(name="%set-method-specializers",
             args="method specializers")
  private static final class pf__set_method_specializers extends Primitive
  {
    pf__set_method_specializers()
    {
      super("%set-method-specializers", PACKAGE_SYS, true,
            "method specializers");
    }
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
