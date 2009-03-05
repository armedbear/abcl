/*
 * StandardGenericFunction.java
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

import java.util.HashMap;

public final class StandardGenericFunction extends StandardObject
{
  private LispObject function;

  private int numberOfRequiredArgs;

  private HashMap<CacheEntry,LispObject> cache;
  private HashMap<LispObject,LispObject> slotCache;

  public StandardGenericFunction()
  {
    super(StandardClass.STANDARD_GENERIC_FUNCTION,
          StandardClass.STANDARD_GENERIC_FUNCTION.getClassLayout().getLength());
  }

  public StandardGenericFunction(String name, Package pkg, boolean exported,
                                 Function function, LispObject lambdaList,
                                 LispObject specializers)
  {
    this();
    try
      {
        Symbol symbol;
        if (exported)
          symbol = pkg.internAndExport(name.toUpperCase());
        else
          symbol = pkg.intern(name.toUpperCase());
        symbol.setSymbolFunction(this);
        this.function = function;
        slots[StandardGenericFunctionClass.SLOT_INDEX_NAME] = symbol;
        slots[StandardGenericFunctionClass.SLOT_INDEX_LAMBDA_LIST] =
          lambdaList;
        slots[StandardGenericFunctionClass.SLOT_INDEX_REQUIRED_ARGS] =
          lambdaList;
        numberOfRequiredArgs = lambdaList.length();
        slots[StandardGenericFunctionClass.SLOT_INDEX_INITIAL_METHODS] =
          NIL;
        StandardMethod method =
          new StandardMethod(this, function, lambdaList, specializers);
        slots[StandardGenericFunctionClass.SLOT_INDEX_METHODS] =
          list1(method);
        slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_CLASS] =
          StandardClass.STANDARD_METHOD;
        slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_COMBINATION] =
          Symbol.STANDARD;
        slots[StandardGenericFunctionClass.SLOT_INDEX_ARGUMENT_PRECEDENCE_ORDER] =
          NIL;
        slots[StandardGenericFunctionClass.SLOT_INDEX_CLASSES_TO_EMF_TABLE] =
          NIL;
        slots[StandardGenericFunctionClass.SLOT_INDEX_DOCUMENTATION] = NIL;
      }
    catch (ConditionThrowable t)
      {
        Debug.assertTrue(false);
      }
  }

  private void finalizeInternal()
  {
    cache = null;
  }

  @Override
  public LispObject typep(LispObject type) throws ConditionThrowable
  {
    if (type == Symbol.COMPILED_FUNCTION)
      {
        if (function != null)
          return function.typep(type);
        else
          return NIL;
      }
    if (type == Symbol.STANDARD_GENERIC_FUNCTION)
      return T;
    if (type == StandardClass.STANDARD_GENERIC_FUNCTION)
      return T;
    return super.typep(type);
  }

  public LispObject getGenericFunctionName()
  {
    return slots[StandardGenericFunctionClass.SLOT_INDEX_NAME];
  }

  public void setGenericFunctionName(LispObject name)
  {
    slots[StandardGenericFunctionClass.SLOT_INDEX_NAME] = name;
  }

  @Override
  public LispObject execute() throws ConditionThrowable
  {
    return function.execute();
  }

  @Override
  public LispObject execute(LispObject arg) throws ConditionThrowable
  {
    return function.execute(arg);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second)
    throws ConditionThrowable
  {
    return function.execute(first, second);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third)
    throws ConditionThrowable
  {
    return function.execute(first, second, third);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)
    throws ConditionThrowable
  {
    return function.execute(first, second, third, fourth);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth)
    throws ConditionThrowable
  {
    return function.execute(first, second, third, fourth,
                            fifth);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth)
    throws ConditionThrowable
  {
    return function.execute(first, second, third, fourth,
                            fifth, sixth);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh)
    throws ConditionThrowable
  {
    return function.execute(first, second, third, fourth,
                            fifth, sixth, seventh);
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh, LispObject eighth)
    throws ConditionThrowable
  {
    return function.execute(first, second, third, fourth,
                            fifth, sixth, seventh, eighth);
  }

  @Override
  public LispObject execute(LispObject[] args) throws ConditionThrowable
  {
    return function.execute(args);
  }

  @Override
  public String writeToString() throws ConditionThrowable
  {
    LispObject name = getGenericFunctionName();
    if (name != null)
      {
        FastStringBuffer sb = new FastStringBuffer();
        sb.append(getLispClass().getSymbol().writeToString());
        sb.append(' ');
        sb.append(name.writeToString());
        return unreadableString(sb.toString());
      }
    return super.writeToString();
  }

  // Profiling.
  private int callCount;

  @Override
  public final int getCallCount()
  {
    return callCount;
  }

  @Override
  public void setCallCount(int n)
  {
    callCount = n;
  }

  @Override
  public final void incrementCallCount()
  {
    ++callCount;
  }

  // AMOP (p. 216) specifies the following readers as generic functions:
  //   generic-function-argument-precedence-order
  //   generic-function-declarations
  //   generic-function-lambda-list
  //   generic-function-method-class
  //   generic-function-method-combination
  //   generic-function-methods
  //   generic-function-name

  // ### %generic-function-name
  private static final Primitive _GENERIC_FUNCTION_NAME =
    new Primitive("%generic-function-name", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_NAME];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### %set-generic-function-name
  private static final Primitive _SET_GENERIC_FUNCTION_NAME =
    new Primitive("%set-generic-function-name", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_NAME] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### %generic-function-lambda-list
  private static final Primitive _GENERIC_FUNCTION_LAMBDA_LIST =
    new Primitive("%generic-function-lambda-list", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_LAMBDA_LIST];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### %set-generic-function-lambdaList
  private static final Primitive _SET_GENERIC_FUNCTION_LAMBDA_LIST =
    new Primitive("%set-generic-function-lambda-list", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_LAMBDA_LIST] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### funcallable-instance-function funcallable-instance => function
  private static final Primitive FUNCALLABLE_INSTANCE_FUNCTION =
    new Primitive("funcallable-instance-function", PACKAGE_MOP, false,
                  "funcallable-instance")
    {
      @Override
      public LispObject execute(LispObject arg)
        throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).function;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### set-funcallable-instance-function funcallable-instance function => unspecified
  // AMOP p. 230
  private static final Primitive SET_FUNCALLABLE_INSTANCE_FUNCTION =
    new Primitive("set-funcallable-instance-function", PACKAGE_MOP, true,
                  "funcallable-instance function")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).function = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### gf-required-args
  private static final Primitive GF_REQUIRED_ARGS =
    new Primitive("gf-required-args", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_REQUIRED_ARGS];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### %set-gf-required-args
  private static final Primitive _SET_GF_REQUIRED_ARGS =
    new Primitive("%set-gf-required-args", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final StandardGenericFunction gf;
        try
          {
            gf = (StandardGenericFunction) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
        gf.slots[StandardGenericFunctionClass.SLOT_INDEX_REQUIRED_ARGS] = second;
        gf.numberOfRequiredArgs = second.length();
        return second;
      }
    };

  // ### generic-function-initial-methods
  private static final Primitive GENERIC_FUNCTION_INITIAL_METHODS =
    new Primitive("generic-function-initial-methods", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_INITIAL_METHODS];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### set-generic-function-initial-methods
  private static final Primitive SET_GENERIC_FUNCTION_INITIAL_METHODS =
    new Primitive("set-generic-function-initial-methods", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_INITIAL_METHODS] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### generic-function-methods
  private static final Primitive GENERIC_FUNCTION_METHODS =
    new Primitive("generic-function-methods", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_METHODS];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### set-generic-function-methods
  private static final Primitive SET_GENERIC_FUNCTION_METHODS =
    new Primitive("set-generic-function-methods", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_METHODS] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### generic-function-method-class
  private static final Primitive GENERIC_FUNCTION_METHOD_CLASS =
    new Primitive("generic-function-method-class", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_CLASS];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### set-generic-function-method-class
  private static final Primitive SET_GENERIC_FUNCTION_METHOD_CLASS =
    new Primitive("set-generic-function-method-class", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_CLASS] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### generic-function-method-combination
  private static final Primitive GENERIC_FUNCTION_METHOD_COMBINATION =
    new Primitive("generic-function-method-combination", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_COMBINATION];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### set-generic-function-method-combination
  private static final Primitive SET_GENERIC_FUNCTION_METHOD_COMBINATION =
    new Primitive("set-generic-function-method-combination", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_COMBINATION] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### generic-function-argument-precedence-order
  private static final Primitive GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER =
    new Primitive("generic-function-argument-precedence-order", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_ARGUMENT_PRECEDENCE_ORDER];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### set-generic-function-argument-precedence-order
  private static final Primitive SET_GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER =
    new Primitive("set-generic-function-argument-precedence-order", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_ARGUMENT_PRECEDENCE_ORDER] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### generic-function-classes-to-emf-table
  private static final Primitive GENERIC_FUNCTION_CLASSES_TO_EMF_TABLE =
    new Primitive("generic-function-classes-to-emf-table", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_CLASSES_TO_EMF_TABLE];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### set-generic-function-classes-to-emf-table
  private static final Primitive SET_GENERIC_FUNCTION_CLASSES_TO_EMF_TABLE =
    new Primitive("set-generic-function-classes-to-emf-table", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_CLASSES_TO_EMF_TABLE] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### generic-function-documentation
  private static final Primitive GENERIC_FUNCTION_DOCUMENTATION =
    new Primitive("generic-function-documentation", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((StandardGenericFunction)arg).slots[StandardGenericFunctionClass.SLOT_INDEX_DOCUMENTATION];
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### set-generic-function-documentation
  private static final Primitive SET_GENERIC_FUNCTION_DOCUMENTATION =
    new Primitive("set-generic-function-documentation", PACKAGE_SYS, true)
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        try
          {
            ((StandardGenericFunction)first).slots[StandardGenericFunctionClass.SLOT_INDEX_DOCUMENTATION] = second;
            return second;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
      }
    };

  // ### %finalize-generic-function
  private static final Primitive _FINALIZE_GENERIC_FUNCTION =
    new Primitive("%finalize-generic-function", PACKAGE_SYS, true,
                  "generic-function")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        final StandardGenericFunction gf;
        try
          {
            gf = (StandardGenericFunction) arg;
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.STANDARD_GENERIC_FUNCTION);
          }
        gf.finalizeInternal();
        return T;
      }
    };

  // ### cache-emf
  private static final Primitive CACHE_EMF =
    new Primitive("cache-emf", PACKAGE_SYS, true, "generic-function args emf")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        final StandardGenericFunction gf;
        try
          {
            gf = (StandardGenericFunction) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
        LispObject args = second;
        LispObject[] array = new LispObject[gf.numberOfRequiredArgs];
        for (int i = gf.numberOfRequiredArgs; i-- > 0;)
          {
            array[i] = args.car().classOf();
            args = args.cdr();
          }
        CacheEntry classes = new CacheEntry(array);
        HashMap<CacheEntry,LispObject> ht = gf.cache;
        if (ht == null)
            ht = gf.cache = new HashMap<CacheEntry,LispObject>();
        ht.put(classes, third);
        return third;
      }
    };

  // ### get-cached-emf
  private static final Primitive GET_CACHED_EMF =
    new Primitive("get-cached-emf", PACKAGE_SYS, true, "generic-function args")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final StandardGenericFunction gf;
        try
          {
            gf = (StandardGenericFunction) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
        LispObject args = second;
        LispObject[] array = new LispObject[gf.numberOfRequiredArgs];
        for (int i = gf.numberOfRequiredArgs; i-- > 0;)
          {
            array[i] = args.car().classOf();
            args = args.cdr();
          }
        CacheEntry classes = new CacheEntry(array);
        HashMap<CacheEntry,LispObject> ht = gf.cache;
        if (ht == null)
          return NIL;
        LispObject emf = (LispObject) ht.get(classes);
        return emf != null ? emf : NIL;
      }
    };

  // ### cache-slot-location
  private static final Primitive CACHE_SLOT_LOCATION =
    new Primitive("cache-slot-location", PACKAGE_SYS, true, "generic-function layout location")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
        throws ConditionThrowable
      {
        final StandardGenericFunction gf;
        try
          {
            gf = (StandardGenericFunction) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
        LispObject layout = second;
        LispObject location = third;
        HashMap<LispObject,LispObject> ht = gf.slotCache;
        if (ht == null)
          ht = gf.slotCache = new HashMap<LispObject,LispObject>();
        ht.put(layout, location);
        return third;
      }
    };

  // ### get-cached-slot-location
  private static final Primitive GET_CACHED_SLOT_LOCATION =
    new Primitive("get-cached-slot-location", PACKAGE_SYS, true, "generic-function layout")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final StandardGenericFunction gf;
        try
          {
            gf = (StandardGenericFunction) first;
          }
        catch (ClassCastException e)
          {
            return type_error(first, Symbol.STANDARD_GENERIC_FUNCTION);
          }
        LispObject layout = second;
        HashMap<LispObject,LispObject> ht = gf.slotCache;
        if (ht == null)
          return NIL;
        LispObject location = (LispObject) ht.get(layout);
        return location != null ? location : NIL;
      }
    };

  private static final StandardGenericFunction GENERIC_FUNCTION_NAME =
    new StandardGenericFunction("generic-function-name",
                                PACKAGE_MOP,
                                true,
                                _GENERIC_FUNCTION_NAME,
                                list1(Symbol.GENERIC_FUNCTION),
                                list1(StandardClass.STANDARD_GENERIC_FUNCTION));

  private static class CacheEntry implements java.io.Serializable
  {
    final LispObject[] array;

    CacheEntry(LispObject[] array)
    {
      this.array = array;
    }

    @Override
    public int hashCode()
    {
      int result = 0;
      for (int i = array.length; i-- > 0;)
        result ^= array[i].hashCode();
      return result;
    }

    @Override
    public boolean equals(Object object)
    {
      if (!(object instanceof CacheEntry))
        return false;
      final CacheEntry otherEntry = (CacheEntry) object;
      if (otherEntry.array.length != array.length)
        return false;
      final LispObject[] otherArray = otherEntry.array;
      for (int i = array.length; i-- > 0;)
        if (array[i] != otherArray[i])
          return false;
      return true;
    }
  }
}
