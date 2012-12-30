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

import static org.armedbear.lisp.Lisp.*;

import java.util.concurrent.ConcurrentHashMap;

public final class StandardGenericFunction extends FuncallableStandardObject
{

  ConcurrentHashMap<CacheEntry,LispObject> cache;
  ConcurrentHashMap<LispObject,LispObject> slotCache;

  public StandardGenericFunction()
  {
    super(StandardClass.STANDARD_GENERIC_FUNCTION,
          StandardClass.STANDARD_GENERIC_FUNCTION.getClassLayout().getLength());
  }

  public StandardGenericFunction(Layout layout)
  {
    super(layout);
    slots[StandardGenericFunctionClass.SLOT_INDEX_NAME] = NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_LAMBDA_LIST] = NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_REQUIRED_ARGS] = NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_OPTIONAL_ARGS] = NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_INITIAL_METHODS] = NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_METHODS] = NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_CLASS] =
      StandardClass.STANDARD_METHOD;
    slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_COMBINATION] =
      list(Symbol.STANDARD); // fixed up by clos.lisp:shared-initialize :after
    slots[StandardGenericFunctionClass.SLOT_INDEX_ARGUMENT_PRECEDENCE_ORDER] =
      NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_DECLARATIONS] = NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_CLASSES_TO_EMF_TABLE] = NIL;
    slots[StandardGenericFunctionClass.SLOT_INDEX_DOCUMENTATION] = NIL;
  }

  void finalizeInternal()
  {
    cache = null;
  }

  @Override
  public LispObject typep(LispObject type)
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
  public String printObject()
  {
    LispObject name = getGenericFunctionName();
    if (name != null)
      {
        StringBuilder sb = new StringBuilder();
        LispObject className;
        LispObject lispClass = getLispClass();
        if (lispClass instanceof LispClass)
          className = ((LispClass)lispClass).getName();
        else
          className = Symbol.CLASS_NAME.execute(lispClass);

        sb.append(className.princToString());
        sb.append(' ');
        sb.append(name.princToString());
        return unreadableString(sb.toString());
      }
    return super.printObject();
  }

  // AMOP (p. 216) specifies the following readers as generic functions:
  //   generic-function-argument-precedence-order
  //   generic-function-declarations
  //   generic-function-lambda-list
  //   generic-function-method-class
  //   generic-function-method-combination
  //   generic-function-methods
  //   generic-function-name

  private static final Primitive _GENERIC_FUNCTION_NAME 
    = new pf__generic_function_name();
  @DocString(name="%generic-function-name")
  private static final class pf__generic_function_name extends Primitive
  {
    pf__generic_function_name() 
    {
      super("%generic-function-name", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_NAME];
    }
  };

  private static final Primitive _SET_GENERIC_FUNCTION_NAME 
    = new pf__set_generic_function_name();
  @DocString(name="%set-generic-function-name")
  private static final class pf__set_generic_function_name extends Primitive
  { 
    pf__set_generic_function_name() 
    {
      super ("%set-generic-function-name", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first).slots[StandardGenericFunctionClass.SLOT_INDEX_NAME] = second;
      return second;
    }
  };

  private static final Primitive _GENERIC_FUNCTION_LAMBDA_LIST 
    = new pf__generic_function_lambda_list();
  @DocString(name ="%generic-function-lambda-list")
  private static final class pf__generic_function_lambda_list extends Primitive {
    pf__generic_function_lambda_list() 
    {
      super("%generic-function-lambda-list", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_LAMBDA_LIST];
    }
  };

  private static final Primitive _SET_GENERIC_FUNCTION_LAMBDA_LIST 
    = new pf__set_generic_function_lambda_list();
  @DocString(name="%set-generic-function-lambdalist")
  private static final class pf__set_generic_function_lambda_list extends Primitive
  {
    pf__set_generic_function_lambda_list()
    {
      super("%set-generic-function-lambda-list", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first).slots[StandardGenericFunctionClass.SLOT_INDEX_LAMBDA_LIST] = second;
      return second;
    }
  };

  private static final Primitive GF_REQUIRED_ARGS 
    = new pf_gf_required_args();
  @DocString(name="gf-required-args")
  private static final class pf_gf_required_args extends Primitive 
  {
    pf_gf_required_args()
    {
      super("gf-required-args", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_REQUIRED_ARGS];
    }
  };

  private static final Primitive _SET_GF_REQUIRED_ARGS
    = new pf__set_gf_required_args();
  @DocString(name="%set-gf-required-args")
  private static final class pf__set_gf_required_args extends Primitive
  {
    pf__set_gf_required_args()
    {
      super("%set-gf-required-args", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(first);
      gf.slots[StandardGenericFunctionClass.SLOT_INDEX_REQUIRED_ARGS] = second;
      return second;
    }
  };

  private static final Primitive GF_OPTIONAL_ARGS 
    = new pf_gf_optional_args();
  @DocString(name="gf-optional-args")
  private static final class pf_gf_optional_args extends Primitive 
  {
    pf_gf_optional_args()
    {
      super("gf-optional-args", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_OPTIONAL_ARGS];
    }
  };

  private static final Primitive _SET_GF_OPTIONAL_ARGS
    = new pf__set_gf_optional_args();
  @DocString(name="%set-gf-optional-args")
  private static final class pf__set_gf_optional_args extends Primitive
  {
    pf__set_gf_optional_args()
    {
      super("%set-gf-optional-args", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(first);
      gf.slots[StandardGenericFunctionClass.SLOT_INDEX_OPTIONAL_ARGS] = second;
      return second;
    }
  };

  private static final Primitive GENERIC_FUNCTION_INITIAL_METHODS 
    = new pf_generic_function_initial_methods();
  @DocString(name="generic-function-initial-methods")
  private static final class pf_generic_function_initial_methods extends Primitive
  {
    pf_generic_function_initial_methods() 
    {
      super("generic-function-initial-methods", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_INITIAL_METHODS];
    }
  };

  private static final Primitive SET_GENERIC_FUNCTION_INITIAL_METHODS 
    = new pf_set_generic_function_initial_methods();
  @DocString(name="set-generic-function-initial-methods")
  private static final class pf_set_generic_function_initial_methods extends Primitive
  {
    pf_set_generic_function_initial_methods()
    {
      super("set-generic-function-initial-methods", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first).slots[StandardGenericFunctionClass.SLOT_INDEX_INITIAL_METHODS] = second;
      return second;
    }
  };

  private static final Primitive GENERIC_FUNCTION_METHODS 
    = new pf_generic_function_methods();
  @DocString(name="%generic-function-methods")
  private static final class pf_generic_function_methods extends Primitive
  {
    pf_generic_function_methods()
    {
      super("%generic-function-methods", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_METHODS];
    }
  };

  private static final Primitive SET_GENERIC_FUNCTION_METHODS 
    = new pf_set_generic_function_methods();
  @DocString(name="set-generic-function-methods")
  private static final class pf_set_generic_function_methods extends Primitive
  {
    pf_set_generic_function_methods()
    {
      super("set-generic-function-methods", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first).slots[StandardGenericFunctionClass.SLOT_INDEX_METHODS] = second;
      return second;
    }
  };

  private static final Primitive GENERIC_FUNCTION_METHOD_CLASS 
    = new pf_generic_function_method_class();
  @DocString(name="%generic-function-method-class")
  private static final class pf_generic_function_method_class extends Primitive
  {
    pf_generic_function_method_class()
    {
      super("%generic-function-method-class", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_CLASS];
    }
  };

  private static final Primitive SET_GENERIC_FUNCTION_METHOD_CLASS 
    = new pf_set_generic_function_method_class();
  @DocString(name="set-generic-function-method-class")
  private static final class pf_set_generic_function_method_class extends Primitive
  {
    pf_set_generic_function_method_class()
    {
      super("set-generic-function-method-class", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first).slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_CLASS] = second;
      return second;
    }
  };

  private static final Primitive GENERIC_FUNCTION_METHOD_COMBINATION 
    = new pf_generic_function_method_combination(); 
  @DocString(name="%generic-function-method-combination")
  private static final class pf_generic_function_method_combination extends Primitive 
  {
    pf_generic_function_method_combination()
    {
      super("%generic-function-method-combination", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_COMBINATION];
    }
  };

  private static final Primitive SET_GENERIC_FUNCTION_METHOD_COMBINATION 
    = new pf_set_generic_function_method_combination(); 
  @DocString(name="set-generic-function-method-combination")
  private static final class pf_set_generic_function_method_combination extends Primitive 
  {
    pf_set_generic_function_method_combination()
    {
      super("set-generic-function-method-combination", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first).slots[StandardGenericFunctionClass.SLOT_INDEX_METHOD_COMBINATION] 
        = second;
      return second;
    }
  };

  private static final Primitive GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER
    = new pf_generic_function_argument_precedence_order();
  @DocString(name="%generic-function-argument-precedence-order")
  private static final class pf_generic_function_argument_precedence_order extends Primitive
  {
    pf_generic_function_argument_precedence_order()
    { 
      super("%generic-function-argument-precedence-order", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass
                                                     .SLOT_INDEX_ARGUMENT_PRECEDENCE_ORDER];
    }
  };

  private static final Primitive SET_GENERIC_FUNCTION_ARGUMENT_PRECEDENCE_ORDER
    = new pf_set_generic_function_argument_precedence_order();
  @DocString(name="set-generic-function-argument-precedence-order")
  private static final class pf_set_generic_function_argument_precedence_order extends Primitive
  {
    pf_set_generic_function_argument_precedence_order()
    {
      super("set-generic-function-argument-precedence-order", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first)
        .slots[StandardGenericFunctionClass.SLOT_INDEX_ARGUMENT_PRECEDENCE_ORDER] = second;
      return second;
    }
  };

  private static final Primitive GENERIC_FUNCTION_DECLARATIONS
    = new pf_generic_function_declarations();
  @DocString(name="%generic-function-declarations")
  private static final class pf_generic_function_declarations extends Primitive
  {
    pf_generic_function_declarations()
    { 
      super("%generic-function-declarations", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg)
        .slots[StandardGenericFunctionClass .SLOT_INDEX_DECLARATIONS];
    }
  };

  private static final Primitive SET_GENERIC_FUNCTION_DECLARATIONS
    = new pf_set_generic_function_declarations();
  @DocString(name="set-generic-function-declarations")
  private static final class pf_set_generic_function_declarations extends Primitive
  {
    pf_set_generic_function_declarations()
    {
      super("set-generic-function-declarations", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first)
        .slots[StandardGenericFunctionClass.SLOT_INDEX_DECLARATIONS] = second;
      return second;
    }
  };



  private static final Primitive GENERIC_FUNCTION_CLASSES_TO_EMF_TABLE
    = new pf_generic_function_classes_to_emf_table();
  @DocString(name="generic-function-classes-to-emf-table")
  private static final class pf_generic_function_classes_to_emf_table extends Primitive 
  {
    pf_generic_function_classes_to_emf_table() 
    {
      super("generic-function-classes-to-emf-table", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg)
        .slots[StandardGenericFunctionClass.SLOT_INDEX_CLASSES_TO_EMF_TABLE];
    }
  };

  private static final Primitive SET_GENERIC_FUNCTION_CLASSES_TO_EMF_TABLE 
    = new pf_set_generic_function_classes_to_emf_table();
  @DocString(name="set-generic-function-classes-to-emf-table")
  private static final class pf_set_generic_function_classes_to_emf_table extends Primitive
  {
    pf_set_generic_function_classes_to_emf_table()
    {
      super("set-generic-function-classes-to-emf-table", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first)
        .slots[StandardGenericFunctionClass.SLOT_INDEX_CLASSES_TO_EMF_TABLE] = second;
      return second;
    }
  };

  private static final Primitive GENERIC_FUNCTION_DOCUMENTATION
    = new pf_generic_function_documentation();
  @DocString(name="generic-function-documentation")
  private static final class pf_generic_function_documentation extends Primitive
  {
    pf_generic_function_documentation() 
    {
      super("generic-function-documentation", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return checkStandardGenericFunction(arg).slots[StandardGenericFunctionClass.SLOT_INDEX_DOCUMENTATION];
    }
  };

  private static final Primitive SET_GENERIC_FUNCTION_DOCUMENTATION 
    = new pf_set_generic_function_documentation();
  @DocString(name="set-generic-function-documentation")
  private static final class pf_set_generic_function_documentation extends Primitive 
  {
    pf_set_generic_function_documentation()
    {
      super("set-generic-function-documentation", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      checkStandardGenericFunction(first).slots[StandardGenericFunctionClass.SLOT_INDEX_DOCUMENTATION] 
        = second;
      return second;
    }
  };

  private static final Primitive _FINALIZE_GENERIC_FUNCTION 
    = new pf__finalize_generic_function();
  @DocString(name="%finalize-generic-function",
             args="generic-function")
  private static final class  pf__finalize_generic_function extends Primitive
  {
    pf__finalize_generic_function()
    {
      super("%finalize-generic-function", PACKAGE_SYS, true,
            "generic-function");
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(arg);
      gf.finalizeInternal();        
      return T;
    }
  };

  private static final Primitive CACHE_EMF 
    = new pf_cache_emf();
  @DocString(name="cache-emf",
             args="generic-function args emf")
  private static final class pf_cache_emf extends Primitive 
  {
    pf_cache_emf()
    {
      super("cache-emf", PACKAGE_SYS, true, "generic-function args emf");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second,
                              LispObject third)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(first);
      LispObject args = second;
      int numberOfRequiredArgs
        = gf.slots[StandardGenericFunctionClass.SLOT_INDEX_REQUIRED_ARGS]
        .length();
      LispObject[] array = new LispObject[numberOfRequiredArgs];
      for (int i = numberOfRequiredArgs; i-- > 0;)
        {
          array[i] = gf.getArgSpecialization(args.car());
          args = args.cdr();
        }
      CacheEntry specializations = new CacheEntry(array);
      ConcurrentHashMap<CacheEntry,LispObject> ht = gf.cache;
      if (ht == null)
        ht = gf.cache = new ConcurrentHashMap<CacheEntry,LispObject>();
      ht.put(specializations, third);
      return third;
    }
  };

  private static final Primitive GET_CACHED_EMF
    = new pf_get_cached_emf();
  @DocString(name="get-cached-emf",
             args="generic-function args")
  private static final class pf_get_cached_emf extends Primitive 
  {
    pf_get_cached_emf() {
      super("get-cached-emf", PACKAGE_SYS, true, "generic-function args");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(first);
      LispObject args = second;
      int numberOfRequiredArgs
        = gf.slots[StandardGenericFunctionClass.SLOT_INDEX_REQUIRED_ARGS]
        .length();
      LispObject[] array = new LispObject[numberOfRequiredArgs];
      for (int i = numberOfRequiredArgs; i-- > 0;)
        {
          array[i] = gf.getArgSpecialization(args.car());
          args = args.cdr();
        }
      CacheEntry specializations = new CacheEntry(array);
      ConcurrentHashMap<CacheEntry,LispObject> ht = gf.cache;
      if (ht == null)
        return NIL;
      LispObject emf = (LispObject) ht.get(specializations);
      return emf != null ? emf : NIL;
    }
  };

  /**
   * Returns an object representing generic function 
   * argument <tt>arg</tt> in a <tt>CacheEntry</tt>
   *
   * <p>In the simplest case, when this generic function
   * does not have EQL specialized methods, and therefore
   * only argument types are relevant for choosing
   * applicable methods, the value returned is the 
   * class of <tt>arg</tt>
   *
   * <p>If the function has EQL specialized methods: 
   *   - if <tt>arg</tt> is EQL to some of the EQL-specializers,
   *     a special object representing equality to that specializer
   *     is returned.
   *   - otherwise class of the <tt>arg</tt> is returned.
   *
   * <p>Note that we do not consider argument position, when
   * calculating arg specialization. In rare cases (when one argument
   * is eql-specialized to a symbol specifying class of another
   * argument) this may result in redundant cache entries caching the
   * same method. But the method cached is anyway correct for the
   * arguments (because in case of cache miss, correct method is
   * calculated by other code, which does not rely on
   * getArgSpecialization; and because EQL is true only for objects of
   * the same type, which guaranties that if a type-specialized
   * methods was chached by eql-specialization, all the cache hits
   * into this records will be from args of the conforming type).
   *
   * <p>Consider:
   * <pre><tt>
   * (defgeneric f (a b))
   *
   * (defmethod f (a (b (eql 'symbol)))
   *   "T (EQL 'SYMBOL)")
   *
   * (defmethod f ((a symbol) (b (eql 'symbol)))
   *   "SYMBOL (EQL 'SYMBOL)")
   *
   * (f 12 'symbol)
   * => "T (EQL 'SYMBOL)"
   *
   * (f 'twelve 'symbol)
   * => "SYMBOL (EQL 'SYMBOL)"
   *
   * (f 'symbol 'symbol)
   * => "SYMBOL (EQL 'SYMBOL)"
   *
   * </tt></pre>
   *
   * After the two above calls <tt>cache</tt> will contain three keys:
   * <pre>
   * { class FIXNUM, EqlSpecialization('SYMBOL) }
   * { class SYMBOL, EqlSpecialization('SYMBOL) }
   * { EqlSpecialization('SYMBOL), EqlSpecialization('SYMBOL) }.
   * </pre>
   */     
  LispObject getArgSpecialization(LispObject arg)
  {
    for (EqlSpecialization eqlSpecialization : eqlSpecializations)
      {
        if (eqlSpecialization.eqlTo.eql(arg))
          return eqlSpecialization;
      }
    return arg.classOf();
  }

  private static final Primitive _GET_ARG_SPECIALIZATION 
    = new pf__get_arg_specialization();
  @DocString(name="%get-arg-specialization",
             args="generic-function arg")
  private static final class pf__get_arg_specialization extends Primitive
  {
    pf__get_arg_specialization() 
    {
      super("%get-arg-specialization", PACKAGE_SYS, true, "generic-function arg");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(first);
      return gf.getArgSpecialization(second);
    }
  };

  private static final Primitive CACHE_SLOT_LOCATION 
    = new pf_cache_slot_location(); 
  @DocString(name="cache-slot-location",
           args="generic-function layout location")
  private static final class pf_cache_slot_location extends Primitive
  {
    pf_cache_slot_location()
    {
      super("cache-slot-location", PACKAGE_SYS, true, "generic-function layout location");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(first);
      LispObject layout = second;
      LispObject location = third;
      ConcurrentHashMap<LispObject,LispObject> ht = gf.slotCache;
      if (ht == null)
        ht = gf.slotCache = new ConcurrentHashMap<LispObject,LispObject>();
      ht.put(layout, location);
      return third;
    }
  };

  private static final Primitive GET_CACHED_SLOT_LOCATION 
    = new pf_get_cached_slot_location();
  @DocString(name="get-cached-slot-location")
  private static final class pf_get_cached_slot_location extends Primitive
  {
    pf_get_cached_slot_location()
    {
      super("get-cached-slot-location", PACKAGE_SYS, true, "generic-function layout");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(first);
      LispObject layout = second;
      ConcurrentHashMap<LispObject,LispObject> ht = gf.slotCache;
      if (ht == null)
        return NIL;
      LispObject location = (LispObject) ht.get(layout);
      return location != null ? location : NIL;
    }
  };

  private static class CacheEntry
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

  EqlSpecialization eqlSpecializations[] = new EqlSpecialization[0];

  private static final Primitive _INIT_EQL_SPECIALIZATIONS  
    = new pf__init_eql_specializations();
  @DocString(name="%init-eql-specializations",
             args="generic-function eql-specilizer-objects-list")
  private static final class pf__init_eql_specializations extends Primitive
  {
    pf__init_eql_specializations()
    {
      super("%init-eql-specializations", PACKAGE_SYS, true, 
            "generic-function eql-specilizer-objects-list");
    }
    @Override
    public LispObject execute(LispObject first, LispObject second)
    {
      final StandardGenericFunction gf = checkStandardGenericFunction(first);
      LispObject eqlSpecializerObjects = second;
      gf.eqlSpecializations = new EqlSpecialization[eqlSpecializerObjects.length()];
      for (int i = 0; i < gf.eqlSpecializations.length; i++) {
        gf.eqlSpecializations[i] = new EqlSpecialization(eqlSpecializerObjects.car());
        eqlSpecializerObjects = eqlSpecializerObjects.cdr();
      }
      return NIL;
    }
  };

  private static class EqlSpecialization extends LispObject
  {
    public LispObject eqlTo;

    public EqlSpecialization(LispObject eqlTo)
    {
        this.eqlTo = eqlTo;
    }
  }
  
  public static final StandardGenericFunction checkStandardGenericFunction(LispObject obj)
  {
    if (obj instanceof StandardGenericFunction)
      return (StandardGenericFunction) obj;
    return (StandardGenericFunction) // Not reached.
      type_error(obj, Symbol.STANDARD_GENERIC_FUNCTION);
  }
}
