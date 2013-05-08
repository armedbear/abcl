/*
 * EMFCache.java
 *
 * Copyright (C) 2003-2006 Peter Graves, 2013 Rudolf Schlatte
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

public final class EMFCache extends LispObject
{
  ConcurrentHashMap<CacheEntry,LispObject> cache
    = new ConcurrentHashMap<CacheEntry,LispObject>();;
  EqlSpecialization eqlSpecializations[] = new EqlSpecialization[0];

  void clearCache()
  {
    cache = new ConcurrentHashMap<CacheEntry,LispObject>();
  }

  @Override
  public String printObject()
  {
    return unreadableString("EMF-CACHE");
  }

  static final FuncallableStandardObject checkStandardGenericFunction(LispObject obj)
  {
    if (obj instanceof FuncallableStandardObject)
      return (FuncallableStandardObject) obj;
    return (FuncallableStandardObject) // Not reached.
      type_error(obj, Symbol.STANDARD_GENERIC_FUNCTION);
  }

  private static class EqlSpecialization extends LispObject
  {
    public LispObject eqlTo;

    public EqlSpecialization(LispObject eqlTo)
    {
        this.eqlTo = eqlTo;
    }
  }

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

  private static final Primitive _MAKE_EMF_CACHE
    = new pf__make_emf_cache();
  @DocString(name="%make-emf-cache")
  private static final class  pf__make_emf_cache extends Primitive
  {
    pf__make_emf_cache()
    {
      super("%make-emf-cache", PACKAGE_SYS, true);
    }
    @Override
    public LispObject execute(LispObject arg)
    {
      return new EMFCache();
    }
  };

  private static final Primitive _REINIT_EMF_CACHE
    = new pf__reinit_emf_cache();
  @DocString(name="%reinit-emf-cache",
             args="generic-function eql-specilizer-objects-list")
  private static final class  pf__reinit_emf_cache extends Primitive
  {
    pf__reinit_emf_cache()
    {
      super("%reinit-emf-cache", PACKAGE_SYS, true,
            "generic-function eql-specializer-objects-list");
    }
    @Override
    public LispObject execute(LispObject generic_function, LispObject eql_specializers)
    {
      final FuncallableStandardObject gf = checkStandardGenericFunction(generic_function);
      EMFCache cache = gf.cache;
      cache.clearCache();
      cache.eqlSpecializations = new EqlSpecialization[eql_specializers.length()];
      for (int i = 0; i < cache.eqlSpecializations.length; i++) {
        cache.eqlSpecializations[i] = new EqlSpecialization(eql_specializers.car());
        eql_specializers = eql_specializers.cdr();
      }
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
      final FuncallableStandardObject gf = checkStandardGenericFunction(first);
      EMFCache cache = gf.cache;
      LispObject args = second;
      int numberOfRequiredArgs
        = gf.getInstanceSlotValue(Symbol.REQUIRED_ARGS).length();
      LispObject[] array = new LispObject[numberOfRequiredArgs];
      for (int i = numberOfRequiredArgs; i-- > 0;)
        {
          array[i] = cache.getArgSpecialization(args.car());
          args = args.cdr();
        }
      CacheEntry specializations = new CacheEntry(array);
      ConcurrentHashMap<CacheEntry,LispObject> ht = cache.cache;
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
      final FuncallableStandardObject gf = checkStandardGenericFunction(first);
      EMFCache cache = gf.cache;
      LispObject args = second;
      int numberOfRequiredArgs
        = gf.getInstanceSlotValue(Symbol.REQUIRED_ARGS).length();
      LispObject[] array = new LispObject[numberOfRequiredArgs];
      for (int i = numberOfRequiredArgs; i-- > 0;)
        {
          array[i] = cache.getArgSpecialization(args.car());
          args = args.cdr();
        }
      CacheEntry specializations = new CacheEntry(array);
      ConcurrentHashMap<CacheEntry,LispObject> ht = cache.cache;
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

}
