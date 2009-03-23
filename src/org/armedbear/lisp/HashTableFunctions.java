/*
 * HashTableFunctions.java
 *
 * Copyright (C) 2002-2006 Peter Graves
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

public final class HashTableFunctions extends Lisp
{
  private static final LispObject FUNCTION_EQ =
    Symbol.EQ.getSymbolFunction();
  private static final LispObject FUNCTION_EQL =
    Symbol.EQL.getSymbolFunction();
  private static final LispObject FUNCTION_EQUAL =
    Symbol.EQUAL.getSymbolFunction();
  private static final LispObject FUNCTION_EQUALP =
    Symbol.EQUALP.getSymbolFunction();

  // ### %make-hash-table
  private static final Primitive _MAKE_HASH_TABLE =
    new Primitive("%make-hash-table", PACKAGE_SYS, false)
    {
      @Override
      public LispObject execute(LispObject test, LispObject size,
                                LispObject rehashSize, LispObject rehashThreshold)
        throws ConditionThrowable
      {
        final int n;
        try
          {
            n = ((Fixnum)size).value;
          }
        catch (ClassCastException e)
          {
            return type_error(size, Symbol.FIXNUM);
          }
        if (test == FUNCTION_EQL || test == NIL)
          return new EqlHashTable(n, rehashSize, rehashThreshold);
        if (test == FUNCTION_EQ)
          return new EqHashTable(n, rehashSize, rehashThreshold);
        if (test == FUNCTION_EQUAL)
          return new EqualHashTable(n, rehashSize, rehashThreshold);
        if (test == FUNCTION_EQUALP)
          return new EqualpHashTable(n, rehashSize, rehashThreshold);
        return error(new LispError("Unsupported test for MAKE-HASH-TABLE: " +
                                    test.writeToString()));
      }
    };

  // ### gethash key hash-table &optional default => value, present-p
  private static final Primitive GETHASH =
    new Primitive(Symbol.GETHASH, "key hash-table &optional default")
    {
      @Override
      public LispObject execute(LispObject key, LispObject ht)
        throws ConditionThrowable
      {
        try
          {
            return ((HashTable)ht).gethash(key);
          }
        catch (ClassCastException e)
          {
            return type_error(ht, Symbol.HASH_TABLE);
          }
      }
      @Override
      public LispObject execute(LispObject key, LispObject ht,
                                LispObject defaultValue)
        throws ConditionThrowable
      {
        try
          {
            return ((HashTable)ht).gethash(key, defaultValue);
          }
        catch (ClassCastException e)
          {
            return type_error(ht, Symbol.HASH_TABLE);
          }
      }
    };

  // ### gethash1 key hash-table => value
  private static final Primitive GETHASH1 =
    new Primitive(Symbol.GETHASH1, "key hash-table")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        final HashTable ht;
        try
          {
            ht = (HashTable) second;
          }
        catch (ClassCastException e)
          {
            return type_error(second, Symbol.HASH_TABLE);
          }
        synchronized (ht)
          {
            final LispObject value = ht.get(first);
            return value != null ? value : NIL;
          }
      }
    };

  // ### puthash key hash-table new-value &optional default => value
  private static final Primitive PUTHASH =
    new Primitive(Symbol.PUTHASH,
                  "key hash-table new-value &optional default")
    {
      @Override
      public LispObject execute(LispObject key, LispObject ht,
                                LispObject value)
        throws ConditionThrowable
      {
        try
          {
            return ((HashTable)ht).puthash(key, value);
          }
        catch (ClassCastException e)
          {
            return type_error(ht, Symbol.HASH_TABLE);
          }
      }
      @Override
      public LispObject execute(LispObject key, LispObject ht,
                                LispObject ignored, LispObject value)
        throws ConditionThrowable
      {
        try
          {
            return ((HashTable)ht).puthash(key, value);
          }
        catch (ClassCastException e)
          {
            return type_error(ht, Symbol.HASH_TABLE);
          }
      }
    };

  // remhash key hash-table => generalized-boolean
  private static final Primitive REMHASH =
    new Primitive(Symbol.REMHASH, "key hash-table")
    {
      @Override
      public LispObject execute(LispObject key, LispObject ht)
        throws ConditionThrowable
      {
        try
          {
            return ((HashTable)ht).remhash(key);
          }
        catch (ClassCastException e)
          {
            return type_error(ht, Symbol.HASH_TABLE);
          }
      }
    };

  // ### clrhash hash-table => hash-table
  private static final Primitive CLRHASH =
    new Primitive(Symbol.CLRHASH, "hash-table")
    {
      @Override
      public LispObject execute(LispObject ht) throws ConditionThrowable
      {
        try
          {
            ((HashTable)ht).clear();
            return ht;
          }
        catch (ClassCastException e)
          {
            return type_error(ht, Symbol.HASH_TABLE);
          }
      }
    };

  // ### hash-table-count
  private static final Primitive HASH_TABLE_COUNT =
    new Primitive(Symbol.HASH_TABLE_COUNT, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return Fixnum.getInstance(((HashTable)arg).getCount());
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.HASH_TABLE);
          }
      }
    };

  // ### sxhash object => hash-code
  private static final Primitive SXHASH =
    new Primitive(Symbol.SXHASH, "object")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return Fixnum.getInstance(arg.sxhash());
      }
    };

  // ### psxhash object => hash-code
  // For EQUALP hash tables.
  private static final Primitive PSXHASH =
    new Primitive("psxhash", PACKAGE_SYS, true, "object")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return Fixnum.getInstance(arg.psxhash());
      }
    };

  // ### hash-table-p
  private static final Primitive HASH_TABLE_P =
    new Primitive(Symbol.HASH_TABLE_P,"object")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        return arg instanceof HashTable ? T : NIL;
      }
    };

  // ### hash-table-entries
  private static final Primitive HASH_TABLE_ENTRIES =
    new Primitive("hash-table-entries", PACKAGE_SYS, false)
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((HashTable)arg).ENTRIES();
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.HASH_TABLE);
          }
      }
    };

  // ### hash-table-test
  private static final Primitive HASH_TABLE_TEST =
    new Primitive(Symbol.HASH_TABLE_TEST, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((HashTable)arg).getTest();
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.HASH_TABLE);
          }
      }
    };

  // ### hash-table-size
  private static final Primitive HASH_TABLE_SIZE =
    new Primitive(Symbol.HASH_TABLE_SIZE, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return Fixnum.getInstance(((HashTable)arg).getSize());
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.HASH_TABLE);
          }
      }
    };

  // ### hash-table-rehash-size
  private static final Primitive HASH_TABLE_REHASH_SIZE =
    new Primitive(Symbol.HASH_TABLE_REHASH_SIZE, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((HashTable)arg).getRehashSize();
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.HASH_TABLE);
          }
      }
    };

  // ### hash-table-rehash-threshold
  private static final Primitive HASH_TABLE_REHASH_THRESHOLD =
    new Primitive(Symbol.HASH_TABLE_REHASH_THRESHOLD, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg) throws ConditionThrowable
      {
        try
          {
            return ((HashTable)arg).getRehashThreshold();
          }
        catch (ClassCastException e)
          {
            return type_error(arg, Symbol.HASH_TABLE);
          }
      }
    };

  // ### maphash
  private static final Primitive MAPHASH =
    new Primitive(Symbol.MAPHASH, "function hash-table")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)
        throws ConditionThrowable
      {
        HashTable ht;
        try
          {
            ht = (HashTable) second;
          }
        catch (ClassCastException e)
          {
            return type_error(second, Symbol.HASH_TABLE);
          }
        return ht.MAPHASH(first);
      }
    };
}
