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

import static org.armedbear.lisp.Lisp.*;

public final class HashTableFunctions
{
  static final LispObject FUNCTION_EQ =
    Symbol.EQ.getSymbolFunction();
  static final LispObject FUNCTION_EQL =
    Symbol.EQL.getSymbolFunction();
  static final LispObject FUNCTION_EQUAL =
    Symbol.EQUAL.getSymbolFunction();
  static final LispObject FUNCTION_EQUALP =
    Symbol.EQUALP.getSymbolFunction();

  // ### %make-hash-table
  private static final Primitive _MAKE_HASH_TABLE =
    new Primitive("%make-hash-table", PACKAGE_SYS, false)
    {
      @Override
      public LispObject execute(LispObject test, LispObject size,
                                LispObject rehashSize, LispObject rehashThreshold)

      {
        final int n = Fixnum.getValue(size);
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

      {
          return checkHashTable(ht).gethash(key);
      }
      
      @Override
      public LispObject execute(LispObject key, LispObject ht,
                                LispObject defaultValue)

      {
          return checkHashTable(ht).gethash(key, defaultValue);
      }
    };

  // ### gethash1 key hash-table => value
  private static final Primitive GETHASH1 =
    new Primitive(Symbol.GETHASH1, "key hash-table")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        final HashTable ht = checkHashTable(second);
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

      {
          return checkHashTable(ht).puthash(key, value);
      }
      @Override
      public LispObject execute(LispObject key, LispObject ht,
                                LispObject ignored, LispObject value)

      {
          return checkHashTable(ht).puthash(key, value);
      }
    };

  // remhash key hash-table => generalized-boolean
  private static final Primitive REMHASH =
    new Primitive(Symbol.REMHASH, "key hash-table")
    {
      @Override
      public LispObject execute(LispObject key, LispObject ht)

      {
            return checkHashTable(ht).remhash(key);
      }
    };

  // ### clrhash hash-table => hash-table
  private static final Primitive CLRHASH =
    new Primitive(Symbol.CLRHASH, "hash-table")
    {
      @Override
      public LispObject execute(LispObject ht)
      {
          checkHashTable(ht).clear();
          return ht;
      }
    };

  // ### hash-table-count
  private static final Primitive HASH_TABLE_COUNT =
    new Primitive(Symbol.HASH_TABLE_COUNT, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return Fixnum.getInstance(checkHashTable(arg).getCount());
      }
    };

  // ### sxhash object => hash-code
  private static final Primitive SXHASH =
    new Primitive(Symbol.SXHASH, "object")
    {
      @Override
      public LispObject execute(LispObject arg)
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
      public LispObject execute(LispObject arg)
      {
        return Fixnum.getInstance(arg.psxhash());
      }
    };

  // ### hash-table-p
  private static final Primitive HASH_TABLE_P =
    new Primitive(Symbol.HASH_TABLE_P,"object")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
        return arg instanceof HashTable ? T : NIL;
      }
    };

  // ### hash-table-entries
  private static final Primitive HASH_TABLE_ENTRIES =
    new Primitive("hash-table-entries", PACKAGE_SYS, false)
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkHashTable(arg).ENTRIES();
      }
    };

  // ### hash-table-test
  private static final Primitive HASH_TABLE_TEST =
    new Primitive(Symbol.HASH_TABLE_TEST, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkHashTable(arg).getTest();
      }
    };

  // ### hash-table-size
  private static final Primitive HASH_TABLE_SIZE =
    new Primitive(Symbol.HASH_TABLE_SIZE, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return Fixnum.getInstance(checkHashTable(arg).getSize());
      }
    };

  // ### hash-table-rehash-size
  private static final Primitive HASH_TABLE_REHASH_SIZE =
    new Primitive(Symbol.HASH_TABLE_REHASH_SIZE, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkHashTable(arg).getRehashSize();
      }
    };

  // ### hash-table-rehash-threshold
  private static final Primitive HASH_TABLE_REHASH_THRESHOLD =
    new Primitive(Symbol.HASH_TABLE_REHASH_THRESHOLD, "hash-table")
    {
      @Override
      public LispObject execute(LispObject arg)
      {
          return checkHashTable(arg).getRehashThreshold();
      }
    };

  // ### maphash
  private static final Primitive MAPHASH =
    new Primitive(Symbol.MAPHASH, "function hash-table")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second)

      {
        return checkHashTable(second).MAPHASH(first);
      }
    };

protected static HashTable checkHashTable(LispObject ht) {
        if (ht instanceof HashTable) return (HashTable)ht;
    type_error(ht, Symbol.HASH_TABLE);    
        return null;
}
}
