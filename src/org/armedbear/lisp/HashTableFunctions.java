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

  private static final Primitive _MAKE_HASH_TABLE 
      = new pf__make_hash_table();
  @DocString(name="%make-hash-table")
  private static final class pf__make_hash_table extends Primitive {
      pf__make_hash_table() {
        super("%make-hash-table", PACKAGE_SYS, false);
      }
        
      @Override
      public LispObject execute(LispObject test, LispObject size,
                                LispObject rehashSize, 
                                LispObject rehashThreshold)
      {
        final int n = Fixnum.getValue(size);
        if (test == FUNCTION_EQL || test == NIL)
          return HashTable.newEqlHashTable(n, rehashSize, rehashThreshold);
        if (test == FUNCTION_EQ)
          return HashTable.newEqHashTable(n, rehashSize, rehashThreshold);
        if (test == FUNCTION_EQUAL)
          return HashTable.newEqualHashTable(n, rehashSize, rehashThreshold);
        if (test == FUNCTION_EQUALP)
          return HashTable.newEqualpHashTable(n, rehashSize, rehashThreshold);
        return error(new LispError("Unsupported test for MAKE-HASH-TABLE: " +
                                    test.princToString()));
      }
    };


  private static final Primitive _MAKE_WEAK_HASH_TABLE 
    = new pf__make_weak_hash_table();
  @DocString(name="%make-weak-hash-table")
  private static final class pf__make_weak_hash_table extends Primitive {
      pf__make_weak_hash_table() {
        super("%make-weak-hash-table", PACKAGE_SYS, false);
      }
      @Override
      public LispObject execute(LispObject test, 
                                LispObject size,
                                LispObject rehashSize, 
                                LispObject rehashThreshold,
				LispObject weakness)
      {
        final int n = Fixnum.getValue(size);
        if (test == FUNCTION_EQL || test == NIL)
          return WeakHashTable.newEqlHashTable(n, rehashSize, 
					       rehashThreshold, weakness);
        if (test == FUNCTION_EQ)
          return WeakHashTable.newEqHashTable(n, rehashSize, 
                                              rehashThreshold, weakness);
        if (test == FUNCTION_EQUAL)
          return WeakHashTable.newEqualHashTable(n, rehashSize, 
						 rehashThreshold, weakness);
        if (test == FUNCTION_EQUALP)
          return WeakHashTable.newEqualpHashTable(n, rehashSize, 
						  rehashThreshold, weakness);
        return error(new LispError("Unsupported test for MAKE-HASH-TABLE: " +
                                    test.princToString()));
      }
    };

  private static final Primitive GETHASH 
    = new pf_gethash();
  @DocString(name="gethash",
             args="key hash-table &optional default",
             returns="value, present-p",
             doc="Returns the value associated with KEY in HASH-TABLE.")
  private static final class pf_gethash extends Primitive {
      pf_gethash() {
        super(Symbol.GETHASH, "key hash-table &optional default");
      }

      @Override
      public LispObject execute(LispObject key, LispObject ht)

      {
          if (ht instanceof WeakHashTable) {
              return ((WeakHashTable)ht).gethash(key);
          }
          return checkHashTable(ht).gethash(key);
      }
      
      @Override
      public LispObject execute(LispObject key, LispObject ht,
                                LispObject defaultValue)
      {
          if (ht instanceof WeakHashTable) {
              return ((WeakHashTable)ht).gethash(key, defaultValue);
          }
          return checkHashTable(ht).gethash(key, defaultValue);
      }
    };

  private static final Primitive GETHASH1 
    = new pf_gethash1();
  @DocString(name="gethash1",
             args="key hash-table", returns="value")
  private static final class pf_gethash1 extends Primitive {
      pf_gethash1() {
        super(Symbol.GETHASH1, "key hash-table");
      }
      @Override
      public LispObject execute(LispObject first, LispObject second) {
        if (second instanceof WeakHashTable) {
            final WeakHashTable ht = (WeakHashTable) second;
            synchronized (ht) {
                final LispObject value = ht.get(first);
                return value != null ? value : NIL;
            }
        } else {
            final HashTable ht = checkHashTable(second);
            synchronized (ht) {
                final LispObject value = ht.get(first);
                return value != null ? value : NIL;
            }
        }
      }
    };

  private static final Primitive PUTHASH 
    = new pf_puthash();
  @DocString(name="puthash",
             args="key hash-table new-value &optional default", returns="value")
  private static final class pf_puthash extends Primitive {
      pf_puthash() {
        super(Symbol.PUTHASH,
             "key hash-table new-value &optional default");
      }
      @Override
      public LispObject execute(LispObject key, LispObject ht,
                                LispObject value)
      {
        if (ht instanceof WeakHashTable) {
            return ((WeakHashTable)ht).puthash(key, value);
        }
        return checkHashTable(ht).puthash(key, value);
      }
      @Override
      public LispObject execute(LispObject key, LispObject ht,
                                LispObject ignored, LispObject value)
      {
        if (ht instanceof WeakHashTable) {
            return ((WeakHashTable)ht).puthash(key, value);
        }
        return checkHashTable(ht).puthash(key, value);
      }
    };

  private static final Primitive REMHASH 
    = new pf_remhash();
  @DocString(name="remhash",
             args="key hash-table", returns="generalized-boolean",
             doc="Removes the value for KEY in HASH-TABLE, if any.")
  private static final class pf_remhash extends Primitive {
      pf_remhash() {
        super(Symbol.REMHASH, "key hash-table");
      }
      @Override
      public LispObject execute(LispObject key, LispObject ht) {
        if (ht instanceof WeakHashTable) {
            return ((WeakHashTable)ht).remhash(key);
        }
        return checkHashTable(ht).remhash(key);
      }
    };

  private static final Primitive CLRHASH 
    = new pf_clrhash();
  @DocString(name="clrhash",
             args="hash-table", returns="hash-table")
  private static final class pf_clrhash extends Primitive {
      pf_clrhash() {
        super(Symbol.CLRHASH, "hash-table");
      }
      @Override
      public LispObject execute(LispObject ht)
      {
        if (ht instanceof WeakHashTable) {
            ((WeakHashTable)ht).clear();
            return ht;
        }
        checkHashTable(ht).clear();
        return ht;
      }
    };

  private static final Primitive HASH_TABLE_COUNT 
    = new pf_hash_table_count();
  @DocString(name="hash-table-count",
             args="hash-table",
             doc="Returns the number of entries in HASH-TABLE.")
  private static final class pf_hash_table_count extends Primitive {
      pf_hash_table_count() {
          super(Symbol.HASH_TABLE_COUNT, "hash-table");
      }
      @Override
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof WeakHashTable) {
              return Fixnum.getInstance(((WeakHashTable)arg).getCount());
          }
          return Fixnum.getInstance(checkHashTable(arg).getCount());
      }
    };

  private static final Primitive SXHASH 
    = new pf_sxhash();
  @DocString(name="sxhash",
             args="object => hash-code")
  private static final class pf_sxhash extends Primitive {
      pf_sxhash() {
        super(Symbol.SXHASH, "object");
      }
      @Override
      public LispObject execute(LispObject arg)
      {
        return Fixnum.getInstance(arg.sxhash());
      }
    };

  // For EQUALP hash tables.
  @DocString(name="psxhash",
             args="object")
  private static final Primitive PSXHASH 
    = new pf_psxhash();
  private static final class pf_psxhash extends Primitive  {
      pf_psxhash() {
        super("psxhash", PACKAGE_SYS, true, "object");
      }
      @Override
      public LispObject execute(LispObject arg)
      {
        return Fixnum.getInstance(arg.psxhash());
      }
    };

  private static final Primitive HASH_TABLE_P 
    = new pf_hash_table_p();
  @DocString(name="hash-table-p",
             args="object",
             doc="Whether OBJECT is an instance of a hash-table.")
  private static final class pf_hash_table_p extends Primitive {
      pf_hash_table_p(){
        super(Symbol.HASH_TABLE_P,"object");
      }
      @Override
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof WeakHashTable) return T;
          return arg instanceof HashTable ? T : NIL;
      }
    };

  private static final Primitive HASH_TABLE_ENTRIES 
    = new pf_hash_table_entries();
  @DocString(name="hah-table-entries",
             args="hash-table",
             doc="Returns a list of all key/values pairs in HASH-TABLE.")
  private static final class pf_hash_table_entries extends Primitive {
      pf_hash_table_entries() {
        super("hash-table-entries", PACKAGE_SYS, false);
      }
      @Override
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof WeakHashTable) {
              return ((WeakHashTable)arg).ENTRIES();
          }
          return checkHashTable(arg).ENTRIES();
      }
    };

  private static final Primitive HASH_TABLE_TEST 
    = new pf_hash_table_test();
  @DocString(name="hash-table-test",
             args="hash-table",
             doc="Return the test used for the keys of HASH-TABLE.")
  private static final class pf_hash_table_test extends Primitive {
      pf_hash_table_test() {
        super(Symbol.HASH_TABLE_TEST, "hash-table");
      }
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof WeakHashTable) {
              return ((WeakHashTable)arg).getTest();
          }
          return checkHashTable(arg).getTest();
      }
    };

  private static final Primitive HASH_TABLE_SIZE 
    = new pf_hash_table_size();
  @DocString(name="hash-table-size",
             args="hash-table",
             doc="Returns the number of storage buckets in HASH-TABLE.")
  private static final class pf_hash_table_size extends Primitive {
      pf_hash_table_size() {
        super(Symbol.HASH_TABLE_SIZE, "hash-table");
      }
      @Override
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof WeakHashTable) {
              return Fixnum.getInstance(((WeakHashTable)arg).getSize());
          }
          return Fixnum.getInstance(checkHashTable(arg).getSize());
      }
    };

  private static final Primitive HASH_TABLE_REHASH_SIZE 
    = new pf_hash_table_rehash_size();
  @DocString(name="hash-table-rehash-size",
             args="hash-table")
  private static final class pf_hash_table_rehash_size extends Primitive {
      pf_hash_table_rehash_size() {
        super(Symbol.HASH_TABLE_REHASH_SIZE, "hash-table");
      }
      @Override
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof WeakHashTable) {
              return ((WeakHashTable)arg).getRehashSize();
          }
          return checkHashTable(arg).getRehashSize();
      }
    };

  private static final Primitive HASH_TABLE_REHASH_THRESHOLD 
    = new pf_hash_table_rehash_threshold();
  @DocString(name="hash-table-rehash-threshold",
             args="hash-table")
  private static final class pf_hash_table_rehash_threshold extends Primitive {
      pf_hash_table_rehash_threshold() {
        super(Symbol.HASH_TABLE_REHASH_THRESHOLD, "hash-table");
      }
      @Override
      public LispObject execute(LispObject arg)
      {
          if (arg instanceof WeakHashTable) {
              return ((WeakHashTable)arg).getRehashThreshold();
          }
          return checkHashTable(arg).getRehashThreshold();
      }
    };

  private static final Primitive MAPHASH 
    = new pf_maphash();
  @DocString(name="maphash",
             args="function hash-table",
             doc="Iterates over all entries in the hash-table. For each entry,"
             + " the function is called with two arguments--the key and the"
             + " value of that entry.")
  private static final class pf_maphash extends Primitive {
      pf_maphash() {
        super(Symbol.MAPHASH, "function hash-table");
      }
      @Override
      public LispObject execute(LispObject first, LispObject second)
      {
          if (second instanceof WeakHashTable) {
              return ((WeakHashTable)second).MAPHASH(first);
          }
        return checkHashTable(second).MAPHASH(first);
      }
    };

  private static final Primitive HASH_TABLE_WEAKNESS
    = new pf_hash_table_weakness();
  @DocString(name="hash-table-weakness",
             args="hash-table",
             doc="Return weakness property of HASH-TABLE, or NIL if it has none.")
  private static final class pf_hash_table_weakness extends Primitive {
      pf_hash_table_weakness() {
          super(Symbol.HASH_TABLE_WEAKNESS, "hash-table");
      }
      @Override
      public LispObject execute(LispObject first) 
      {
          if (first instanceof HashTable) {
              return NIL;
          } else if (first instanceof WeakHashTable) {
              return ((WeakHashTable)first).getWeakness();
          }
          return error(new TypeError(first, Symbol.HASH_TABLE));
      }
  };

  protected static HashTable checkHashTable(LispObject ht) {
    if (ht instanceof HashTable) return (HashTable)ht;
    type_error(ht, Symbol.HASH_TABLE);    
    return null;
  }
}
