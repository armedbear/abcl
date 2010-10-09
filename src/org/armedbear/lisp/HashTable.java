/*
 * HashTable.java
 *
 * Copyright (C) 2002-2007 Peter Graves
 * Copyright (C) 2010 Erik Huelsmann
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
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

import java.util.concurrent.locks.ReentrantReadWriteLock;
import static org.armedbear.lisp.Lisp.*;

public abstract class HashTable extends LispObject {

    protected static final float loadFactor = 0.75f;
    protected final LispObject rehashSize;
    protected final LispObject rehashThreshold;
    // The rounded product of the capacity and the load factor. When the number
    // of elements exceeds the threshold, the implementation calls rehash().
    protected int threshold;
    // Array containing the actual key-value mappings.
    protected HashEntry[] buckets;
    // The number of key-value pairs.
    protected int count;
    private int mask;
    final Comparator comparator;
    final private ReentrantReadWriteLock lock = new ReentrantReadWriteLock();

    protected HashTable(Comparator c, int size, LispObject rehashSize,
            LispObject rehashThreshold) {
        this.rehashSize = rehashSize;
        this.rehashThreshold = rehashThreshold;
        buckets = new HashEntry[size];
        threshold = (int) (size * loadFactor);
        comparator = c;
        mask = buckets.length - 1;
    }

    protected static int calculateInitialCapacity(int size) {
        int capacity = 1;
        while (capacity < size) {
            capacity <<= 1;
        }
        return capacity;
    }

    public final LispObject getRehashSize() {
        return rehashSize;
    }

    public final LispObject getRehashThreshold() {
        return rehashThreshold;
    }

    public int getSize() {
        return buckets.length;
    }

    public int getCount() {
        return count;
    }

    @Override
    public LispObject typeOf() {
        return Symbol.HASH_TABLE;
    }

    @Override
    public LispObject classOf() {
        return BuiltInClass.HASH_TABLE;
    }

    @Override
    public LispObject typep(LispObject type) {
        if (type == Symbol.HASH_TABLE) {
            return T;
        }
        if (type == BuiltInClass.HASH_TABLE) {
            return T;
        }
        return super.typep(type);
    }

    @Override
    public boolean equalp(LispObject obj) {
        if (this == obj) {
            return true;
        }
        if (obj instanceof HashTable) {
            HashTable ht = (HashTable) obj;
            if (count != ht.count) {
                return false;
            }
            if (getTest() != ht.getTest()) {
                return false;
            }
            LispObject entries = ENTRIES();
            while (entries != NIL) {
                LispObject entry = entries.car();
                LispObject key = entry.car();
                LispObject value = entry.cdr();
                if (!value.equalp(ht.get(key))) {
                    return false;
                }
                entries = entries.cdr();
            }
            return true;
        }
        return false;
    }

    @Override
    public LispObject getParts() {
        LispObject parts = NIL;
        for (int i = 0; i < buckets.length; i++) {
            HashEntry e = buckets[i];
            while (e != null) {
                parts = parts.push(new Cons("KEY [bucket " + i + "]", e.key));
                parts = parts.push(new Cons("VALUE", e.value));
                e = e.next;
            }
        }
        return parts.nreverse();
    }

    public void clear() {
        lock.writeLock().lock();
        try {
            buckets = new HashEntry[buckets.length];
            count = 0;
        } finally {
            lock.writeLock().unlock();
        }
    }

    // gethash key hash-table &optional default => value, present-p
    public LispObject gethash(LispObject key) {
        LispObject value = get(key);
        final LispObject presentp;
        if (value == null) {
            value = presentp = NIL;
        } else {
            presentp = T;
        }
        return LispThread.currentThread().setValues(value, presentp);
    }

    // gethash key hash-table &optional default => value, present-p
    public LispObject gethash(LispObject key, LispObject defaultValue) {
        LispObject value = get(key);
        final LispObject presentp;
        if (value == null) {
            value = defaultValue;
            presentp = NIL;
        } else {
            presentp = T;
        }
        return LispThread.currentThread().setValues(value, presentp);
    }

    public LispObject gethash1(LispObject key) {
        final LispObject value = get(key);
        return value != null ? value : NIL;
    }

    public LispObject puthash(LispObject key, LispObject newValue) {
        put(key, newValue);
        return newValue;
    }

    // remhash key hash-table => generalized-boolean
    public LispObject remhash(LispObject key) {
        // A value in a Lisp hash table can never be null, so...
        return remove(key) != null ? T : NIL;
    }

    @Override
    public String writeToString() {
        if (Symbol.PRINT_READABLY.symbolValue(LispThread.currentThread()) != NIL) {
            error(new PrintNotReadable(list(Keyword.OBJECT, this)));
            return null; // Not reached.
        }
        StringBuilder sb = new StringBuilder(getTest().writeToString());
        sb.append(' ');
        sb.append(Symbol.HASH_TABLE.writeToString());
        sb.append(' ');
        sb.append(count);
        if (count == 1) {
            sb.append(" entry");
        } else {
            sb.append(" entries");
        }
        sb.append(", ");
        sb.append(buckets.length);
        sb.append(" buckets");
        return unreadableString(sb.toString());
    }

    public Symbol getTest() {
        return comparator.getTest();
    }

    public LispObject get(LispObject key) {
        lock.readLock().lock();
        try {
            int index = comparator.hash(key) & mask;
            HashEntry e = buckets[index];
            while (e != null) {
                if (comparator.keysEqual(key, e.key)) {
                    return e.value;
                }
                e = e.next;
            }
            return null;
        } finally {
            lock.readLock().unlock();
        }
    }

    public void put(LispObject key, LispObject value) {
        lock.writeLock().lock();
        try {
            int index = comparator.hash(key) & mask;
            for (HashEntry e = buckets[index]; e != null; e = e.next) {
                if (comparator.keysEqual(key, e.key)) {
                    e.value = value;
                    return;
                }
            }
            // Not found. We need to add a new entry.
            if (++count > threshold) {
                rehash();
                // Need a new hash value to suit the bigger table.
                index = comparator.hash(key) & mask;
            }
            buckets[index] = new HashEntry(key, value, buckets[index]);
        } finally {
            lock.writeLock().unlock();
        }
    }

    public LispObject remove(LispObject key) {
        lock.writeLock().lock();
        try {
            int index = comparator.hash(key) & mask;

            HashEntry e = buckets[index];
            HashEntry last = null;
            while (e != null) {
                if (comparator.keysEqual(key, e.key)) {
                    if (last == null) {
                        buckets[index] = e.next;
                    } else {
                        last.next = e.next;
                    }
                    --count;
                    return e.value;
                }
                last = e;
                e = e.next;
            }
            return null;
        } finally {
            lock.writeLock().unlock();
        }
    }

    protected void rehash() {
        lock.writeLock().lock();
        try {
            final int newCapacity = buckets.length * 2;
            threshold = (int) (newCapacity * loadFactor);
            mask = newCapacity - 1;
            HashEntry[] newBuckets = new HashEntry[newCapacity];

            for (int i = buckets.length; i-- > 0;) {
                HashEntry e = buckets[i];
                while (e != null) {
                    final int index = comparator.hash(e.key) & mask;
                    newBuckets[index] = new HashEntry(e.key, e.value, newBuckets[index]);
                    e = e.next;
                }
            }
            buckets = newBuckets;
        } finally {
            lock.writeLock().unlock();
        }
    }

    // Returns a list of (key . value) pairs.
    public LispObject ENTRIES() {
        LispObject list = NIL;
        for (int i = buckets.length; i-- > 0;) {
            HashEntry e = buckets[i];
            while (e != null) {
                list = new Cons(new Cons(e.key, e.value), list);
                e = e.next;
            }
        }
        return list;
    }

    public LispObject MAPHASH(LispObject function) {
        for (int i = buckets.length; i-- > 0;) {
            HashEntry e = buckets[i];
            while (e != null) {
                function.execute(e.key, e.value);
                e = e.next;
            }
        }
        return NIL;
    }

    protected static class Comparator {

        Symbol getTest() {
            return Symbol.EQ;
        }

        boolean keysEqual(LispObject key1, LispObject key2) {
            return key1 == key2;
        }

        int hash(LispObject key) {
            return key.sxhash();
        }
    }

    protected static class EqlComparator extends Comparator {

        @Override
        Symbol getTest() {
            return Symbol.EQL;
        }

        @Override
        boolean keysEqual(LispObject key1, LispObject key2) {
            return key1.eql(key2);
        }
    }

    protected static class EqualComparator extends Comparator {

        @Override
        Symbol getTest() {
            return Symbol.EQUAL;
        }

        @Override
        boolean keysEqual(LispObject key1, LispObject key2) {
            return key1.equal(key2);
        }
    }

    protected static class EqualpComparator extends Comparator {

        @Override
        Symbol getTest() {
            return Symbol.EQUALP;
        }

        @Override
        boolean keysEqual(LispObject key1, LispObject key2) {
            return key1.equalp(key2);
        }

        @Override
        int hash(LispObject key) {
            return key.psxhash();
        }
    }

    protected static class HashEntry {

        LispObject key;
        LispObject value;
        HashEntry next;

        HashEntry(LispObject key, LispObject value, HashEntry next) {
            this.key = key;
            this.value = value;
            this.next = next;
        }
    }

    // For EQUALP hash tables.
    @Override
    public int psxhash() {
        long result = 2062775257; // Chosen at random.
        result = mix(result, count);
        result = mix(result, getTest().sxhash());
        return (int) (result & 0x7fffffff);
    }
}
