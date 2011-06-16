/*
 * HashTable.java
 *
 * Copyright (C) 2002-2007 Peter Graves
 * Copyright (C) 2010 Erik Huelsmann
 * Copyright (C) 2011 Mark Evenson
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
 * 02110-1301, USA.
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

import java.lang.ref.WeakReference;
import java.lang.ref.Reference;
import java.lang.ref.ReferenceQueue;
import java.util.Collections;
import java.util.HashMap;
import java.util.Map;
import java.util.concurrent.locks.ReentrantLock;

// ??? Replace standard Hashtable when this code is working; maybe not
// because we have additional places for locking here.
// 
// We can't simply extend HashTable as the methods returning HashEntry
// are referring to different types as HashEntry is internal to this
// class.
//
// XXX individuals are invited to figure out how to use Java generics
// to simplify/beautify things here, but I couldn't get the
// WeakHashTable type to be parameterized on an enclosed type.
public class WeakHashTable
    extends LispObject
    implements org.armedbear.lisp.protocol.Hashtable
{
    protected static final float loadFactor = 0.75f;
    protected final LispObject rehashSize;
    protected final LispObject rehashThreshold;
    /**
     * The rounded product of the capacity and the load factor. When the number
     * of elements exceeds the threshold, the implementation calls rehash().
     */
    protected int threshold;
    /** Array containing the actual key-value mappings. */
    @SuppressWarnings("VolatileArrayField")
    protected volatile HashEntry[] buckets;
    /** The actual current number of key-value pairs. */
    protected volatile int count;
    final Comparator comparator;
    final private ReentrantLock lock = new ReentrantLock();
    HashEntry bucketType;
    final LispObject weakness;

    private WeakHashTable(Comparator c, int size, LispObject rehashSize, 
                          LispObject rehashThreshold, LispObject weakness) 
    {
        this.rehashSize = rehashSize;
        this.rehashThreshold = rehashThreshold;
        bucketType = null;
        this.weakness = weakness;
        if (weakness.equals(Keyword.KEY)) {
            bucketType = this.new HashEntryWeakKey();
        } else if (weakness.equals(Keyword.VALUE)) {
            bucketType = this.new HashEntryWeakValue();
        } else if (weakness.equals(Keyword.KEY_AND_VALUE)) {
            bucketType = this.new HashEntryWeakKeyAndValue();
        } else if (weakness.equals(Keyword.KEY_OR_VALUE)) {
            bucketType = this.new HashEntryWeakKeyOrValue();
        } else {
            // We handle this check in the wrapping Lisp code.
            assert false 
                : "Bad weakness argument to WeakHashTable type constructor.";
        }
        buckets = bucketType.makeArray(size);
        threshold = (int) (size * loadFactor);
        comparator = c;
    }

    protected static int calculateInitialCapacity(int size) {
        int capacity = 1;
        while (capacity < size) {
            capacity <<= 1;
        }
        return capacity;
    }

    // XXX only WEAK references types are implemented for WeakHashTable.
    // XXX This enum is currently unused in this code
    enum ReferenceType {
        NORMAL, 
        WEAK, 
        SOFT
    }
  
    // XXX This enum is currently unused in this code
    enum WeaknessType {
            /** KEY means that the key of an entry must be live to
                guarantee that the entry is preserved. */
        KEY,
            /** VALUE means that the value of an entry must be live to
                guarantee that the entry is preserved. */
        VALUE,
            /** KEY-AND-VALUE means that both the key and the value
                must be live to guarantee that the entry is preserved. */
        KEY_AND_VALUE,
            /** KEY-OR-VALUE means that either the key or the value
                must be live to guarantee that the entry is preserved. */
        KEY_OR_VALUE
    }

    public static WeakHashTable newEqHashTable(int size, LispObject rehashSize,
                                               LispObject rehashThreshold,
                                               LispObject weakness) 
    {
        return new WeakHashTable(new Comparator(), size, 
                                 rehashSize, rehashThreshold, weakness);
    }

    public static WeakHashTable newEqlHashTable(int size, LispObject rehashSize,
                                                LispObject rehashThreshold,
                                                LispObject weakness) 
    {
        return new WeakHashTable(new EqlComparator(), size, 
                                 rehashSize, rehashThreshold, weakness);
    }

    public static WeakHashTable newEqualHashTable(int size, LispObject rehashSize,
                                                  LispObject rehashThreshold,
                                                  LispObject weakness) 
    {
        return new WeakHashTable(new EqualComparator(), size, 
                                 rehashSize, rehashThreshold, weakness);
    }

    public static WeakHashTable newEqualpHashTable(int size, LispObject rehashSize,
                                                   LispObject rehashThreshold,
                                                   LispObject weakness) 
    {
        return new WeakHashTable(new EqualpComparator(), size, 
                                 rehashSize, rehashThreshold, weakness);
    }

    public final LispObject getRehashSize() {
        return rehashSize;
    }

    public final LispObject getRehashThreshold() {
        return rehashThreshold;
    }

    /** How many hash buckets exist in the underlying data structure.  */
    public int getSize() {
        HashEntry[] b = getTable();
        return b.length;
    }

    /** Number of entries stored in the hash buckets. */
    public int getCount() {
        getTable(); // To force gc on entries
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

    // XXX Not thread-safe as hash entries can be GCd "out from under"
    // the invoking thread.  But the HashTable implementation
    // seemingly suffers from the same problem if entries are
    // removed/added while this method executes.
    @Override 
    public boolean equalp(LispObject obj) { 
        if (this == obj) {
            return true;
        }
        if (obj instanceof WeakHashTable) {
            WeakHashTable ht = (WeakHashTable) obj;
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
        HashEntry[] b = getTable();;
        LispObject parts = NIL;
        for (int i = 0; i < b.length; i++) {
            HashEntry e = b[i];
            while (e != null) {
                LispObject key = e.getKey();
                LispObject value = e.getValue();
                if (key != null && value != null) {
                    parts = parts.push(new Cons("KEY [bucket " + i + "]", key));
                    parts = parts.push(new Cons("VALUE", value));
                } else {
                    assert false
                        : "Dangling hash entries encountered.";
                }
                e = e.getNext();
            }
        }
        return parts.nreverse();
    }

    public void clear() {
        lock.lock();
        try {
            buckets = bucketType.makeArray(buckets.length);
            count = 0;
            while (queue.poll() != null)
                ;
        } finally {
            lock.unlock();
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
        if (bucketType instanceof HashEntryWeakKey) {
            sb.append("WEAKNESS :KEY");
        } else if (bucketType instanceof HashEntryWeakValue) {
            sb.append("WEAKNESS :VALUE");
        } else if (bucketType instanceof HashEntryWeakKeyAndValue) {
            sb.append("WEAKNESS :KEY-AND-VALUE");
        } else if (bucketType instanceof HashEntryWeakKeyOrValue) {
            sb.append("WEAKNESS :KEY-OR-VALUE");
        }
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
    
    public LispObject getWeakness() {
        return weakness;
    }

    HashEntry[] getTable() {
        lock.lock();
        try {
            bucketType.expungeQueue();
            return buckets;
        } finally {
            lock.unlock();
        }
    }

    protected HashEntry getEntry(LispObject key) {
        HashEntry[] b = getTable();
        int hash = comparator.hash(key);
        HashEntry e = b[hash & (b.length - 1)];
        while (e != null) {
            if (hash == e.getHash() 
                && (key == e.getKey() 
                    || comparator.keysEqual(key, e.getKey()))) {
                return e;
            }
            e = e.getNext();
        }
        return null;
    }

    public LispObject get(LispObject key) {
        HashEntry e = getEntry(key);
        LispObject v = (e == null) ? null : e.getValue();
        
        if (e == null || v != null) {
            return v;
        }
        return e.getValue();
    }

    public void put(LispObject key, LispObject value) {
        HashEntry e = getEntry(key);
        if (e != null) {
            e.setValue(value);
        } else {
            // Not found. We need to add a new entry.
            if (++count > threshold) {
                rehash();
            }
            int hash = comparator.hash(key);
            int index = hash & (buckets.length - 1);
            buckets[index] = bucketType.makeInstance(key, hash, 
                                                     value, buckets[index],
                                                     index);
        }
    }

    public LispObject remove(LispObject key) {
        lock.lock();
        try {
            bucketType.expungeQueue();
            int index = comparator.hash(key) & (buckets.length - 1);

            HashEntry e = buckets[index];
            HashEntry last = null;
            while (e != null) {
                LispObject entryKey = e.getKey();
                if (entryKey == null) {
                    e.clear();
                    if (last == null) {
                        buckets[index] = e.getNext();
                    } else {
                        last.setNext(e.getNext());
                    }
                    --count;
                } else if (comparator.keysEqual(key, entryKey)) {
                    e.clear();
                    if (last == null) {
                        buckets[index] = e.getNext();
                    } else {
                        last.setNext(e.getNext());
                    }
                    --count;
                    return e.getValue();
                }
                last = e;
                e = e.getNext();
            }
            return null;
        } finally {
            lock.unlock();
        }
    }

    
    /** 
     * Internal removal of the HashEntry associated with the
     * Reference used for a hashtables with soft/weak references. 
     */
    private void remove(Reference ref) {
        assert lock.isHeldByCurrentThread();
        HashEntry entry = entryLookup.get(ref);
        // assert entry != null
        //     : "Failed to find hash entry for reference.";
        if (entry == null) {
            return; // XXX how does this happen?
        }
        int index = entry.getSlot();
        HashEntry e = this.buckets[index];
        HashEntry last = null;
        while (e != null) {
            if (e.equals(entry)) {
                if (last == null) {
                    this.buckets[index] = e.getNext();
                } else {
                    last.setNext(e.getNext());
                }
                --count;
                break;
            }
            last = e;
            e = e.getNext();
        }
    }

    protected void rehash() {
        lock.lock();
        try {
            final int newCapacity = buckets.length * 2;
            threshold = (int) (newCapacity * loadFactor);
            int mask = newCapacity - 1;
            HashEntry[] newBuckets = bucketType.makeArray(newCapacity);

            for (int i = buckets.length; i-- > 0;) {
                HashEntry e = buckets[i];
                while (e != null) {
                    LispObject key = e.getKey();
                    LispObject value = e.getValue();
                    if (key == null || value == null) {
                        e.clear();
                        e = e.getNext();
                        continue;
                    }
                    final int index = comparator.hash(key) & mask;
                    e.clear();
                    newBuckets[index] 
                        = bucketType.makeInstance(key, 
                                                  e.getHash(), 
                                                  value,
                                                  newBuckets[index],
                                                  index);
                    e = e.getNext();
                }
            }
            buckets = newBuckets;
        } finally {
            lock.unlock();
        }
    }

    @Deprecated
    public LispObject ENTRIES() {
        return getEntries();
    }

    /** @returns A list of (key . value) pairs. */
    public LispObject getEntries() {
        HashEntry[] b = getTable();
        LispObject list = NIL;
        for (int i = b.length; i-- > 0;) {
            HashEntry e = b[i];
            while (e != null) {
                LispObject key = e.getKey();
                LispObject value = e.getValue();
                if (key != null && value != null) {
                    list = new Cons(new Cons(key, value), list);
                } else {
                    assert false 
                        : "ENTRIES encounted dangling entries.";
                }
                e = e.getNext();
            }
        }
        return list;
    }

    public LispObject MAPHASH(LispObject function) {
        HashEntry[] b = getTable();
        for (int i = b.length; i-- > 0;) {
            HashEntry e = b[i];
            while (e != null) {
                LispObject key = e.getKey();
                LispObject value = e.getValue();
                if (key != null && value != null) {
                    function.execute(key, value);
                } else {
                    assert false 
                        : "MAPHASH encountered dangling entries.";
                }
                e = e.getNext();
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

    abstract class HashEntry
    {
        LispObject key;
        int hash;
        volatile LispObject value;
        HashEntry next;
        int slot;

        public HashEntry() {};

        public HashEntry(LispObject key, int hash, LispObject value, 
                         HashEntry next, int slot)
        {
            this.key = key;
            this.hash = hash;
            this.value = value;
            this.next = next;
            this.slot = slot;
        }

        public LispObject getKey() {
            return key;
        }

        public void setKey(LispObject key) {
            this.key = key;
        }

        public int getHash() {
            return hash;
        }

        public void setHash(int hash) {
            this.hash = hash;
        }

        public LispObject getValue() {
            return value;
        }

        public void setValue(LispObject value) {
            this.value = value;
        }

        public HashEntry getNext() {
            return next;
        }

        public void setNext(HashEntry next) {
            this.next = next;
        }

        public int getSlot() {
            return slot;
        }
        
        public void setSlot(int slot) {
            this.slot = slot;
        }

        abstract HashEntry[] makeArray(int length);

        abstract HashEntry makeInstance(LispObject key, int hash, 
                                        LispObject value, 
                                        HashEntry next, int slot);
        abstract void expungeQueue();
        abstract void clear();
    }

    ReferenceQueue<LispObject> queue 
        = new ReferenceQueue<LispObject>();

    Map<Reference, HashEntry> entryLookup
        = Collections.synchronizedMap(new HashMap<Reference, HashEntry>());

    class HashEntryWeakKey 
        extends HashEntry
    {
        private WeakReference<LispObject> key;
        
        public HashEntryWeakKey() {};

        public HashEntryWeakKey(LispObject key, int hash, LispObject value, 
                                HashEntry next, int slot)
        {
            this.hash = hash;
            this.value = value;
            this.next = next;
            this.slot = slot;

            this.key = new WeakReference<LispObject>(key, queue);
            entryLookup.put(this.key, this);
        }

        public LispObject getKey() {
            return key.get();
        }

        public void setKey(LispObject key) {
            java.lang.ref.WeakReference<LispObject> old = this.key;
            old.clear();
            this.key = new WeakReference<LispObject>(key, queue);
            entryLookup.put(this.key, this);
        }

        HashEntryWeakKey[] makeArray(int length) {
            return new HashEntryWeakKey[length];
        }

        HashEntry makeInstance(LispObject key, int hash, LispObject value, 
                               HashEntry next, int slot) 
        {
            return new HashEntryWeakKey(key, hash, value, next, slot);
        } 

        void expungeQueue() {
            Reference ref = queue.poll();
            while (ref != null) {
                WeakHashTable.this.remove(ref);
                entryLookup.remove(ref);
                ref = queue.poll();
            }
        }

        /** Remove referenced objects from GC queue structures. */
        void clear() {
            key.clear();
            assert entryLookup.containsKey(key) 
                : "Key was not in lookup table";
            entryLookup.remove(key);
        }
    }

    class HashEntryWeakValue
        extends HashEntry
    {
        private WeakReference<LispObject> value;
        
        public HashEntryWeakValue() {};

        public HashEntryWeakValue(LispObject key, int hash, LispObject value, 
                                  HashEntry next, int slot)
        {
            this.hash = hash;
            this.key = key;
            this.next = next;
            this.slot = slot;

            this.value = new WeakReference<LispObject>(value, queue);
            entryLookup.put(this.value, this);
        }

        public LispObject getValue() {
            return value.get();
        }

        public void setValue(LispObject value) {
            java.lang.ref.WeakReference<LispObject> old = this.value;
            old.clear();
            this.value = new WeakReference<LispObject>(value, queue);
            entryLookup.put(this.value, this);
        }

        HashEntryWeakValue[] makeArray(int length) {
            return new HashEntryWeakValue[length];
        }

        HashEntryWeakValue makeInstance(LispObject key, int hash, LispObject value, 
                               HashEntry next, int slot) 
        {
            return new HashEntryWeakValue(key, hash, value, next, slot);
        } 

        void expungeQueue() {
            Reference ref = queue.poll();
            while (ref != null) {
                WeakHashTable.this.remove(ref);
                entryLookup.remove(ref);
                ref = queue.poll();
            }
        }

        /** Remove referenced objects from GC queue structures. */
        void clear() {
            value.clear();
            assert entryLookup.containsKey(value) 
                : "Value was not in lookup table.";
            entryLookup.remove(value);
        }
    }

    class HashEntryWeakKeyAndValue
        extends HashEntry
    {
        private WeakReference<LispObject> key;
        private WeakReference<LispObject> value;
        
        public HashEntryWeakKeyAndValue() {};

        public HashEntryWeakKeyAndValue(LispObject key, int hash, 
                                        LispObject value, 
                                        HashEntry next, int slot)
        {
            this.hash = hash;
            this.next = next;
            this.slot = slot;
            
            this.key = new WeakReference<LispObject>(key, queue);
            entryLookup.put(this.key, this);

            this.value = new WeakReference<LispObject>(value, queue);
            entryLookup.put(this.value, this);
            
        }

        public LispObject getKey() {
            return key.get();
        }

        public void setKey(LispObject key) {
            java.lang.ref.WeakReference<LispObject> old = this.key;
            entryLookup.remove(old);
            old.clear();
            this.key = new WeakReference<LispObject>(key, queue);
            entryLookup.put(this.key, this);
        }

        public LispObject getValue() {
            return value.get();
        }

        public void setValue(LispObject value) {
            java.lang.ref.WeakReference<LispObject> old = this.value;
            entryLookup.remove(old);
            old.clear();
            this.value = new WeakReference<LispObject>(value, queue);
            entryLookup.put(this.value, this);
        }

        HashEntryWeakKeyAndValue[] makeArray(int length) {
            return new HashEntryWeakKeyAndValue[length];
        }

        HashEntryWeakKeyAndValue makeInstance(LispObject key, int hash, 
                                              LispObject value, 
                                              HashEntry next, int slot) 
        {
            return new HashEntryWeakKeyAndValue(key, hash, value, next, slot);
        } 

        void expungeQueue() {
            Reference ref = queue.poll();
            while (ref != null) {
                HashEntry entry = entryLookup.get(ref);
                if (entry == null) {
                    ref = queue.poll();
                    continue;
                }
                if (entry.getKey() == null
                    && entry.getValue() == null) {
                    WeakHashTable.this.remove(ref);
                    entryLookup.remove(ref);
                } else {
                    entryLookup.remove(ref);
                }
                ref = queue.poll();
            }
        }

        /** Remove referenced objects from GC queue structures. */
        void clear() {
            key.clear();
            value.clear();
            entryLookup.remove(key);
            entryLookup.remove(value);
        }
    }

    class HashEntryWeakKeyOrValue
        extends HashEntryWeakKeyAndValue
    {
        public HashEntryWeakKeyOrValue() {};

        public HashEntryWeakKeyOrValue(LispObject key, int hash, 
                                       LispObject value, 
                                       HashEntry next, int slot)
        {
            super(key, hash, value, next, slot);
        }
        HashEntryWeakKeyOrValue[] makeArray(int length) {
            return new HashEntryWeakKeyOrValue[length];
        }

        HashEntryWeakKeyOrValue makeInstance(LispObject key, int hash, 
                                             LispObject value, 
                                             HashEntry next, int slot) 
        {
            return new HashEntryWeakKeyOrValue(key, hash, value, next, slot);
        } 

        void expungeQueue() {
            Reference ref = queue.poll();
            while (ref != null) {
                HashEntry entry = entryLookup.get(ref);
                if (entry == null) {
                    ref = queue.poll();
                    continue;
                }
                WeakHashTable.this.remove(ref);
                entryLookup.remove(entry.key);
                entryLookup.remove(entry.value);
                ref = queue.poll();
            }
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
