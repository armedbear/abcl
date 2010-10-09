/*
 * EqlHashTable.java
 *
 * Copyright (C) 2004-2006 Peter Graves
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

public final class EqlHashTable extends HashTable
{
  private int mask;

  public EqlHashTable(int size, LispObject rehashSize,
                      LispObject rehashThreshold)
  {
    super(calculateInitialCapacity(size), rehashSize, rehashThreshold);
    mask = buckets.length - 1;
  }

  @Override
  public Symbol getTest()
  {
    return Symbol.EQL;
  }

  @Override
  public synchronized LispObject get(LispObject key)
  {
    HashEntry e = buckets[key.sxhash() & mask];
    while (e != null)
      {
        if (key.eql(e.key))
          return e.value;
        e = e.next;
      }
    return null;
  }

  @Override
  public synchronized void put(LispObject key, LispObject value)
  {
    int index = key.sxhash() & mask;
    HashEntry e = buckets[index];
    while (e != null)
      {
        if (key.eql(e.key))
          {
            e.value = value;
            return;
          }
        e = e.next;
      }
    // Not found. We need to add a new entry.
    if (++count > threshold)
      {
        rehash();
        // Need a new hash value to suit the bigger table.
        index = key.sxhash() & mask;
      }
    e = new HashEntry(key, value);
    e.next = buckets[index];
    buckets[index] = e;
  }

  @Override
  public LispObject remove(LispObject key)
  {
    final int index = key.sxhash() & mask;
    HashEntry e = buckets[index];
    HashEntry last = null;
    while (e != null)
      {
        if (key.eql(e.key))
          {
            if (last == null)
              buckets[index] = e.next;
            else
              last.next = e.next;
            --count;
            return e.value;
          }
        last = e;
        e = e.next;
      }
    return null;
  }

  @Override
  protected void rehash()
  {
    HashEntry[] oldBuckets = buckets;
    int newCapacity = buckets.length * 2;
    threshold = (int) (newCapacity * loadFactor);
    buckets = new HashEntry[newCapacity];
    mask = buckets.length - 1;
    for (int i = oldBuckets.length; i-- > 0;)
      {
        HashEntry e = oldBuckets[i];
        while (e != null)
          {
            final int index = e.key.sxhash() & mask;
            HashEntry dest = buckets[index];
            if (dest != null)
              {
                while (dest.next != null)
                  dest = dest.next;
                dest.next = e;
              }
            else
              buckets[index] = e;
            HashEntry next = e.next;
            e.next = null;
            e = next;
          }
      }
  }
}
