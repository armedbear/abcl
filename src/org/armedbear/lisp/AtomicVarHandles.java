/*
 * AtomicVarHandles.java
 *
 * Copyright (C) 2026 David John Cooper
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

import static org.armedbear.lisp.Lisp.*;

import java.lang.invoke.MethodHandles;
import java.lang.invoke.VarHandle;

/**
 * The lock-free strategy for Atomic: VarHandle compareAndExchange on
 * the fields themselves, which need not be volatile.
 *
 * Needs Java 9.  build.xml compiles this file only when the JDK
 * provides java.lang.invoke.VarHandle, and Atomic loads it by name,
 * falling back to locks when it is absent or cannot be linked.
 */
final class AtomicVarHandles extends Atomic.Strategy
{
  private static final VarHandle CAR;
  private static final VarHandle CDR;
  private static final VarHandle ELEMENT;
  private static final VarHandle SYMBOL_VALUE;

  static {
    try {
      MethodHandles.Lookup lookup = MethodHandles.lookup();
      CAR = lookup.findVarHandle(Cons.class, "car", LispObject.class);
      CDR = lookup.findVarHandle(Cons.class, "cdr", LispObject.class);
      ELEMENT = MethodHandles.arrayElementVarHandle(LispObject[].class);
      SYMBOL_VALUE = MethodHandles.privateLookupIn(Symbol.class, lookup)
        .findVarHandle(Symbol.class, "value", LispObject.class);
    } catch (ReflectiveOperationException e) {
      throw new ExceptionInInitializerError(e);
    }
  }

  LispObject car(Cons cons, LispObject expected, LispObject newValue)
  {
    return (LispObject) CAR.compareAndExchange(cons, expected, newValue);
  }

  LispObject cdr(Cons cons, LispObject expected, LispObject newValue)
  {
    return (LispObject) CDR.compareAndExchange(cons, expected, newValue);
  }

  LispObject element(LispObject[] array, int index,
                     LispObject expected, LispObject newValue)
  {
    return (LispObject) ELEMENT.compareAndExchange(array, index,
                                                   expected, newValue);
  }

  LispObject symbolValue(Symbol symbol, LispObject expected,
                         LispObject newValue)
  {
    return (LispObject) SYMBOL_VALUE.compareAndExchange(symbol, expected,
                                                        newValue);
  }

  Symbol name()
  {
    return internKeyword("VARHANDLE");
  }
}
