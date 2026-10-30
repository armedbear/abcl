/*
 * Atomic.java
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

/**
 * Compare-and-swap on the places that EXT:COMPARE-AND-SWAP supports:
 * the CAR and CDR of a cons, the elements of a simple vector, the
 * slots of a structure or standard object, and the global value of a
 * symbol.
 *
 * Every operation returns the value it found in the place.  The swap
 * happened if and only if that value is EQ to the OLD argument.
 *
 * The comparison is EQL rather than EQ.  ABCL boxes fixnums outside
 * [0, 256) and other numbers, so two EQL fixnums need not be EQ; code
 * written for implementations with immediate fixnums expects
 * (COMPARE-AND-SWAP (CAR X) 1000 1001) to succeed when the car is
 * 1000.  When the place holds a value EQL to OLD but not EQ to it, the
 * swap is retried against that value, and on success the OLD argument
 * itself is returned so that the caller's EQ test succeeds.
 *
 * On Java 9 and later the swaps are VarHandle compareAndExchange
 * calls, in AtomicVarHandles, which is compiled only when the JDK
 * provides java.lang.invoke.VarHandle.  Without it (Java 8) the swaps
 * take one of a fixed set of striped locks; they are then atomic with
 * respect to each other, but a plain SETF of the same place may be
 * overwritten by a racing swap.
 */
public final class Atomic
{
  private Atomic() {}

  /** The machine underneath: each method swaps when the place holds
   *  EXPECTED (by identity) and returns the value it found. */
  static abstract class Strategy
  {
    abstract LispObject car(Cons cons, LispObject expected, LispObject newValue);
    abstract LispObject cdr(Cons cons, LispObject expected, LispObject newValue);
    abstract LispObject element(LispObject[] array, int index,
                                LispObject expected, LispObject newValue);
    abstract LispObject symbolValue(Symbol symbol, LispObject expected,
                                    LispObject newValue);
    abstract Symbol name();
  }

  static final Strategy STRATEGY = chooseStrategy();

  private static Strategy chooseStrategy()
  {
    try {
      Class<?> c = Class.forName("org.armedbear.lisp.AtomicVarHandles");
      return (Strategy) c.getDeclaredConstructor().newInstance();
    } catch (Throwable t) {
      // ClassNotFoundException on a Java 8 build, LinkageError when
      // a build made on Java 9 or later runs on Java 8.
      return new Locks();
    }
  }

  static final class Locks extends Strategy
  {
    private static final Object[] LOCKS = new Object[64];
    static {
      for (int i = 0; i < LOCKS.length; i++)
        LOCKS[i] = new Object();
    }

    private static Object lockFor(Object o)
    {
      return LOCKS[System.identityHashCode(o) & (LOCKS.length - 1)];
    }

    LispObject car(Cons cons, LispObject expected, LispObject newValue)
    {
      synchronized (lockFor(cons)) {
        LispObject witness = cons.car;
        if (witness == expected)
          cons.car = newValue;
        return witness;
      }
    }

    LispObject cdr(Cons cons, LispObject expected, LispObject newValue)
    {
      synchronized (lockFor(cons)) {
        LispObject witness = cons.cdr;
        if (witness == expected)
          cons.cdr = newValue;
        return witness;
      }
    }

    LispObject element(LispObject[] array, int index,
                       LispObject expected, LispObject newValue)
    {
      synchronized (lockFor(array)) {
        LispObject witness = array[index];
        if (witness == expected)
          array[index] = newValue;
        return witness;
      }
    }

    LispObject symbolValue(Symbol symbol, LispObject expected,
                           LispObject newValue)
    {
      synchronized (lockFor(symbol)) {
        LispObject witness = symbol.getSymbolValue();
        if (witness == expected)
          symbol.setSymbolValue(newValue);
        return witness;
      }
    }

    Symbol name()
    {
      return internKeyword("LOCK");
    }
  }

  // The EQL loops.  Each returns OLD itself after a successful swap.

  static LispObject casCar(Cons cons, LispObject old, LispObject newValue)
  {
    LispObject expected = old;
    for (;;) {
      LispObject witness = STRATEGY.car(cons, expected, newValue);
      if (witness == expected)
        return old;
      if (!witness.eql(old))
        return witness;
      expected = witness;
    }
  }

  static LispObject casCdr(Cons cons, LispObject old, LispObject newValue)
  {
    LispObject expected = old;
    for (;;) {
      LispObject witness = STRATEGY.cdr(cons, expected, newValue);
      if (witness == expected)
        return old;
      if (!witness.eql(old))
        return witness;
      expected = witness;
    }
  }

  static LispObject casElement(LispObject[] array, int index,
                               LispObject old, LispObject newValue)
  {
    LispObject expected = old;
    for (;;) {
      LispObject witness = STRATEGY.element(array, index, expected, newValue);
      if (witness == expected)
        return old;
      if (!witness.eql(old))
        return witness;
      expected = witness;
    }
  }

  /** Swaps the value of SYMBOL that SYMBOL-VALUE would read: the
   *  current thread's innermost dynamic binding if there is one (no
   *  other thread can see it, so a plain compare and store is
   *  atomic), otherwise the global value. */
  static LispObject casSymbolValue(Symbol symbol, LispObject old,
                                   LispObject newValue)
  {
    if (symbol.isConstant())
      return program_error("Can't change value of constant symbol "
                           + symbol.princToString() + ".");
    SpecialBinding binding =
      LispThread.currentThread().getSpecialBinding(symbol);
    LispObject witness;
    if (binding != null) {
      witness = binding.value;
      if (witness != null && witness.eql(old)) {
        binding.value = newValue;
        return old;
      }
    } else {
      LispObject expected = old;
      for (;;) {
        witness = STRATEGY.symbolValue(symbol, expected, newValue);
        if (witness == expected)
          return old;
        if (witness == null || !witness.eql(old))
          break;
        expected = witness;
      }
    }
    if (witness == null)
      return error(new UnboundVariable(symbol));
    return witness;
  }

  static LispObject casSlotValue(StandardObject instance, LispObject slotName,
                                 LispObject old, LispObject newValue)
  {
    Layout layout = instance.layout;
    if (layout.isInvalid())
      layout = instance.updateLayout();
    LispObject witness;
    final LispObject index = layout.slotTable.get(slotName);
    if (index != null) {
      witness = casElement(instance.slots, ((Fixnum)index).value,
                           old, newValue);
    } else {
      LispObject location = layout.getSharedSlotLocation(slotName);
      if (location == null)
        return Symbol.SLOT_MISSING.execute(instance.getLispClass(), instance,
                                           slotName, Symbol.SLOT_VALUE);
      witness = casCdr((Cons)location, old, newValue);
    }
    if (witness == UNBOUND_VALUE) {
      witness = Symbol.SLOT_UNBOUND.execute(instance.getLispClass(), instance,
                                            slotName);
      LispThread.currentThread()._values = null;
    }
    return witness;
  }

  // The entry points.  The compiler calls these directly (see
  // P2-COMPARE-AND-SWAP in compiler-pass2.lisp), and the primitives
  // below call them for the interpreter and FUNCALL.

  public static LispObject compareAndSwapCar(LispObject cons, LispObject old,
                                             LispObject newValue)
  {
    if (cons instanceof Cons)
      return casCar((Cons)cons, old, newValue);
    return type_error(cons, Symbol.CONS);
  }

  public static LispObject compareAndSwapCdr(LispObject cons, LispObject old,
                                             LispObject newValue)
  {
    if (cons instanceof Cons)
      return casCdr((Cons)cons, old, newValue);
    return type_error(cons, Symbol.CONS);
  }

  public static LispObject compareAndSwapSvref(LispObject vector,
                                               LispObject index,
                                               LispObject old,
                                               LispObject newValue)
  {
    if (vector instanceof SimpleVector) {
      final SimpleVector sv = (SimpleVector)vector;
      final int i = Fixnum.getValue(index);
      if (i < 0 || i >= sv.capacity) {
        sv.badIndex(i, sv.capacity);
        return NIL; // Not reached.
      }
      return casElement(sv.data, i, old, newValue);
    }
    return type_error(vector, Symbol.SIMPLE_VECTOR);
  }

  public static LispObject compareAndSwapStructureRef(LispObject instance,
                                                      LispObject index,
                                                      LispObject old,
                                                      LispObject newValue)
  {
    if (instance instanceof StructureObject) {
      final LispObject[] slots = ((StructureObject)instance).slots;
      final int i = Fixnum.getValue(index);
      if (i < 0 || i >= slots.length)
        // Shouldn't happen.
        return error(new LispError("Internal error."));
      return casElement(slots, i, old, newValue);
    }
    return type_error(instance, Symbol.STRUCTURE_OBJECT);
  }

  public static LispObject
    compareAndSwapStandardInstanceAccess(LispObject object, LispObject location,
                                         LispObject old, LispObject newValue)
  {
    final StandardObject instance = StandardObject.checkStandardObject(object);
    if (instance.layout.isInvalid())
      instance.updateLayout();
    if (!(location instanceof Fixnum))
      return type_error(location, Symbol.INTEGER);
    final int i = ((Fixnum)location).value;
    final LispObject[] slots = instance.slots;
    if (i < 0 || i >= slots.length) {
      if (slots.length > 0)
        return type_error(location,
                          list(Symbol.INTEGER, Fixnum.ZERO,
                               Fixnum.getInstance(slots.length - 1)));
      return program_error("The object " + instance.princToString()
                           + " has no slots.");
    }
    return casElement(slots, i, old, newValue);
  }

  public static LispObject compareAndSwapSlotValue(LispObject instance,
                                                   LispObject slotName,
                                                   LispObject old,
                                                   LispObject newValue)
  {
    return casSlotValue(StandardObject.checkStandardObject(instance),
                        slotName, old, newValue);
  }

  public static LispObject compareAndSwapSymbolValue(LispObject symbol,
                                                     LispObject old,
                                                     LispObject newValue)
  {
    return casSymbolValue(checkSymbol(symbol), old, newValue);
  }

  // ### %compare-and-swap-car cons old new => witness
  private static final Primitive _COMPARE_AND_SWAP_CAR =
    new Primitive("%compare-and-swap-car", PACKAGE_SYS, true,
                  "cons old new")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
      {
        return compareAndSwapCar(first, second, third);
      }
    };

  // ### %compare-and-swap-cdr cons old new => witness
  private static final Primitive _COMPARE_AND_SWAP_CDR =
    new Primitive("%compare-and-swap-cdr", PACKAGE_SYS, true,
                  "cons old new")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
      {
        return compareAndSwapCdr(first, second, third);
      }
    };

  // ### %compare-and-swap-svref simple-vector index old new => witness
  private static final Primitive _COMPARE_AND_SWAP_SVREF =
    new Primitive("%compare-and-swap-svref", PACKAGE_SYS, true,
                  "simple-vector index old new")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
      {
        return compareAndSwapSvref(first, second, third, fourth);
      }
    };

  // ### %compare-and-swap-structure-ref instance index old new => witness
  private static final Primitive _COMPARE_AND_SWAP_STRUCTURE_REF =
    new Primitive("%compare-and-swap-structure-ref", PACKAGE_SYS, true,
                  "instance index old new")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
      {
        return compareAndSwapStructureRef(first, second, third, fourth);
      }
    };

  // ### %compare-and-swap-standard-instance-access instance location old new => witness
  private static final Primitive _COMPARE_AND_SWAP_STANDARD_INSTANCE_ACCESS =
    new Primitive("%compare-and-swap-standard-instance-access", PACKAGE_SYS,
                  true, "instance location old new")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
      {
        return compareAndSwapStandardInstanceAccess(first, second, third,
                                                    fourth);
      }
    };

  // ### %compare-and-swap-slot-value instance slot-name old new => witness
  private static final Primitive _COMPARE_AND_SWAP_SLOT_VALUE =
    new Primitive("%compare-and-swap-slot-value", PACKAGE_SYS, true,
                  "instance slot-name old new")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third, LispObject fourth)
      {
        return compareAndSwapSlotValue(first, second, third, fourth);
      }
    };

  // ### %compare-and-swap-symbol-value symbol old new => witness
  private static final Primitive _COMPARE_AND_SWAP_SYMBOL_VALUE =
    new Primitive("%compare-and-swap-symbol-value", PACKAGE_SYS, true,
                  "symbol old new")
    {
      @Override
      public LispObject execute(LispObject first, LispObject second,
                                LispObject third)
      {
        return compareAndSwapSymbolValue(first, second, third);
      }
    };

  // ### compare-and-swap-implementation => :varhandle or :lock
  private static final Primitive COMPARE_AND_SWAP_IMPLEMENTATION =
    new Primitive("compare-and-swap-implementation", PACKAGE_EXT, true, "",
                  "Return :VARHANDLE when COMPARE-AND-SWAP is lock-free (Java 9 and later),\n"
                  + "or :LOCK when it falls back to striped locks (Java 8).")
    {
      @Override
      public LispObject execute()
      {
        return STRATEGY.name();
      }
    };
}
