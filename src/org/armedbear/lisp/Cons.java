/*
 * Cons.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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

public final class Cons extends LispObject implements java.io.Serializable
{
  public LispObject car;
  public LispObject cdr;

  public Cons(LispObject car, LispObject cdr)
  {
    this.car = car;
    this.cdr = cdr;
    ++count;
  }

  public Cons(LispObject car)
  {
    this.car = car;
    this.cdr = NIL;
    ++count;
  }

  public Cons(String name, LispObject value)
  {
    this.car = new SimpleString(name);
    this.cdr = value != null ? value : NULL_VALUE;
    ++count;
  }

  public Cons(Cons original) 
  {
    Cons rest = original;
    LispObject result = NIL;
    while (rest.car() != NIL) {
      result = result.push(rest.car());
      if (rest.cdr() == NIL) {
        break;
      }
      rest = (Cons) rest.cdr();
    }
    result = result.nreverse();
    this.car = result.car();
    this.cdr = result.cdr();
    ++count;
  }

  @Override
  public LispObject typeOf()
  {
    return Symbol.CONS;
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.CONS;
  }

  @Override
  public LispObject typep(LispObject typeSpecifier)
  {
    if (typeSpecifier instanceof Symbol)
      {
        if (typeSpecifier == Symbol.LIST)
          return T;
        if (typeSpecifier == Symbol.CONS)
          return T;
        if (typeSpecifier == Symbol.SEQUENCE)
          return T;
        if (typeSpecifier == T)
          return T;
      }
    else if (typeSpecifier instanceof LispClass)
      {
        if (typeSpecifier == BuiltInClass.LIST)
          return T;
        if (typeSpecifier == BuiltInClass.CONS)
          return T;
        if (typeSpecifier == BuiltInClass.SEQUENCE)
          return T;
        if (typeSpecifier == BuiltInClass.CLASS_T)
          return T;
      }
    return NIL;
  }

  @Override
  public final boolean constantp()
  {
    if (car == Symbol.QUOTE)
      {
        if (cdr instanceof Cons)
          if (((Cons)cdr).cdr == NIL)
            return true;
      }
    return false;
  }

  @Override
  public boolean atom()
  {
    return false;
  }

  @Override
  public LispObject RPLACA(LispObject obj)
  {
    car = obj;
    return this;
  }

  @Override
  public LispObject RPLACD(LispObject obj)
  {
    cdr = obj;
    return this;
  }

  @Override
  public final int sxhash()
  {
    return computeHash(this, 4);
  }

  private static final int computeHash(LispObject obj, int depth)
  {
    if (obj instanceof Cons)
      {
        if (depth > 0)
          {
            int n1 = computeHash(((Cons)obj).car, depth - 1);
            int n2 = computeHash(((Cons)obj).cdr, depth - 1);
            return n1 ^ n2;
          }
        else
          {
            // This number comes from SBCL, but since we're not really
            // using SBCL's SXHASH algorithm, it's probably not optimal.
            // But who knows?
            return 261835505;
          }
      }
    else
      return obj.sxhash();
  }

  @Override
  public final int psxhash()
  {
    return computeEqualpHash(this, 4);
  }

  private static final int computeEqualpHash(LispObject obj, int depth)
  {
    if (obj instanceof Cons)
      {
        if (depth > 0)
          {
            int n1 = computeEqualpHash(((Cons)obj).car, depth - 1);
            int n2 = computeEqualpHash(((Cons)obj).cdr, depth - 1);
            return n1 ^ n2;
          }
        else
          return 261835505; // See above.
      }
    else
      return obj.psxhash();
  }

  @Override
  public final boolean equal(LispObject obj)
  {
    if (this == obj)
      return true;
    if (obj instanceof Cons)
      {
        if (car.equal(((Cons)obj).car) && cdr.equal(((Cons)obj).cdr))
          return true;
      }
    return false;
  }

  @Override
  public final boolean equalp(LispObject obj)
  {
    if (this == obj)
      return true;
    if (obj instanceof Cons)
      {
        if (car.equalp(((Cons)obj).car) && cdr.equalp(((Cons)obj).cdr))
          return true;
      }
    return false;
  }

  @Override
  public final int length()
  {
    int length = 1;
    LispObject obj = cdr;
        while (obj != NIL)
          {
            ++length;
            if (obj instanceof Cons) {
                obj = ((Cons)obj).cdr;
            } else  type_error(obj, Symbol.LIST);
          }      
    return length;
  }

  @Override
  public LispObject NTH(int index)
  {
    if (index < 0)
      type_error(Fixnum.getInstance(index), Symbol.UNSIGNED_BYTE);
    int i = 0;
    LispObject obj = this;
    while (true)
      {
        if (i == index)
          return obj.car();
        obj = obj.cdr();
        if (obj == NIL)
          return NIL;
        ++i;
      }
  }

  @Override
  public LispObject elt(int index)
  {
    if (index < 0)
      type_error(Fixnum.getInstance(index), Symbol.UNSIGNED_BYTE);
    int i = 0;
    Cons cons = this;
    while (true)
      {
        if (i == index)
          return cons.car;
        LispObject conscdr = cons.cdr;
        if (conscdr instanceof Cons)
          {
            cons = (Cons) conscdr;
          }
        else
          {
            if (conscdr == NIL)
              {
                // Index too large.
                type_error(Fixnum.getInstance(index),
                                list(Symbol.INTEGER, Fixnum.ZERO,
                                      Fixnum.getInstance(length() - 1)));
              }
            else
              {
                // Dotted list.
                type_error(conscdr, Symbol.LIST);
              }
            // Not reached.
            return NIL;
          }
        ++i;
      }
  }

  @Override
  public LispObject reverse()
  {
    Cons cons = this;
    LispObject result = new Cons(cons.car);
    while (cons.cdr instanceof Cons)
      {
        cons = (Cons) cons.cdr;
        result = new Cons(cons.car, result);
      }
    if (cons.cdr != NIL)
      return type_error(cons.cdr, Symbol.LIST);
    return result;
  }

  @Override
  public final LispObject nreverse()
  {
    if (cdr instanceof Cons)
      {
        Cons cons = (Cons) cdr;
        if (cons.cdr instanceof Cons)
          {
            Cons cons1 = cons;
            LispObject list = NIL;
            do
              {
                Cons temp = (Cons) cons.cdr;
                cons.cdr = list;
                list = cons;
                cons = temp;
              }
            while (cons.cdr instanceof Cons);
            if (cons.cdr != NIL)
              return type_error(cons.cdr, Symbol.LIST);
            cdr = list;
            cons1.cdr = cons;
          }
        else if (cons.cdr != NIL)
          return type_error(cons.cdr, Symbol.LIST);
        LispObject temp = car;
        car = cons.car;
        cons.car = temp;
      }
    else if (cdr != NIL)
      return type_error(cdr, Symbol.LIST);
    return this;
  }

  @Override
  public final LispObject[] copyToArray()
  {
    final int length = length();
    LispObject[] array = new LispObject[length];
    LispObject rest = this;
    for (int i = 0; i < length; i++)
      {
        array[i] = rest.car();
        rest = rest.cdr();
      }
    return array;
  }

  @Override
  public LispObject execute()
  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute();
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject arg)
  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(arg);
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject first, LispObject second)

  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(first, second);
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third)

  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(first, second, third);
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)

  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(first, second, third, fourth);
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth)

  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(first, second, third, fourth, fifth);
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth)

  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(first, second, third, fourth, fifth, sixth);
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh)

  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(first, second, third, fourth, fifth, sixth,
                               seventh);
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh, LispObject eighth)

  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(first, second, third, fourth, fifth, sixth,
                               seventh, eighth);
      }
    return signalExecutionError();
  }

  @Override
  public LispObject execute(LispObject[] args)
  {
    if (car == Symbol.LAMBDA)
      {
        Closure closure = new Closure(this, new Environment());
        return closure.execute(args);
      }
    return signalExecutionError();
  }

  private final LispObject signalExecutionError()
  {
    return type_error(this, list(Symbol.OR, Symbol.FUNCTION,
                                       Symbol.SYMBOL));
  }

  @Override
  public String writeToString()
  {
    final LispThread thread = LispThread.currentThread();
    final LispObject printLength = Symbol.PRINT_LENGTH.symbolValue(thread);
    final int maxLength;
    if (printLength instanceof Fixnum)
      maxLength = ((Fixnum)printLength).value;
    else
      maxLength = Integer.MAX_VALUE;
    final LispObject printLevel = Symbol.PRINT_LEVEL.symbolValue(thread);
    final int maxLevel;
    if (printLevel instanceof Fixnum)
      maxLevel = ((Fixnum)printLevel).value;
    else
      maxLevel = Integer.MAX_VALUE;
    StringBuilder sb = new StringBuilder();
    if (car == Symbol.QUOTE)
      {
        if (cdr instanceof Cons)
          {
            // Not a dotted list.
            if (cdr.cdr() == NIL)
              {
                sb.append('\'');
                sb.append(cdr.car().writeToString());
                return sb.toString();
              }
          }
      }
    if (car == Symbol.FUNCTION)
      {
        if (cdr instanceof Cons)
          {
            // Not a dotted list.
            if (cdr.cdr() == NIL)
              {
                sb.append("#'");
                sb.append(cdr.car().writeToString());
                return sb.toString();
              }
          }
      }
    LispObject currentPrintLevel =
      _CURRENT_PRINT_LEVEL_.symbolValue(thread);
    int currentLevel = Fixnum.getValue(currentPrintLevel);
    if (currentLevel < maxLevel)
      {
        final SpecialBindingsMark mark = thread.markSpecialBindings();
        thread.bindSpecial(_CURRENT_PRINT_LEVEL_, currentPrintLevel.incr());
        try
          {
            int count = 0;
            boolean truncated = false;
            sb.append('(');
            if (count < maxLength)
              {
                LispObject p = this;
                sb.append(p.car().writeToString());
                ++count;
                while ((p = p.cdr()) instanceof Cons)
                  {
                    sb.append(' ');
                    if (count < maxLength)
                      {
                        sb.append(p.car().writeToString());
                        ++count;
                      }
                    else
                      {
                        truncated = true;
                        break;
                      }
                  }
                if (!truncated && p != NIL)
                  {
                    sb.append(" . ");
                    sb.append(p.writeToString());
                  }
              }
            else
              truncated = true;
            if (truncated)
              sb.append("...");
            sb.append(')');
          }
        finally
          {
            thread.resetSpecialBindings(mark);
          }
      }
    else
      sb.append('#');
    return sb.toString();
  }

  // Statistics for TIME.
  private static long count;

  /*package*/ static long getCount()
  {
    return count;
  }

  /*package*/ static void setCount(long n)
  {
    count = n;
  }
}
