/*
 * LispObject.java
 *
 * Copyright (C) 2002-2007 Peter Graves
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

import static org.armedbear.lisp.Lisp.*;
import java.util.WeakHashMap;

public class LispObject //extends Lisp
{

  /** Function to allow objects to return the value
   * "they stand for". Used by AutoloadedFunctionProxy to return
   * the function it is proxying.
   */
  public LispObject resolve()
  {
    return this;
  }

  public LispObject typeOf()
  {
    return T;
  }

  static public LispObject getInstance(boolean b) {
      return b ? T : NIL;
  }

  public LispObject classOf()
  {
    return BuiltInClass.CLASS_T;
  }

  public LispObject getDescription()
  {
    StringBuilder sb = new StringBuilder("An object of type ");
    sb.append(typeOf().writeToString());
    sb.append(" at #x");
    sb.append(Integer.toHexString(System.identityHashCode(this)).toUpperCase());
    return new SimpleString(sb);
  }

  /** 
   *  Implementing the getParts() protocol will allow INSPECT to
   *  return information about the substructure of a descendent of
   *  LispObject.
   *  
   *  The protocol is to return a List of Cons pairs, where the car of
   *  each pair contains a decriptive string, and the cdr returns a
   *  subobject for inspection.
   */
  public LispObject getParts()
  {
    return NIL;
  }

  public boolean getBooleanValue()
  {
    return true;
  }

  public LispObject typep(LispObject typeSpecifier)
  {
    if (typeSpecifier == T)
      return T;
    if (typeSpecifier == BuiltInClass.CLASS_T)
      return T;
    if (typeSpecifier == Symbol.ATOM)
      return T;
    return NIL;
  }

  public boolean constantp()
  {
    return true;
  }

  public final LispObject CONSTANTP()
  {
    return constantp() ? T : NIL;
  }

  public final LispObject ATOM()
  {
    return atom() ? T : NIL;
  }

  public boolean atom()
  {
    return true;
  }

  public Object javaInstance()
  {
        return this;
  }

  public Object javaInstance(Class<?> c)
  {
      if (c.isAssignableFrom(getClass()))
	  return this;
      return error(new LispError("The value " + writeToString() +
				 " is not of class " + c.getName()));
  }

  /** This method returns 'this' by default, but allows
   * objects to return different values to increase Java
   * interoperability
   * 
   * @return An object to be used with synchronized, wait, notify, etc
   */
  public Object lockableInstance()
  {
      return this;
  }


  public final LispObject car()
  {
    if (this instanceof Cons) {
      return ((Cons)this).car;
    } else if (this instanceof Nil) {
      return NIL;
    }
    return type_error(this, Symbol.LIST);
  }

  public final void setCar(LispObject obj)
  {
      if (this instanceof Cons) {
          ((Cons)this).car = obj;
          return;
      }
    type_error(this, Symbol.CONS);
  }

  public LispObject RPLACA(LispObject obj)
  {
    return type_error(this, Symbol.CONS);
  }

  public final LispObject cdr()
  {
    if (this instanceof Cons) {
      return ((Cons)this).cdr;
    } else if (this instanceof Nil) {
      return NIL;
    }
    return type_error(this, Symbol.LIST);
  }

  public final void setCdr(LispObject obj)
  {
      if (this instanceof Cons) {
          ((Cons)this).cdr = obj;
          return;
      }

    type_error(this, Symbol.CONS);
  }

  public LispObject RPLACD(LispObject obj)
  {
    return type_error(this, Symbol.CONS);
  }

  public final LispObject cadr()
  {
    LispObject tail = cdr();
    if (!(tail instanceof Nil)) {
        return tail.car();
    } else 
        return NIL;
  }

  public final LispObject cddr()
  {
    LispObject tail = cdr();
    if (!(tail instanceof Nil)) {
        return tail.cdr();
    } else 
        return NIL;
  }

  public final LispObject caddr()
  {
    LispObject tail = cddr();
    if (!(tail instanceof Nil)) {
        return tail.car();
    } else 
        return NIL;
  }

  public final LispObject nthcdr(int n)
  {
    if (n < 0)
      return type_error(Fixnum.getInstance(n),
                             list(Symbol.INTEGER, Fixnum.ZERO));
    if (this instanceof Cons) {
      LispObject result = this;
      for (int i = n; i-- > 0;) {
          result = result.cdr();
          if (result == NIL)
              break;
      }
      return result;
    } else if (this instanceof Nil) {
      return NIL;
    }
    return type_error(this, Symbol.LIST);
  }

  public final LispObject push(LispObject obj)
  {
    if (this instanceof Cons) {
      return new Cons(obj, this);
    } else if (this instanceof Nil) {
      return new Cons(obj);
    }
    return type_error(this, Symbol.LIST);
  }

  final public LispObject EQ(LispObject obj)
  {
    return this == obj ? T : NIL;
  }

  public boolean eql(char c)
  {
    return false;
  }

  public boolean eql(int n)
  {
    return false;
  }

  public boolean eql(LispObject obj)
  {
    return this == obj;
  }

  public final LispObject EQL(LispObject obj)
  {
    return eql(obj) ? T : NIL;
  }

  public final LispObject EQUAL(LispObject obj)
  {
    return equal(obj) ? T : NIL;
  }

  public boolean equal(int n)
  {
    return false;
  }

  public boolean equal(LispObject obj)
  {
    return this == obj;
  }

  public boolean equalp(int n)
  {
    return false;
  }

  public boolean equalp(LispObject obj)
  {
    return this == obj;
  }

  public LispObject ABS()
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject NUMERATOR()
  {
    return type_error(this, Symbol.RATIONAL);
  }

  public LispObject DENOMINATOR()
  {
    return type_error(this, Symbol.RATIONAL);
  }

  public final LispObject EVENP()
  {
    return evenp() ? T : NIL;
  }

  public boolean evenp()
  {
    type_error(this, Symbol.INTEGER);
    // Not reached.
    return false;
  }

  public final LispObject ODDP()
  {
    return oddp() ? T : NIL;
  }

  public boolean oddp()
  {
    type_error(this, Symbol.INTEGER);
    // Not reached.
    return false;
  }

  public final LispObject PLUSP()
  {
    return plusp() ? T : NIL;
  }

  public boolean plusp()
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public final LispObject MINUSP()
  {
    return minusp() ? T : NIL;
  }

  public boolean minusp()
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public final LispObject NUMBERP()
  {
    return numberp() ? T : NIL;
  }

  public boolean numberp()
  {
    return false;
  }

  public final LispObject ZEROP()
  {
    return zerop() ? T : NIL;
  }

  public boolean zerop()
  {
    type_error(this, Symbol.NUMBER);
    // Not reached.
    return false;
  }

  public LispObject COMPLEXP()
  {
    return NIL;
  }

  public final LispObject FLOATP()
  {
    return floatp() ? T : NIL;
  }

  public boolean floatp()
  {
    return false;
  }

  public final LispObject INTEGERP()
  {
    return integerp() ? T : NIL;
  }

  public boolean integerp()
  {
    return false;
  }

  public final LispObject RATIONALP()
  {
    return rationalp() ? T : NIL;
  }

  public boolean rationalp()
  {
    return false;
  }

  public final LispObject REALP()
  {
    return realp() ? T : NIL;
  }

  public boolean realp()
  {
    return false;
  }

  public final LispObject STRINGP()
  {
    return stringp() ? T : NIL;
  }

  public boolean stringp()
  {
    return false;
  }

  public LispObject SIMPLE_STRING_P()
  {
    return NIL;
  }

  public final LispObject VECTORP()
  {
    return vectorp() ? T : NIL;
  }

  public boolean vectorp()
  {
    return false;
  }

  public final LispObject CHARACTERP()
  {
    return characterp() ? T : NIL;
  }

  public boolean characterp()
  {
    return false;
  }

  public int length()
  {
    type_error(this, Symbol.SEQUENCE);
    // Not reached.
    return 0;
  }

  public final LispObject LENGTH()
  {
    return Fixnum.getInstance(length());
  }

  public LispObject CHAR(int index)
  {
    return type_error(this, Symbol.STRING);
  }

  public LispObject SCHAR(int index)
  {
    return type_error(this, Symbol.SIMPLE_STRING);
  }

  public LispObject NTH(int index)
  {
    return type_error(this, Symbol.LIST);
  }

  public final LispObject NTH(LispObject arg)
  {
    return NTH(Fixnum.getValue(arg));
  }

  public LispObject elt(int index)
  {
    return type_error(this, Symbol.SEQUENCE);
  }

  public LispObject reverse()
  {
    return type_error(this, Symbol.SEQUENCE);
  }

  public LispObject nreverse()
  {
    return type_error(this, Symbol.SEQUENCE);
  }

  public long aref_long(int index)
  {
    return AREF(index).longValue();
  }

  public int aref(int index)
  {
    return AREF(index).intValue();
  }

  public LispObject AREF(int index)
  {
    return type_error(this, Symbol.ARRAY);
  }

  public final LispObject AREF(LispObject index)
  {
      return AREF(Fixnum.getValue(index));
  }

  public void aset(int index, int n)

  {    
          aset(index, Fixnum.getInstance(n));
  }

  public void aset(int index, LispObject newValue)

  {
    type_error(this, Symbol.ARRAY);
  }

  public final void aset(LispObject index, LispObject newValue)

  {
      aset(Fixnum.getValue(index), newValue);
  }

  public LispObject SVREF(int index)
  {
    return type_error(this, Symbol.SIMPLE_VECTOR);
  }

  public void svset(int index, LispObject newValue)
  {
    type_error(this, Symbol.SIMPLE_VECTOR);
  }

  public void vectorPushExtend(LispObject element)

  {
    noFillPointer();
  }

  public LispObject VECTOR_PUSH_EXTEND(LispObject element)

  {
    return noFillPointer();
  }

  public LispObject VECTOR_PUSH_EXTEND(LispObject element, LispObject extension)

  {
    return noFillPointer();
  }

  public final LispObject noFillPointer()
  {
    return type_error(this, list(Symbol.AND, Symbol.VECTOR,
                                       list(Symbol.SATISFIES,
                                             Symbol.ARRAY_HAS_FILL_POINTER_P)));
  }

  public LispObject[] copyToArray()
  {
    type_error(this, Symbol.LIST);
    // Not reached.
    return null;
  }

  public final LispObject SYMBOLP()
  {
    return (this instanceof Symbol) ? T : NIL;
  }

  public final boolean listp()
  {
    return (this instanceof Cons) || (this instanceof Nil);
  }

  public final LispObject LISTP()
  {
    return listp() ? T : NIL;
  }

  public final boolean endp()
  {
    if (this instanceof Cons)
        return false;
    else if (this instanceof Nil)
        return true;
    type_error(this, Symbol.LIST);
    // Not reached.
    return false;
  }

  public final LispObject ENDP()
  {
    return endp() ? T : NIL;
  }

  public LispObject NOT()
  {
    return NIL;
  }

  public boolean isSpecialOperator()
  {
    type_error(this, Symbol.SYMBOL);
    // Not reached.
    return false;
  }

  public boolean isSpecialVariable()
  {
    return false;
  }

  private static final WeakHashMap<LispObject, LispObject>
      documentationHashTable = new WeakHashMap<LispObject, LispObject>();

  public LispObject getDocumentation(LispObject docType)

  {
    LispObject alist;
    synchronized (documentationHashTable) {
      alist = documentationHashTable.get(this);
    }
    if (alist != null)
      {
        LispObject entry = assq(docType, alist);
        if (entry instanceof Cons)
          return ((Cons)entry).cdr;
      }
    if(docType == Symbol.FUNCTION && this instanceof Symbol) {
        Object fn = ((Symbol)this).getSymbolFunction();
        if(fn instanceof Function) {
            DocString ds = fn.getClass().getAnnotation(DocString.class);
            if(ds != null) {
                String arglist = ds.args();
                String docstring = ds.doc();
                if(arglist.length() != 0)
                    ((Function)fn).setLambdaList(new SimpleString(arglist));
                if(docstring.length() != 0) {
                    SimpleString doc = new SimpleString(docstring);
                    ((Symbol)this).setDocumentation(Symbol.FUNCTION, doc);
                    return doc;
                }
            }
        }
    }
    return NIL;
  }

  public void setDocumentation(LispObject docType, LispObject documentation)

  {
    synchronized (documentationHashTable) {
      LispObject alist = documentationHashTable.get(this);
      if (alist == null)
        alist = NIL;
      LispObject entry = assq(docType, alist);
      if (entry instanceof Cons)
        {
          ((Cons)entry).cdr = documentation;
        }
      else
        {
          alist = alist.push(new Cons(docType, documentation));
          documentationHashTable.put(this, alist);
        }
    }
  }

  public LispObject getPropertyList()
  {
    return null;
  }

  public void setPropertyList(LispObject obj)
  {
  }

  public LispObject getSymbolValue()
  {
    return type_error(this, Symbol.SYMBOL);
  }

  public LispObject getSymbolFunction()
  {
    return type_error(this, Symbol.SYMBOL);
  }

  public LispObject getSymbolFunctionOrDie()
  {
    return type_error(this, Symbol.SYMBOL);
  }

  public LispObject getSymbolSetfFunction()
  {
    return type_error(this, Symbol.SYMBOL);
  }

  public LispObject getSymbolSetfFunctionOrDie()
  {
    return type_error(this, Symbol.SYMBOL);
  }

  public String writeToString()
  {
    return toString();
  }

  public final String unreadableString(String s) {
     return unreadableString(s, true);
  }
  public final String unreadableString(Symbol sym) {
     return unreadableString(sym, true);
  }

  public final String unreadableString(String s, boolean identity)
  {
    StringBuilder sb = new StringBuilder("#<");
    sb.append(s);
    if (identity) {
      sb.append(" {");
      sb.append(Integer.toHexString(System.identityHashCode(this)).toUpperCase());
      sb.append("}");
    }
    sb.append(">");
    return sb.toString();
  }

  public final String unreadableString(Symbol symbol, boolean identity) 

  {
    return unreadableString(symbol.writeToString(), identity);
  }

  // Special operator
  public LispObject execute(LispObject args, Environment env)

  {
    return error(new LispError());
  }

  public LispObject execute()
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject arg)
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second)

  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third)

  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)

  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth)

  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth)

  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh)

  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh, LispObject eighth)

  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject[] args)
  {
    return type_error(this, Symbol.FUNCTION);
  }

  // Used by COMPILE-MULTIPLE-VALUE-CALL.
  public LispObject dispatch(LispObject[] args)
  {
    switch (args.length)
      {
      case 0:
        return execute();
      case 1:
        return execute(args[0]);
      case 2:
        return execute(args[0], args[1]);
      case 3:
        return execute(args[0], args[1], args[2]);
      case 4:
        return execute(args[0], args[1], args[2], args[3]);
      case 5:
        return execute(args[0], args[1], args[2], args[3], args[4]);
      case 6:
        return execute(args[0], args[1], args[2], args[3], args[4],
                       args[5]);
      case 7:
        return execute(args[0], args[1], args[2], args[3], args[4],
                       args[5], args[6]);
      case 8:
        return execute(args[0], args[1], args[2], args[3], args[4],
                       args[5], args[6], args[7]);
      default:
        return execute(args);
      }
  }

  public int intValue()
  {
    type_error(this, Symbol.INTEGER);
    // Not reached.
    return 0;
  }

  public long longValue()
  {
    type_error(this, Symbol.INTEGER);
    // Not reached.
    return 0;
  }

  public float floatValue()
  {
    type_error(this, Symbol.SINGLE_FLOAT);
    // Not reached
    return 0;
  }

  public double doubleValue()
  {
    type_error(this, Symbol.DOUBLE_FLOAT);
    // Not reached
    return 0;
  }

  public LispObject incr()
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject decr()
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject negate()
  {
    return Fixnum.ZERO.subtract(this);
  }

  public LispObject add(int n)
  {
    return add(Fixnum.getInstance(n));
  }

  public LispObject add(LispObject obj)
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject subtract(int n)
  {
    return subtract(Fixnum.getInstance(n));
  }

  public LispObject subtract(LispObject obj)
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject multiplyBy(int n)
  {
    return multiplyBy(Fixnum.getInstance(n));
  }

  public LispObject multiplyBy(LispObject obj)
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject divideBy(LispObject obj)
  {
    return type_error(this, Symbol.NUMBER);
  }

  public boolean isEqualTo(int n)
  {
    return isEqualTo(Fixnum.getInstance(n));
  }

  public boolean isEqualTo(LispObject obj)
  {
    type_error(this, Symbol.NUMBER);
    // Not reached.
    return false;
  }

  public final LispObject IS_E(LispObject obj)
  {
    return isEqualTo(obj) ? T : NIL;
  }

  public boolean isNotEqualTo(int n)
  {
    return isNotEqualTo(Fixnum.getInstance(n));
  }

  public boolean isNotEqualTo(LispObject obj)
  {
    type_error(this, Symbol.NUMBER);
    // Not reached.
    return false;
  }

  public final LispObject IS_NE(LispObject obj)
  {
    return isNotEqualTo(obj) ? T : NIL;
  }

  public boolean isLessThan(int n)
  {
    return isLessThan(Fixnum.getInstance(n));
  }

  public boolean isLessThan(LispObject obj)
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public final LispObject IS_LT(LispObject obj)
  {
    return isLessThan(obj) ? T : NIL;
  }

  public boolean isGreaterThan(int n)
  {
    return isGreaterThan(Fixnum.getInstance(n));
  }

  public boolean isGreaterThan(LispObject obj)
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public final LispObject IS_GT(LispObject obj)
  {
    return isGreaterThan(obj) ? T : NIL;
  }

  public boolean isLessThanOrEqualTo(int n)
  {
    return isLessThanOrEqualTo(Fixnum.getInstance(n));
  }

  public boolean isLessThanOrEqualTo(LispObject obj)
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public final LispObject IS_LE(LispObject obj)
  {
    return isLessThanOrEqualTo(obj) ? T : NIL;
  }

  public boolean isGreaterThanOrEqualTo(int n)
  {
    return isGreaterThanOrEqualTo(Fixnum.getInstance(n));
  }

  public boolean isGreaterThanOrEqualTo(LispObject obj)
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public final LispObject IS_GE(LispObject obj)
  {
    return isGreaterThanOrEqualTo(obj) ? T : NIL;
  }

  public LispObject truncate(LispObject obj)
  {
    return type_error(this, Symbol.REAL);
  }

  public LispObject MOD(LispObject divisor)
  {
    truncate(divisor);
    final LispThread thread = LispThread.currentThread();
    LispObject remainder = thread._values[1];
    thread.clearValues();
    if (!remainder.zerop())
      {
        if (divisor.minusp())
          {
            if (plusp())
              return remainder.add(divisor);
          }
        else
          {
            if (minusp())
              return remainder.add(divisor);
          }
      }
    return remainder;
  }

  public LispObject MOD(int divisor)
  {
    return MOD(Fixnum.getInstance(divisor));
  }

  public LispObject ash(int shift)
  {
    return ash(Fixnum.getInstance(shift));
  }

  public LispObject ash(LispObject obj)
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LOGNOT()
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LOGAND(int n)
  {
    return LOGAND(Fixnum.getInstance(n));
  }

  public LispObject LOGAND(LispObject obj)
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LOGIOR(int n)
  {
    return LOGIOR(Fixnum.getInstance(n));
  }

  public LispObject LOGIOR(LispObject obj)
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LOGXOR(int n)
  {
    return LOGXOR(Fixnum.getInstance(n));
  }

  public LispObject LOGXOR(LispObject obj)
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LDB(int size, int position)
  {
    return type_error(this, Symbol.INTEGER);
  }

  public int sxhash()
  {
    return hashCode() & 0x7fffffff;
  }

  // For EQUALP hash tables.
  public int psxhash()
  {
    return sxhash();
  }

  public int psxhash(int depth)
  {
    return psxhash();
  }

  public LispObject STRING()
  {
    return error(new TypeError(writeToString() + " cannot be coerced to a string."));
  }

  public char[] chars()
  {
    type_error(this, Symbol.STRING);
    // Not reached.
    return null;
  }

  public char[] getStringChars()
  {
    type_error(this, Symbol.STRING);
    // Not reached.
    return null;
  }

  /** Returns a string representing the value
   * of a 'string designator', if the instance is one.
   *
   * Throws an error if the instance isn't a string designator.
   */
  public String getStringValue()
  {
    type_error(this, Symbol.STRING);
    // Not reached.
    return null;
  }

  public LispObject getSlotValue_0()
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject getSlotValue_1()
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject getSlotValue_2()
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject getSlotValue_3()
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject getSlotValue(int index)
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public int getFixnumSlotValue(int index)
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
    // Not reached.
    return 0;
  }

  public boolean getSlotValueAsBoolean(int index)
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
    // Not reached.
    return false;
  }

  public void setSlotValue_0(LispObject value)

  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public void setSlotValue_1(LispObject value)

  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public void setSlotValue_2(LispObject value)

  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public void setSlotValue_3(LispObject value)

  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public void setSlotValue(int index, LispObject value)

  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject SLOT_VALUE(LispObject slotName)
  {
    return type_error(this, Symbol.STANDARD_OBJECT);
  }

  public void setSlotValue(LispObject slotName, LispObject newValue)

  {
    type_error(this, Symbol.STANDARD_OBJECT);
  }

  // Profiling.
  public int getCallCount()
  {
    return 0;
  }

  public void setCallCount(int n)
  {
  }

  public void incrementCallCount()
  {
  }

  public int getHotCount()
  {
      return 0;
  }

  public void setHotCount(int n)
  {
  }

  public void incrementHotCount()
  {
  }

  private Cons finalizers = null;

  synchronized public void addFinalizer(LispObject fun) {
      finalizers = new Cons(fun, finalizers);
  }

  synchronized public void cancelFinalizers() {
      finalizers = null;
  }

  @Override
  @SuppressWarnings("FinalizeDeclaration")
  protected void finalize()
    throws Throwable {
      while (finalizers != null) {
          finalizers.car.execute();
          finalizers = (Cons)finalizers.cdr;
      }
      super.finalize();
  }
}
