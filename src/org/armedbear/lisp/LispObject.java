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

public class LispObject extends Lisp
{
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

  public LispObject getDescription() throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer("An object of type ");
    sb.append(typeOf().writeToString());
    sb.append(" at #x");
    sb.append(Integer.toHexString(System.identityHashCode(this)).toUpperCase());
    return new SimpleString(sb);
  }

  public LispObject getParts() throws ConditionThrowable
  {
    return NIL;
  }

  public boolean getBooleanValue()
  {
    return true;
  }

  public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
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

  public LispObject CONSTANTP()
  {
    return constantp() ? T : NIL;
  }

  public LispObject ATOM()
  {
    return T;
  }

  public boolean atom()
  {
    return true;
  }

  public Object javaInstance() throws ConditionThrowable
  {
        return this;
  }

  public Object javaInstance(Class<?> c) throws ConditionThrowable
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
   * @throws org.armedbear.lisp.ConditionThrowable
   */
  public Object lockableInstance() throws ConditionThrowable
  {
      return this;
  }


  public LispObject car() throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public void setCar(LispObject obj) throws ConditionThrowable
  {
    type_error(this, Symbol.CONS);
  }

  public LispObject RPLACA(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.CONS);
  }

  public LispObject cdr() throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public void setCdr(LispObject obj) throws ConditionThrowable
  {
    type_error(this, Symbol.CONS);
  }

  public LispObject RPLACD(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.CONS);
  }

  public LispObject cadr() throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public LispObject cddr() throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public LispObject caddr() throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public LispObject nthcdr(int n) throws ConditionThrowable
  {
    if (n < 0)
      return type_error(Fixnum.getInstance(n),
                             list(Symbol.INTEGER, Fixnum.ZERO));
    return type_error(this, Symbol.LIST);
  }

  public LispObject push(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public LispObject EQ(LispObject obj)
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

  public final LispObject EQUAL(LispObject obj) throws ConditionThrowable
  {
    return equal(obj) ? T : NIL;
  }

  public boolean equal(int n)
  {
    return false;
  }

  public boolean equal(LispObject obj) throws ConditionThrowable
  {
    return this == obj;
  }

  public boolean equalp(int n)
  {
    return false;
  }

  public boolean equalp(LispObject obj) throws ConditionThrowable
  {
    return this == obj;
  }

  public LispObject ABS() throws ConditionThrowable
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject NUMERATOR() throws ConditionThrowable
  {
    return type_error(this, Symbol.RATIONAL);
  }

  public LispObject DENOMINATOR() throws ConditionThrowable
  {
    return type_error(this, Symbol.RATIONAL);
  }

  public LispObject EVENP() throws ConditionThrowable
  {
    return evenp() ? T : NIL;
  }

  public boolean evenp() throws ConditionThrowable
  {
    type_error(this, Symbol.INTEGER);
    // Not reached.
    return false;
  }

  public LispObject ODDP() throws ConditionThrowable
  {
    return oddp() ? T : NIL;
  }

  public boolean oddp() throws ConditionThrowable
  {
    type_error(this, Symbol.INTEGER);
    // Not reached.
    return false;
  }

  public LispObject PLUSP() throws ConditionThrowable
  {
    return plusp() ? T : NIL;
  }

  public boolean plusp() throws ConditionThrowable
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public LispObject MINUSP() throws ConditionThrowable
  {
    return minusp() ? T : NIL;
  }

  public boolean minusp() throws ConditionThrowable
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public LispObject NUMBERP()
  {
    return NIL;
  }

  public boolean numberp()
  {
    return false;
  }

  public LispObject ZEROP() throws ConditionThrowable
  {
    return zerop() ? T : NIL;
  }

  public boolean zerop() throws ConditionThrowable
  {
    type_error(this, Symbol.NUMBER);
    // Not reached.
    return false;
  }

  public LispObject COMPLEXP()
  {
    return NIL;
  }

  public LispObject FLOATP()
  {
    return NIL;
  }

  public boolean floatp()
  {
    return false;
  }

  public LispObject INTEGERP()
  {
    return NIL;
  }

  public boolean integerp()
  {
    return false;
  }

  public LispObject RATIONALP()
  {
    return rationalp() ? T : NIL;
  }

  public boolean rationalp()
  {
    return false;
  }

  public LispObject REALP()
  {
    return realp() ? T : NIL;
  }

  public boolean realp()
  {
    return false;
  }

  public LispObject STRINGP()
  {
    return NIL;
  }

  public boolean stringp()
  {
    return false;
  }

  public LispObject SIMPLE_STRING_P()
  {
    return NIL;
  }

  public LispObject VECTORP()
  {
    return NIL;
  }

  public boolean vectorp()
  {
    return false;
  }

  public LispObject CHARACTERP()
  {
    return NIL;
  }

  public boolean characterp()
  {
    return false;
  }

  public int length() throws ConditionThrowable
  {
    type_error(this, Symbol.SEQUENCE);
    // Not reached.
    return 0;
  }

  public final LispObject LENGTH() throws ConditionThrowable
  {
    return Fixnum.getInstance(length());
  }

  public LispObject CHAR(int index) throws ConditionThrowable
  {
    return type_error(this, Symbol.STRING);
  }

  public LispObject SCHAR(int index) throws ConditionThrowable
  {
    return type_error(this, Symbol.SIMPLE_STRING);
  }

  public LispObject NTH(int index) throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public LispObject NTH(LispObject arg) throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public LispObject elt(int index) throws ConditionThrowable
  {
    return type_error(this, Symbol.SEQUENCE);
  }

  public LispObject reverse() throws ConditionThrowable
  {
    return type_error(this, Symbol.SEQUENCE);
  }

  public LispObject nreverse() throws ConditionThrowable
  {
    return type_error(this, Symbol.SEQUENCE);
  }

  public long aref_long(int index) throws ConditionThrowable
  {
    return AREF(index).longValue();
  }

  public int aref(int index) throws ConditionThrowable
  {
    return AREF(index).intValue();
  }

  public LispObject AREF(int index) throws ConditionThrowable
  {
    return type_error(this, Symbol.ARRAY);
  }

  public LispObject AREF(LispObject index) throws ConditionThrowable
  {
      return AREF(Fixnum.getValue(index));
  }

  public void aset(int index, int n)
    throws ConditionThrowable
  {    
          aset(index, Fixnum.getInstance(n));
  }

  public void aset(int index, LispObject newValue)
    throws ConditionThrowable
  {
    type_error(this, Symbol.ARRAY);
  }

  public void aset(LispObject index, LispObject newValue)
    throws ConditionThrowable
  {
      aset(Fixnum.getValue(index), newValue);
  }

  public LispObject SVREF(int index) throws ConditionThrowable
  {
    return type_error(this, Symbol.SIMPLE_VECTOR);
  }

  public void svset(int index, LispObject newValue) throws ConditionThrowable
  {
    type_error(this, Symbol.SIMPLE_VECTOR);
  }

  public void vectorPushExtend(LispObject element)
    throws ConditionThrowable
  {
    noFillPointer();
  }

  public LispObject VECTOR_PUSH_EXTEND(LispObject element)
    throws ConditionThrowable
  {
    return noFillPointer();
  }

  public LispObject VECTOR_PUSH_EXTEND(LispObject element, LispObject extension)
    throws ConditionThrowable
  {
    return noFillPointer();
  }

  public final LispObject noFillPointer() throws ConditionThrowable
  {
    return type_error(this, list(Symbol.AND, Symbol.VECTOR,
                                       list(Symbol.SATISFIES,
                                             Symbol.ARRAY_HAS_FILL_POINTER_P)));
  }

  public LispObject[] copyToArray() throws ConditionThrowable
  {
    type_error(this, Symbol.LIST);
    // Not reached.
    return null;
  }

  public LispObject SYMBOLP()
  {
    return NIL;
  }

  public boolean listp()
  {
    return false;
  }

  public LispObject LISTP()
  {
    return NIL;
  }

  public boolean endp() throws ConditionThrowable
  {
    type_error(this, Symbol.LIST);
    // Not reached.
    return false;
  }

  public LispObject ENDP() throws ConditionThrowable
  {
    return type_error(this, Symbol.LIST);
  }

  public LispObject NOT()
  {
    return NIL;
  }

  public boolean isSpecialOperator() throws ConditionThrowable
  {
    type_error(this, Symbol.SYMBOL);
    // Not reached.
    return false;
  }

  public boolean isSpecialVariable()
  {
    return false;
  }

  private static final EqHashTable documentationHashTable =
    new EqHashTable(11, NIL, NIL);

  public LispObject getDocumentation(LispObject docType)
    throws ConditionThrowable
  {
    LispObject alist = documentationHashTable.get(this);
    if (alist != null)
      {
        LispObject entry = assq(docType, alist);
        if (entry instanceof Cons)
          return ((Cons)entry).cdr;
      }
    return NIL;
  }

  public void setDocumentation(LispObject docType, LispObject documentation)
    throws ConditionThrowable
  {
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

  public LispObject getPropertyList()
  {
    return null;
  }

  public void setPropertyList(LispObject obj)
  {
  }

  public LispObject getSymbolValue() throws ConditionThrowable
  {
    return type_error(this, Symbol.SYMBOL);
  }

  public LispObject getSymbolFunction() throws ConditionThrowable
  {
    return type_error(this, Symbol.SYMBOL);
  }

  public LispObject getSymbolFunctionOrDie() throws ConditionThrowable
  {
    return type_error(this, Symbol.SYMBOL);
  }

  public String writeToString() throws ConditionThrowable
  {
    return toString();
  }

  public String unreadableString(String s)
  {
    FastStringBuffer sb = new FastStringBuffer("#<");
    sb.append(s);
    sb.append(" {");
    sb.append(Integer.toHexString(System.identityHashCode(this)).toUpperCase());
    sb.append("}>");
    return sb.toString();
  }

  public String unreadableString(Symbol symbol) throws ConditionThrowable
  {
    FastStringBuffer sb = new FastStringBuffer("#<");
    sb.append(symbol.writeToString());
    sb.append(" {");
    sb.append(Integer.toHexString(System.identityHashCode(this)).toUpperCase());
    sb.append("}>");
    return sb.toString();
  }

  // Special operator
  public LispObject execute(LispObject args, Environment env)
    throws ConditionThrowable
  {
    return error(new LispError());
  }

  public LispObject execute() throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject arg) throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second)
    throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third)
    throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth)
    throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth)
    throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth)
    throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh)
    throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject first, LispObject second,
                            LispObject third, LispObject fourth,
                            LispObject fifth, LispObject sixth,
                            LispObject seventh, LispObject eighth)
    throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  public LispObject execute(LispObject[] args) throws ConditionThrowable
  {
    return type_error(this, Symbol.FUNCTION);
  }

  // Used by COMPILE-MULTIPLE-VALUE-CALL.
  public LispObject dispatch(LispObject[] args) throws ConditionThrowable
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
        return type_error(this, Symbol.FUNCTION);
      }
  }

  public int intValue() throws ConditionThrowable
  {
    type_error(this, Symbol.INTEGER);
    // Not reached.
    return 0;
  }

  public long longValue() throws ConditionThrowable
  {
    type_error(this, Symbol.INTEGER);
    // Not reached.
    return 0;
  }

  public float floatValue() throws ConditionThrowable
  {
    type_error(this, Symbol.SINGLE_FLOAT);
    // Not reached
    return 0;
  }

  public double doubleValue() throws ConditionThrowable
  {
    type_error(this, Symbol.DOUBLE_FLOAT);
    // Not reached
    return 0;
  }

  public LispObject incr() throws ConditionThrowable
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject decr() throws ConditionThrowable
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject negate() throws ConditionThrowable
  {
    return Fixnum.ZERO.subtract(this);
  }

  public LispObject add(int n) throws ConditionThrowable
  {
    return add(Fixnum.getInstance(n));
  }

  public LispObject add(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject subtract(int n) throws ConditionThrowable
  {
    return subtract(Fixnum.getInstance(n));
  }

  public LispObject subtract(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject multiplyBy(int n) throws ConditionThrowable
  {
    return multiplyBy(Fixnum.getInstance(n));
  }

  public LispObject multiplyBy(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.NUMBER);
  }

  public LispObject divideBy(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.NUMBER);
  }

  public boolean isEqualTo(int n) throws ConditionThrowable
  {
    return isEqualTo(Fixnum.getInstance(n));
  }

  public boolean isEqualTo(LispObject obj) throws ConditionThrowable
  {
    type_error(this, Symbol.NUMBER);
    // Not reached.
    return false;
  }

  public LispObject IS_E(LispObject obj) throws ConditionThrowable
  {
    return isEqualTo(obj) ? T : NIL;
  }

  public boolean isNotEqualTo(int n) throws ConditionThrowable
  {
    return isNotEqualTo(Fixnum.getInstance(n));
  }

  public boolean isNotEqualTo(LispObject obj) throws ConditionThrowable
  {
    type_error(this, Symbol.NUMBER);
    // Not reached.
    return false;
  }

  public LispObject IS_NE(LispObject obj) throws ConditionThrowable
  {
    return isNotEqualTo(obj) ? T : NIL;
  }

  public boolean isLessThan(int n) throws ConditionThrowable
  {
    return isLessThan(Fixnum.getInstance(n));
  }

  public boolean isLessThan(LispObject obj) throws ConditionThrowable
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public LispObject IS_LT(LispObject obj) throws ConditionThrowable
  {
    return isLessThan(obj) ? T : NIL;
  }

  public boolean isGreaterThan(int n) throws ConditionThrowable
  {
    return isGreaterThan(Fixnum.getInstance(n));
  }

  public boolean isGreaterThan(LispObject obj) throws ConditionThrowable
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public LispObject IS_GT(LispObject obj) throws ConditionThrowable
  {
    return isGreaterThan(obj) ? T : NIL;
  }

  public boolean isLessThanOrEqualTo(int n) throws ConditionThrowable
  {
    return isLessThanOrEqualTo(Fixnum.getInstance(n));
  }

  public boolean isLessThanOrEqualTo(LispObject obj) throws ConditionThrowable
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public LispObject IS_LE(LispObject obj) throws ConditionThrowable
  {
    return isLessThanOrEqualTo(obj) ? T : NIL;
  }

  public boolean isGreaterThanOrEqualTo(int n) throws ConditionThrowable
  {
    return isGreaterThanOrEqualTo(Fixnum.getInstance(n));
  }

  public boolean isGreaterThanOrEqualTo(LispObject obj) throws ConditionThrowable
  {
    type_error(this, Symbol.REAL);
    // Not reached.
    return false;
  }

  public LispObject IS_GE(LispObject obj) throws ConditionThrowable
  {
    return isGreaterThanOrEqualTo(obj) ? T : NIL;
  }

  public LispObject truncate(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.REAL);
  }

  public LispObject MOD(LispObject divisor) throws ConditionThrowable
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

  public LispObject MOD(int divisor) throws ConditionThrowable
  {
    return MOD(Fixnum.getInstance(divisor));
  }

  public LispObject ash(int shift) throws ConditionThrowable
  {
    return ash(Fixnum.getInstance(shift));
  }

  public LispObject ash(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LOGNOT() throws ConditionThrowable
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LOGAND(int n) throws ConditionThrowable
  {
    return LOGAND(Fixnum.getInstance(n));
  }

  public LispObject LOGAND(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LOGIOR(int n) throws ConditionThrowable
  {
    return LOGIOR(Fixnum.getInstance(n));
  }

  public LispObject LOGIOR(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LOGXOR(int n) throws ConditionThrowable
  {
    return LOGXOR(Fixnum.getInstance(n));
  }

  public LispObject LOGXOR(LispObject obj) throws ConditionThrowable
  {
    return type_error(this, Symbol.INTEGER);
  }

  public LispObject LDB(int size, int position) throws ConditionThrowable
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

  public LispObject STRING() throws ConditionThrowable
  {
    return error(new TypeError(writeToString() + " cannot be coerced to a string."));
  }

  public char[] chars() throws ConditionThrowable
  {
    type_error(this, Symbol.STRING);
    // Not reached.
    return null;
  }

  public char[] getStringChars() throws ConditionThrowable
  {
    type_error(this, Symbol.STRING);
    // Not reached.
    return null;
  }

  public String getStringValue() throws ConditionThrowable
  {
    type_error(this, Symbol.STRING);
    // Not reached.
    return null;
  }

  public LispObject getSlotValue_0() throws ConditionThrowable
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject getSlotValue_1() throws ConditionThrowable
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject getSlotValue_2() throws ConditionThrowable
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject getSlotValue_3() throws ConditionThrowable
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject getSlotValue(int index) throws ConditionThrowable
  {
    return type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public int getFixnumSlotValue(int index) throws ConditionThrowable
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
    // Not reached.
    return 0;
  }

  public boolean getSlotValueAsBoolean(int index) throws ConditionThrowable
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
    // Not reached.
    return false;
  }

  public void setSlotValue_0(LispObject value)
    throws ConditionThrowable
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public void setSlotValue_1(LispObject value)
    throws ConditionThrowable
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public void setSlotValue_2(LispObject value)
    throws ConditionThrowable
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public void setSlotValue_3(LispObject value)
    throws ConditionThrowable
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public void setSlotValue(int index, LispObject value)
    throws ConditionThrowable
  {
    type_error(this, Symbol.STRUCTURE_OBJECT);
  }

  public LispObject SLOT_VALUE(LispObject slotName) throws ConditionThrowable
  {
    return type_error(this, Symbol.STANDARD_OBJECT);
  }

  public void setSlotValue(LispObject slotName, LispObject newValue)
    throws ConditionThrowable
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
}
