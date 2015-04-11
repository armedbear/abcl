/*
 * Fixnum.java
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

import java.math.BigInteger;

public final class Fixnum extends LispInteger
{
  public static final int MAX_POS_CACHE = 256;//just like before - however never set this to less than 256
  public static final Fixnum[] constants = new Fixnum[MAX_POS_CACHE];
  static
  {
    for (int i = 0; i < MAX_POS_CACHE; i++)
      constants[i] = new Fixnum(i);
  }

  public static final Fixnum ZERO      = constants[0];
  public static final Fixnum ONE       = constants[1];
  public static final Fixnum TWO       = constants[2];
  public static final Fixnum THREE     = constants[3];

  public static final Fixnum MINUS_ONE = Fixnum.getInstance(-1);

  public static Fixnum getInstance(int n)
  {
    return (n >= 0 && n < MAX_POS_CACHE) ? constants[n] : new Fixnum(n);
  }

  public final int value;

  // set to private to hunt down sneaky creators
  private Fixnum(int value)
  {
    this.value = value;
  }

  @Override
  public Object javaInstance()
  {
    return Integer.valueOf(value);
  }

  @Override
  public Object javaInstance(Class c)
  {
    String cn = c.getName();
    if (cn.equals("java.lang.Byte") || cn.equals("byte"))
      return Byte.valueOf((byte)value);
    if (cn.equals("java.lang.Short") || cn.equals("short"))
      return Short.valueOf((short)value);
    if (cn.equals("java.lang.Long") || cn.equals("long"))
      return Long.valueOf((long)value);
    return javaInstance();
  }

  @Override
  public LispObject typeOf()
  {
    if (value == 0 || value == 1)
      return Symbol.BIT;
    if (value > 1)
      return list(Symbol.INTEGER, ZERO, Fixnum.getInstance(Integer.MAX_VALUE));
    return Symbol.FIXNUM;
  }

  @Override
  public LispObject classOf()
  {
    return BuiltInClass.FIXNUM;
  }

  @Override
  public LispObject getDescription()
  {
    StringBuffer sb = new StringBuffer("The fixnum ");
    sb.append(value);
    return new SimpleString(sb);
  }

  @Override
  public LispObject typep(LispObject type)
  {
    if (type instanceof Symbol)
      {
        if (type == Symbol.FIXNUM)
          return T;
        if (type == Symbol.INTEGER)
          return T;
        if (type == Symbol.RATIONAL)
          return T;
        if (type == Symbol.REAL)
          return T;
        if (type == Symbol.NUMBER)
          return T;
        if (type == Symbol.SIGNED_BYTE)
          return T;
        if (type == Symbol.UNSIGNED_BYTE)
          return value >= 0 ? T : NIL;
        if (type == Symbol.BIT)
          return (value == 0 || value == 1) ? T : NIL;
      }
    else if (type instanceof LispClass)
      {
        if (type == BuiltInClass.FIXNUM)
          return T;
        if (type == BuiltInClass.INTEGER)
          return T;
        if (type == BuiltInClass.RATIONAL)
          return T;
        if (type == BuiltInClass.REAL)
          return T;
        if (type == BuiltInClass.NUMBER)
          return T;
      }
    else if (type instanceof Cons)
      {
        if (type.equal(UNSIGNED_BYTE_8))
          return (value >= 0 && value <= 255) ? T : NIL;
        if (type.equal(UNSIGNED_BYTE_16))
          return (value >= 0 && value <= 65535) ? T : NIL;
        if (type.equal(UNSIGNED_BYTE_32))
          return value >= 0 ? T : NIL;
      }
    return super.typep(type);
  }

  @Override
  public boolean numberp()
  {
    return true;
  }

  @Override
  public boolean integerp()
  {
    return true;
  }

  @Override
  public boolean rationalp()
  {
    return true;
  }

  @Override
  public boolean realp()
  {
    return true;
  }

  @Override
  public boolean eql(int n)
  {
    return value == n;
  }

  @Override
  public boolean eql(LispObject obj)
  {
    if (this == obj)
      return true;
    if (obj instanceof Fixnum)
      {
        if (value == ((Fixnum)obj).value)
          return true;
      }
    return false;
  }

  @Override
  public boolean equal(int n)
  {
    return value == n;
  }

  @Override
  public boolean equal(LispObject obj)
  {
    if (this == obj)
      return true;
    if (obj instanceof Fixnum)
      {
        if (value == ((Fixnum)obj).value)
          return true;
      }
    return false;
  }

  @Override
  public boolean equalp(int n)
  {
    return value == n;
  }

  @Override
  public boolean equalp(LispObject obj)
  {
    if (obj != null && obj.numberp())
      return isEqualTo(obj);
    return false;
  }

  @Override
  public LispObject ABS()
  {
    if (value >= 0)
      return this;
    return LispInteger.getInstance(-(long)value);
  }

  @Override
  public LispObject NUMERATOR()
  {
    return this;
  }

  @Override
  public LispObject DENOMINATOR()
  {
    return ONE;
  }

  @Override
  public boolean evenp()
  {
    return (value & 0x01) == 0;
  }

  @Override
  public boolean oddp()
  {
    return (value & 0x01) != 0;
  }

  @Override
  public boolean plusp()
  {
    return value > 0;
  }

  @Override
  public boolean minusp()
  {
    return value < 0;
  }

  @Override
  public boolean zerop()
  {
    return value == 0;
  }

  public static int getValue(LispObject obj)
  {
          if (obj instanceof Fixnum) return ((Fixnum)obj).value;
          type_error(obj, Symbol.FIXNUM);
      // Not reached.
          return 0;
  }

  @Override
  public float floatValue() {
    return (float)value;
  }

  @Override
  public double doubleValue() {
    return (double)value;
  }

  public static int getInt(LispObject obj)
  {
          if (obj instanceof Fixnum) return ((Fixnum)obj).value;
          type_error(obj, Symbol.FIXNUM);
      // Not reached.
          return 0;
  }

  public static BigInteger getBigInteger(LispObject obj)
  {
          if (obj instanceof Fixnum) return BigInteger.valueOf(((Fixnum)obj).value);
          type_error(obj, Symbol.FIXNUM);
      // Not reached.
          return null;
  }

  @Override
  public int intValue()
  {
    return value;
  }

  @Override
  public long longValue()
  {
    return (long) value;
  }

  public final BigInteger getBigInteger()
  {
    return BigInteger.valueOf(value);
  }

  @Override
  public final LispObject incr()
  {
    return LispInteger.getInstance(1 + (long)value);
  }

  @Override
  public final LispObject decr()
  {
    return LispInteger.getInstance(-1 + (long)value);
  }

  @Override
  public LispObject negate()
  {
    return LispInteger.getInstance((-(long)value));
  }

  @Override
  public LispObject add(int n)
  {
    return LispInteger.getInstance((long) value + n);
  }

  @Override
  public LispObject add(LispObject obj)
  {
    if (obj instanceof Fixnum)
      {
        long result = (long) value + ((Fixnum)obj).value;
        return LispInteger.getInstance(result);
      }
    if (obj instanceof Bignum)
      return number(getBigInteger().add(((Bignum)obj).value));
    if (obj instanceof Ratio)
      {
        BigInteger numerator = ((Ratio)obj).numerator();
        BigInteger denominator = ((Ratio)obj).denominator();
        return number(getBigInteger().multiply(denominator).add(numerator),
                      denominator);
      }
    if (obj instanceof SingleFloat)
      return new SingleFloat(value + ((SingleFloat)obj).value);
    if (obj instanceof DoubleFloat)
      return new DoubleFloat(value + ((DoubleFloat)obj).value);
    if (obj instanceof Complex)
      {
        Complex c = (Complex) obj;
        return Complex.getInstance(add(c.getRealPart()), c.getImaginaryPart());
      }
    return type_error(obj, Symbol.NUMBER);
  }

  @Override
  public LispObject subtract(int n)
  {
    return LispInteger.getInstance((long)value - n);
  }

  @Override
  public LispObject subtract(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return number((long) value - ((Fixnum)obj).value);
    if (obj instanceof Bignum)
      return number(getBigInteger().subtract(Bignum.getValue(obj)));
    if (obj instanceof Ratio)
      {
        BigInteger numerator = ((Ratio)obj).numerator();
        BigInteger denominator = ((Ratio)obj).denominator();
        return number(
          getBigInteger().multiply(denominator).subtract(numerator),
          denominator);
      }
    if (obj instanceof SingleFloat)
      return new SingleFloat(value - ((SingleFloat)obj).value);
    if (obj instanceof DoubleFloat)
      return new DoubleFloat(value - ((DoubleFloat)obj).value);
    if (obj instanceof Complex)
      {
        Complex c = (Complex) obj;
        return Complex.getInstance(subtract(c.getRealPart()),
                                   ZERO.subtract(c.getImaginaryPart()));
      }
    return type_error(obj, Symbol.NUMBER);
  }

  @Override
  public LispObject multiplyBy(int n)
  {
    long result = (long) value * n;
    return LispInteger.getInstance(result);
  }

  @Override
  public LispObject multiplyBy(LispObject obj)
  {
    if (obj instanceof Fixnum)
      {
        long result = (long) value * ((Fixnum)obj).value;
        return LispInteger.getInstance(result);
      }
    if (obj instanceof Bignum)
      return number(getBigInteger().multiply(((Bignum)obj).value));
    if (obj instanceof Ratio)
      {
        BigInteger numerator = ((Ratio)obj).numerator();
        BigInteger denominator = ((Ratio)obj).denominator();
        return number(
          getBigInteger().multiply(numerator),
          denominator);
      }
    if (obj instanceof SingleFloat)
      return new SingleFloat(value * ((SingleFloat)obj).value);
    if (obj instanceof DoubleFloat)
      return new DoubleFloat(value * ((DoubleFloat)obj).value);
    if (obj instanceof Complex)
      {
        Complex c = (Complex) obj;
        return Complex.getInstance(multiplyBy(c.getRealPart()),
                                   multiplyBy(c.getImaginaryPart()));
      }
    return type_error(obj, Symbol.NUMBER);
  }

  @Override
  public LispObject divideBy(LispObject obj)
  {
    try
      {
        if (obj instanceof Fixnum)
          {
            final int divisor = ((Fixnum)obj).value;
            // (/ MOST-NEGATIVE-FIXNUM -1) is a bignum.
            if (value > Integer.MIN_VALUE)
              if (value % divisor == 0)
                return Fixnum.getInstance(value / divisor);
            return number(BigInteger.valueOf(value),
                          BigInteger.valueOf(divisor));
          }
        if (obj instanceof Bignum)
          return number(getBigInteger(), ((Bignum)obj).value);
        if (obj instanceof Ratio)
          {
            BigInteger numerator = ((Ratio)obj).numerator();
            BigInteger denominator = ((Ratio)obj).denominator();
            return number(getBigInteger().multiply(denominator),
                          numerator);
          }
        if (obj instanceof SingleFloat)
          return new SingleFloat(value / ((SingleFloat)obj).value);
        if (obj instanceof DoubleFloat)
          return new DoubleFloat(value / ((DoubleFloat)obj).value);
        if (obj instanceof Complex)
          {
            Complex c = (Complex) obj;
            LispObject realPart = c.getRealPart();
            LispObject imagPart = c.getImaginaryPart();
            LispObject denominator =
              realPart.multiplyBy(realPart).add(imagPart.multiplyBy(imagPart));
            return Complex.getInstance(multiplyBy(realPart).divideBy(denominator),
                                       Fixnum.ZERO.subtract(multiplyBy(imagPart).divideBy(denominator)));
          }
        return type_error(obj, Symbol.NUMBER);
      }
    catch (ArithmeticException e)
      {
        if (obj.zerop())
          return error(new DivisionByZero());
        return error(new ArithmeticError(e.getMessage()));
      }
  }

  @Override
  public boolean isEqualTo(int n)
  {
    return value == n;
  }

  @Override
  public boolean isEqualTo(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return value == ((Fixnum)obj).value;
    if (obj instanceof SingleFloat)
      return isEqualTo(((SingleFloat)obj).rational());
    if (obj instanceof DoubleFloat)
      return value == ((DoubleFloat)obj).value;
    if (obj instanceof Complex)
      return obj.isEqualTo(this);
    if (obj.numberp())
      return false;
    type_error(obj, Symbol.NUMBER);
    // Not reached.
    return false;
  }

  @Override
  public boolean isNotEqualTo(int n)
  {
    return value != n;
  }

  @Override
  public boolean isNotEqualTo(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return value != ((Fixnum)obj).value;
    // obj is not a fixnum.
    if (obj instanceof SingleFloat)
      return isNotEqualTo(((SingleFloat)obj).rational());
    if (obj instanceof DoubleFloat)
      return value != ((DoubleFloat)obj).value;
    if (obj instanceof Complex)
      return obj.isNotEqualTo(this);
    if (obj.numberp())
      return true;
    type_error(obj, Symbol.NUMBER);
    // Not reached.
    return false;
  }

  @Override
  public boolean isLessThan(int n)
  {
    return value < n;
  }

  @Override
  public boolean isLessThan(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return value < ((Fixnum)obj).value;
    if (obj instanceof Bignum)
      return getBigInteger().compareTo(Bignum.getValue(obj)) < 0;
    if (obj instanceof Ratio)
      {
        BigInteger n = getBigInteger().multiply(((Ratio)obj).denominator());
        return n.compareTo(((Ratio)obj).numerator()) < 0;
      }
    if (obj instanceof SingleFloat)
      return isLessThan(((SingleFloat)obj).rational());
    if (obj instanceof DoubleFloat)
      return isLessThan(((DoubleFloat)obj).rational());
    type_error(obj, Symbol.REAL);
    // Not reached.
    return false;
  }

  @Override
  public boolean isGreaterThan(int n)
  {
    return value > n;
  }

  @Override
  public boolean isGreaterThan(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return value > ((Fixnum)obj).value;
    if (obj instanceof Bignum)
      return getBigInteger().compareTo(Bignum.getValue(obj)) > 0;
    if (obj instanceof Ratio)
      {
        BigInteger n = getBigInteger().multiply(((Ratio)obj).denominator());
        return n.compareTo(((Ratio)obj).numerator()) > 0;
      }
    if (obj instanceof SingleFloat)
      return isGreaterThan(((SingleFloat)obj).rational());
    if (obj instanceof DoubleFloat)
      return isGreaterThan(((DoubleFloat)obj).rational());
    type_error(obj, Symbol.REAL);
    // Not reached.
    return false;
  }

  @Override
  public boolean isLessThanOrEqualTo(int n)
  {
    return value <= n;
  }

  @Override
  public boolean isLessThanOrEqualTo(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return value <= ((Fixnum)obj).value;
    if (obj instanceof Bignum)
      return getBigInteger().compareTo(Bignum.getValue(obj)) <= 0;
    if (obj instanceof Ratio)
      {
        BigInteger n = getBigInteger().multiply(((Ratio)obj).denominator());
        return n.compareTo(((Ratio)obj).numerator()) <= 0;
      }
    if (obj instanceof SingleFloat)
      return isLessThanOrEqualTo(((SingleFloat)obj).rational());
    if (obj instanceof DoubleFloat)
      return isLessThanOrEqualTo(((DoubleFloat)obj).rational());
    type_error(obj, Symbol.REAL);
    // Not reached.
    return false;
  }

  @Override
  public boolean isGreaterThanOrEqualTo(int n)
  {
    return value >= n;
  }

  @Override
  public boolean isGreaterThanOrEqualTo(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return value >= ((Fixnum)obj).value;
    if (obj instanceof Bignum)
      return getBigInteger().compareTo(Bignum.getValue(obj)) >= 0;
    if (obj instanceof Ratio)
      {
        BigInteger n = getBigInteger().multiply(((Ratio)obj).denominator());
        return n.compareTo(((Ratio)obj).numerator()) >= 0;
      }
    if (obj instanceof SingleFloat)
      return isGreaterThanOrEqualTo(((SingleFloat)obj).rational());
    if (obj instanceof DoubleFloat)
      return isGreaterThanOrEqualTo(((DoubleFloat)obj).rational());
    type_error(obj, Symbol.REAL);
    // Not reached.
    return false;
  }

  @Override
  public LispObject truncate(LispObject obj)
  {
    final LispThread thread = LispThread.currentThread();
    final LispObject value1, value2;
    try
      {
        if (obj instanceof Fixnum)
          {
            int divisor = ((Fixnum)obj).value;
            int quotient = value / divisor;
            int remainder = value % divisor;
            value1 = Fixnum.getInstance(quotient);
            value2 = remainder == 0 ? Fixnum.ZERO : Fixnum.getInstance(remainder);
          }
        else if (obj instanceof Bignum)
          {
            BigInteger val = getBigInteger();
            BigInteger divisor = ((Bignum)obj).value;
            BigInteger[] results = val.divideAndRemainder(divisor);
            BigInteger quotient = results[0];
            BigInteger remainder = results[1];
            value1 = number(quotient);
            value2 = (remainder.signum() == 0) ? Fixnum.ZERO : number(remainder);
          }
        else if (obj instanceof Ratio)
          {
            Ratio divisor = (Ratio) obj;
            LispObject quotient =
              multiplyBy(divisor.DENOMINATOR()).truncate(divisor.NUMERATOR());
            LispObject remainder =
              subtract(quotient.multiplyBy(divisor));
            value1 = quotient;
            value2 = remainder;
          }
        else if (obj instanceof SingleFloat)
          {
            // "When rationals and floats are combined by a numerical function,
            // the rational is first converted to a float of the same format."
            // 12.1.4.1
            return new SingleFloat(value).truncate(obj);
          }
        else if (obj instanceof DoubleFloat)
          {
            // "When rationals and floats are combined by a numerical function,
            // the rational is first converted to a float of the same format."
            // 12.1.4.1
            return new DoubleFloat(value).truncate(obj);
          }
        else
          return type_error(obj, Symbol.REAL);
      }
    catch (ArithmeticException e)
      {
        if (obj.zerop())
          return error(new DivisionByZero());
        else
          return error(new ArithmeticError(e.getMessage()));
      }
    return thread.setValues(value1, value2);
  }

  @Override
  public LispObject MOD(LispObject divisor)
  {
    if (divisor instanceof Fixnum)
      return MOD(((Fixnum)divisor).value);
    return super.MOD(divisor);
  }

  @Override
  public LispObject MOD(int divisor)
  {
    final int r;
    try
      {
        r = value % divisor;
      }
    catch (ArithmeticException e)
      {
        return error(new ArithmeticError("Division by zero."));
      }
    if (r == 0)
      return Fixnum.ZERO;
    if (divisor < 0)
      {
        if (value > 0)
          return Fixnum.getInstance(r + divisor);
      }
    else
      {
        if (value < 0)
          return Fixnum.getInstance(r + divisor);
      }
    return Fixnum.getInstance(r);
  }

  @Override
  public LispObject ash(int shift)
  {
    if (value == 0)
      return this;
    if (shift == 0)
      return this;
    long n = value;
    if (shift <= -32)
      {
        // Right shift.
        return n >= 0 ? Fixnum.ZERO : Fixnum.MINUS_ONE;
      }
    if (shift < 0)
      return Fixnum.getInstance((int)(n >> -shift));
    if (shift <= 32)
      {
        n = n << shift;
        return LispInteger.getInstance(n);
      }
    // BigInteger.shiftLeft() succumbs to a stack overflow if shift
    // is Integer.MIN_VALUE, so...
    if (shift == Integer.MIN_VALUE)
      return n >= 0 ? Fixnum.ZERO : Fixnum.MINUS_ONE;
    return number(BigInteger.valueOf(value).shiftLeft(shift));
  }

  @Override
  public LispObject ash(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return ash(((Fixnum)obj).value);
    if (obj instanceof Bignum)
      {
        if (value == 0)
          return this;
        BigInteger n = BigInteger.valueOf(value);
        BigInteger shift = ((Bignum)obj).value;
        if (shift.signum() > 0)
          return error(new LispError("Can't represent result of left shift."));
        if (shift.signum() < 0)
          return n.signum() >= 0 ? Fixnum.ZERO : Fixnum.MINUS_ONE;
        Debug.bug(); // Shouldn't happen.
      }
    return type_error(obj, Symbol.INTEGER);
  }

  @Override
  public LispObject LOGNOT()
  {
    return Fixnum.getInstance(~value);
  }

  @Override
  public LispObject LOGAND(int n)
  {
    return Fixnum.getInstance(value & n);
  }

  @Override
  public LispObject LOGAND(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return Fixnum.getInstance(value & ((Fixnum)obj).value);
    if (obj instanceof Bignum)
      {
        if (value >= 0)
          {
            int n2 = (((Bignum)obj).value).intValue();
            return Fixnum.getInstance(value & n2);
          }
        else
          {
            BigInteger n1 = getBigInteger();
            BigInteger n2 = ((Bignum)obj).value;
            return number(n1.and(n2));
          }
      }
    return type_error(obj, Symbol.INTEGER);
  }

  @Override
  public LispObject LOGIOR(int n)
  {
    return Fixnum.getInstance(value | n);
  }

  @Override
  public LispObject LOGIOR(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return Fixnum.getInstance(value | ((Fixnum)obj).value);
    if (obj instanceof Bignum)
      {
        BigInteger n1 = getBigInteger();
        BigInteger n2 = ((Bignum)obj).value;
        return number(n1.or(n2));
      }
    return type_error(obj, Symbol.INTEGER);
  }

  @Override
  public LispObject LOGXOR(int n)
  {
    return Fixnum.getInstance(value ^ n);
  }

  @Override
  public LispObject LOGXOR(LispObject obj)
  {
    if (obj instanceof Fixnum)
      return Fixnum.getInstance(value ^ ((Fixnum)obj).value);
    if (obj instanceof Bignum)
      {
        BigInteger n1 = getBigInteger();
        BigInteger n2 = ((Bignum)obj).value;
        return number(n1.xor(n2));
      }
    return type_error(obj, Symbol.INTEGER);
  }

  @Override
  public LispObject LDB(int size, int position)
  {
    long n = (long) value >> position;
    long mask = (1L << size) - 1;
    return number(n & mask);
  }

  final static BigInteger BIGINTEGER_TWO = new BigInteger ("2");

  /** Computes fixnum^bignum, returning a fixnum or a bignum.
    */
  public LispObject pow(LispObject obj)
  {
    BigInteger y = Bignum.getValue(obj);

    if (y.compareTo (BigInteger.ZERO) < 0)
      return (Fixnum.getInstance(1)).divideBy(this.pow(Bignum.getInstance(y.negate())));

    if (y.compareTo(BigInteger.ZERO) == 0)
      // No need to test base here; CLHS says 0^0 == 1.
      return Fixnum.getInstance(1);
      
    int x = this.value;

    if (x == 0)
      return Fixnum.getInstance(0);

    if (x == 1)
      return Fixnum.getInstance(1);

    BigInteger xy = BigInteger.ONE;
    BigInteger term = BigInteger.valueOf((long) x);

    while (! y.equals(BigInteger.ZERO))
    {
      if (y.testBit(0))
        xy = xy.multiply(term);

      term = term.multiply(term);
      y = y.shiftLeft(1);
    }

    return Bignum.getInstance(xy);
  }

  @Override
  public int hashCode()
  {
    return value;
  }

  @Override
  public String printObject()
  {
    final LispThread thread = LispThread.currentThread();
    int base = Fixnum.getValue(Symbol.PRINT_BASE.symbolValue(thread));
    String s = Integer.toString(value, base).toUpperCase();
    if (Symbol.PRINT_RADIX.symbolValue(thread) != NIL)
      {
        StringBuilder sb = new StringBuilder();
        switch (base)
          {
          case 2:
            sb.append("#b");
            sb.append(s);
            break;
          case 8:
            sb.append("#o");
            sb.append(s);
            break;
          case 10:
            sb.append(s);
            sb.append('.');
            break;
          case 16:
            sb.append("#x");
            sb.append(s);
            break;
          default:
            sb.append('#');
            sb.append(String.valueOf(base));
            sb.append('r');
            sb.append(s);
            break;
          }
        s = sb.toString();
      }
    return s;
  }
}
