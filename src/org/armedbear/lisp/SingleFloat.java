/*
 * SingleFloat.java
 *
 * Copyright (C) 2003-2007 Peter Graves
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

import java.math.BigInteger;

public final class SingleFloat extends LispObject
{
    public static final SingleFloat ZERO       = new SingleFloat(0);
    public static final SingleFloat MINUS_ZERO = new SingleFloat(-0.0f);
    public static final SingleFloat ONE        = new SingleFloat(1);
    public static final SingleFloat MINUS_ONE  = new SingleFloat(-1);

    public static final SingleFloat SINGLE_FLOAT_POSITIVE_INFINITY =
        new SingleFloat(Float.POSITIVE_INFINITY);

    public static final SingleFloat SINGLE_FLOAT_NEGATIVE_INFINITY =
        new SingleFloat(Float.NEGATIVE_INFINITY);

    static {
        Symbol.SINGLE_FLOAT_POSITIVE_INFINITY.initializeConstant(SINGLE_FLOAT_POSITIVE_INFINITY);
        Symbol.SINGLE_FLOAT_NEGATIVE_INFINITY.initializeConstant(SINGLE_FLOAT_NEGATIVE_INFINITY);
    }

    public static SingleFloat getInstance(float f) {
        if (f == 0) {
            int bits = Float.floatToRawIntBits(f);
            if (bits < 0)
                return MINUS_ZERO;
            else
                return ZERO;
        }
        else if (f == 1)
            return ONE;
        else if (f == -1)
            return MINUS_ONE;
        else
            return new SingleFloat(f);
    }

    public final float value;

    public SingleFloat(float value)
    {
        this.value = value;
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.SINGLE_FLOAT;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.SINGLE_FLOAT;
    }

    @Override
    public LispObject typep(LispObject typeSpecifier)
    {
        if (typeSpecifier == Symbol.FLOAT)
            return T;
        if (typeSpecifier == Symbol.REAL)
            return T;
        if (typeSpecifier == Symbol.NUMBER)
            return T;
        if (typeSpecifier == Symbol.SINGLE_FLOAT)
            return T;
        if (typeSpecifier == Symbol.SHORT_FLOAT)
            return T;
        if (typeSpecifier == BuiltInClass.FLOAT)
            return T;
        if (typeSpecifier == BuiltInClass.SINGLE_FLOAT)
            return T;
        return super.typep(typeSpecifier);
    }

    @Override
    public boolean numberp()
    {
        return true;
    }

    @Override
    public boolean realp()
    {
        return true;
    }

    @Override
    public boolean eql(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof SingleFloat) {
            if (value == 0) {
                // "If an implementation supports positive and negative zeros
                // as distinct values, then (EQL 0.0 -0.0) returns false."
                float f = ((SingleFloat)obj).value;
                int bits = Float.floatToRawIntBits(f);
                return bits == Float.floatToRawIntBits(value);
            }
            if (value == ((SingleFloat)obj).value)
                return true;
        }
        return false;
    }

    @Override
    public boolean equal(LispObject obj)
    {
        if (this == obj)
            return true;
        if (obj instanceof SingleFloat) {
            if (value == 0) {
                // same as EQL
                float f = ((SingleFloat)obj).value;
                int bits = Float.floatToRawIntBits(f);
                return bits == Float.floatToRawIntBits(value);
            }
            if (value == ((SingleFloat)obj).value)
                return true;
        }
        return false;
    }

    @Override
    public boolean equalp(int n)
    {
        // "If two numbers are the same under =."
        return value == n;
    }

    @Override
    public boolean equalp(LispObject obj)
    {
        if (obj instanceof SingleFloat)
            return value == ((SingleFloat)obj).value;
        if (obj instanceof DoubleFloat)
            return value == ((DoubleFloat)obj).value;
        if (obj instanceof Fixnum)
            return value == ((Fixnum)obj).value;
        if (obj instanceof Bignum)
            return value == ((Bignum)obj).floatValue();
        if (obj instanceof Ratio)
            return value == ((Ratio)obj).floatValue();
        return false;
    }

    @Override
    public LispObject ABS()
    {
        if (value > 0)
            return this;
        if (value == 0) // 0.0 or -0.0
            return ZERO;
        return new SingleFloat(- value);
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

    @Override
    public boolean floatp()
    {
        return true;
    }

    public static double getValue(LispObject obj)
    {
        if (obj instanceof SingleFloat)
            return ((SingleFloat)obj).value;
        type_error(obj, Symbol.FLOAT);
        // not reached
        return 0.0D;
    }

    public final float getValue()
    {
        return value;
    }

    @Override
    public float floatValue() {
        return value;
    }

    @Override
    public double doubleValue() {
        return value;
    }

    @Override
    public Object javaInstance()
    {
        return Float.valueOf(value);
    }

    @Override
    public Object javaInstance(Class c)
    {
        String cn = c.getName();
        if (cn.equals("java.lang.Float") || cn.equals("float"))
            return Float.valueOf(value);
        return javaInstance();
    }

    @Override
    public final LispObject incr()
    {
        return new SingleFloat(value + 1);
    }

    @Override
    public final LispObject decr()
    {
        return new SingleFloat(value - 1);
    }

    @Override
    public LispObject add(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return new SingleFloat(value + ((Fixnum)obj).value);
        if (obj instanceof SingleFloat)
            return new SingleFloat(value + ((SingleFloat)obj).value);
        if (obj instanceof DoubleFloat)
            return new DoubleFloat(value + ((DoubleFloat)obj).value);
        if (obj instanceof Bignum)
            return new SingleFloat(value + ((Bignum)obj).floatValue());
        if (obj instanceof Ratio)
            return new SingleFloat(value + ((Ratio)obj).floatValue());
        if (obj instanceof Complex) {
            Complex c = (Complex) obj;
            return Complex.getInstance(add(c.getRealPart()), c.getImaginaryPart());
        }
        return type_error(obj, Symbol.NUMBER);
    }

    @Override
    public LispObject negate()
    {
        if (value == 0) {
            int bits = Float.floatToRawIntBits(value);
            return (bits < 0) ? ZERO : MINUS_ZERO;
        }
        return new SingleFloat(-value);
    }

    @Override
    public LispObject subtract(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return new SingleFloat(value - ((Fixnum)obj).value);
        if (obj instanceof SingleFloat)
            return new SingleFloat(value - ((SingleFloat)obj).value);
        if (obj instanceof DoubleFloat)
            return new DoubleFloat(value - ((DoubleFloat)obj).value);
        if (obj instanceof Bignum)
            return new SingleFloat(value - ((Bignum)obj).floatValue());
        if (obj instanceof Ratio)
            return new SingleFloat(value - ((Ratio)obj).floatValue());
        if (obj instanceof Complex) {
            Complex c = (Complex) obj;
            return Complex.getInstance(subtract(c.getRealPart()),
                                       ZERO.subtract(c.getImaginaryPart()));
        }
        return type_error(obj, Symbol.NUMBER);
    }

    @Override
    public LispObject multiplyBy(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return new SingleFloat(value * ((Fixnum)obj).value);
        if (obj instanceof SingleFloat)
            return new SingleFloat(value * ((SingleFloat)obj).value);
        if (obj instanceof DoubleFloat)
            return new DoubleFloat(value * ((DoubleFloat)obj).value);
        if (obj instanceof Bignum)
            return new SingleFloat(value * ((Bignum)obj).floatValue());
        if (obj instanceof Ratio)
            return new SingleFloat(value * ((Ratio)obj).floatValue());
        if (obj instanceof Complex) {
            Complex c = (Complex) obj;
            return Complex.getInstance(multiplyBy(c.getRealPart()),
                                       multiplyBy(c.getImaginaryPart()));
        }
        return type_error(obj, Symbol.NUMBER);
    }

    @Override
    public LispObject divideBy(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return new SingleFloat(value / ((Fixnum)obj).value);
        if (obj instanceof SingleFloat)
            return new SingleFloat(value / ((SingleFloat)obj).value);
        if (obj instanceof DoubleFloat)
            return new DoubleFloat(value / ((DoubleFloat)obj).value);
        if (obj instanceof Bignum)
            return new SingleFloat(value / ((Bignum)obj).floatValue());
        if (obj instanceof Ratio)
            return new SingleFloat(value / ((Ratio)obj).floatValue());
        if (obj instanceof Complex) {
            Complex c = (Complex) obj;
            LispObject re = c.getRealPart();
            LispObject im = c.getImaginaryPart();
            LispObject denom = re.multiplyBy(re).add(im.multiplyBy(im));
            LispObject resX = multiplyBy(re).divideBy(denom);
            LispObject resY =
                multiplyBy(Fixnum.MINUS_ONE).multiplyBy(im).divideBy(denom);
            return Complex.getInstance(resX, resY);
        }
        return type_error(obj, Symbol.NUMBER);
    }

    @Override
    public boolean isEqualTo(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return rational().isEqualTo(obj);
        if (obj instanceof SingleFloat)
            return value == ((SingleFloat)obj).value;
        if (obj instanceof DoubleFloat)
            return value == ((DoubleFloat)obj).value;
        if (obj instanceof Bignum)
            return rational().isEqualTo(obj);
        if (obj instanceof Ratio)
            return rational().isEqualTo(obj);
        if (obj instanceof Complex)
            return obj.isEqualTo(this);
        type_error(obj, Symbol.NUMBER);
        // Not reached.
        return false;
    }

    @Override
    public boolean isNotEqualTo(LispObject obj)
    {
        return !isEqualTo(obj);
    }

    @Override
    public boolean isLessThan(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return rational().isLessThan(obj);
        if (obj instanceof SingleFloat)
            return value < ((SingleFloat)obj).value;
        if (obj instanceof DoubleFloat)
            return value < ((DoubleFloat)obj).value;
        if (obj instanceof Bignum)
            return rational().isLessThan(obj);
        if (obj instanceof Ratio)
            return rational().isLessThan(obj);
        type_error(obj, Symbol.REAL);
        // Not reached.
        return false;
    }

    @Override
    public boolean isGreaterThan(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return rational().isGreaterThan(obj);
        if (obj instanceof SingleFloat)
            return value > ((SingleFloat)obj).value;
        if (obj instanceof DoubleFloat)
            return value > ((DoubleFloat)obj).value;
        if (obj instanceof Bignum)
            return rational().isGreaterThan(obj);
        if (obj instanceof Ratio)
            return rational().isGreaterThan(obj);
        type_error(obj, Symbol.REAL);
        // Not reached.
        return false;
    }

    @Override
    public boolean isLessThanOrEqualTo(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return rational().isLessThanOrEqualTo(obj);
        if (obj instanceof SingleFloat)
            return value <= ((SingleFloat)obj).value;
        if (obj instanceof DoubleFloat)
            return value <= ((DoubleFloat)obj).value;
        if (obj instanceof Bignum)
            return rational().isLessThanOrEqualTo(obj);
        if (obj instanceof Ratio)
            return rational().isLessThanOrEqualTo(obj);
        type_error(obj, Symbol.REAL);
        // Not reached.
        return false;
    }

    @Override
    public boolean isGreaterThanOrEqualTo(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return rational().isGreaterThanOrEqualTo(obj);
        if (obj instanceof SingleFloat)
            return value >= ((SingleFloat)obj).value;
        if (obj instanceof DoubleFloat)
            return value >= ((DoubleFloat)obj).value;
        if (obj instanceof Bignum)
            return rational().isGreaterThanOrEqualTo(obj);
        if (obj instanceof Ratio)
            return rational().isGreaterThanOrEqualTo(obj);
        type_error(obj, Symbol.REAL);
        // Not reached.
        return false;
    }

    @Override
    public LispObject truncate(LispObject obj)
    {
        // "When rationals and floats are combined by a numerical function,
        // the rational is first converted to a float of the same format."
        // 12.1.4.1
        if (obj instanceof Fixnum) {
            return truncate(new SingleFloat(((Fixnum)obj).value));
        }
        if (obj instanceof Bignum) {
            return truncate(new SingleFloat(((Bignum)obj).floatValue()));
        }
        if (obj instanceof Ratio) {
            return truncate(new SingleFloat(((Ratio)obj).floatValue()));
        }
        if (obj instanceof SingleFloat) {
            final LispThread thread = LispThread.currentThread();
            float divisor = ((SingleFloat)obj).value;
            float quotient = value / divisor;
            if (value != 0)
                MathFunctions.OverUnderFlowCheck(quotient);
            if (quotient >= Integer.MIN_VALUE && quotient <= Integer.MAX_VALUE) {
                int q = (int) quotient;
                return thread.setValues(Fixnum.getInstance(q),
                                        new SingleFloat(value - q * divisor));
            }
            // We need to convert the quotient to a bignum.
            int bits = Float.floatToRawIntBits(quotient);
            int s = ((bits >> 31) == 0) ? 1 : -1;
            int e = (int) ((bits >> 23) & 0xff);
            long m;
            if (e == 0)
                m = (bits & 0x7fffff) << 1;
            else
                m = (bits & 0x7fffff) | 0x800000;
            LispObject significand = number(m);
            Fixnum exponent = Fixnum.getInstance(e - 150);
            Fixnum sign = Fixnum.getInstance(s);
            LispObject result = significand;
            result =
                result.multiplyBy(MathFunctions.EXPT.execute(Fixnum.TWO, exponent));
            result = result.multiplyBy(sign);
            // Calculate remainder.
            LispObject product = result.multiplyBy(obj);
            LispObject remainder = subtract(product);
            return thread.setValues(result, remainder);
        }
        if (obj instanceof DoubleFloat) {
            final LispThread thread = LispThread.currentThread();
            double divisor = ((DoubleFloat)obj).value;
            double quotient = value / divisor;
            if (value != 0)
                MathFunctions.OverUnderFlowCheck(quotient);
            if (quotient >= Integer.MIN_VALUE && quotient <= Integer.MAX_VALUE) {
                int q = (int) quotient;
                return thread.setValues(Fixnum.getInstance(q),
                                        new DoubleFloat(value - q * divisor));
            }
            // We need to convert the quotient to a bignum.
            long bits = Double.doubleToRawLongBits((double)quotient);
            int s = ((bits >> 63) == 0) ? 1 : -1;
            int e = (int) ((bits >> 52) & 0x7ffL);
            long m;
            if (e == 0)
                m = (bits & 0xfffffffffffffL) << 1;
            else
                m = (bits & 0xfffffffffffffL) | 0x10000000000000L;
            LispObject significand = number(m);
            Fixnum exponent = Fixnum.getInstance(e - 1075);
            Fixnum sign = Fixnum.getInstance(s);
            LispObject result = significand;
            result =
                result.multiplyBy(MathFunctions.EXPT.execute(Fixnum.TWO, exponent));
            result = result.multiplyBy(sign);
            // Calculate remainder.
            LispObject product = result.multiplyBy(obj);
            LispObject remainder = subtract(product);
            return thread.setValues(result, remainder);
        }
        return type_error(obj, Symbol.REAL);
    }

    @Override
    public int hashCode()
    {
        return Float.floatToIntBits(value);
    }

    @Override
    public int psxhash()
    {
        if ((value % 1) == 0)
            return (((int)value) & 0x7fffffff);
        else
            return (hashCode() & 0x7fffffff);
    }

    @Override
    public String printObject()
    {
        if (value == Float.POSITIVE_INFINITY) {
            StringBuffer sb = new StringBuffer("#.");
            sb.append(Symbol.SINGLE_FLOAT_POSITIVE_INFINITY.printObject());
            return sb.toString();
        }
        if (value == Float.NEGATIVE_INFINITY) {
            StringBuffer sb = new StringBuffer("#.");
            sb.append(Symbol.SINGLE_FLOAT_NEGATIVE_INFINITY.printObject());
            return sb.toString();
        }

        LispThread thread = LispThread.currentThread();
        boolean printReadably = Symbol.PRINT_READABLY.symbolValue(thread) != NIL;

        if (value != value) {
            if (printReadably)
                return "#.(CL:PROGN \"Comment: create a NaN.\" (CL:/ 0.0s0 0.0s0))";
            else
                return unreadableString("SINGLE-FLOAT NaN", false);
        }
        String s1 = String.valueOf(value);
        if (printReadably ||
            !memq(Symbol.READ_DEFAULT_FLOAT_FORMAT.symbolValue(thread),
                  list(Symbol.SINGLE_FLOAT, Symbol.SHORT_FLOAT)))
        {
            if (s1.indexOf('E') >= 0)
                return s1.replace('E', 'f');
            else
                return s1.concat("f0");
        } else
            return s1;
    }

    public LispObject rational()
    {
        final int bits = Float.floatToRawIntBits(value);
        int sign = ((bits >> 31) == 0) ? 1 : -1;
        int storedExponent = ((bits >> 23) & 0xff);
        long mantissa;
        if (storedExponent == 0)
            mantissa = (bits & 0x7fffff) << 1;
        else
            mantissa = (bits & 0x7fffff) | 0x800000;
        if (mantissa == 0)
            return Fixnum.ZERO;
        if (sign < 0)
            mantissa = -mantissa;
        // Subtract bias.
        final int exponent = storedExponent - 127;
        BigInteger numerator, denominator;
        if (exponent < 0) {
            numerator = BigInteger.valueOf(mantissa);
            denominator = BigInteger.valueOf(1).shiftLeft(23 - exponent);
        } else {
            numerator = BigInteger.valueOf(mantissa).shiftLeft(exponent);
            denominator = BigInteger.valueOf(0x800000); // (ash 1 23)
        }
        return number(numerator, denominator);
    }

    public static SingleFloat coerceToFloat(LispObject obj)
    {
        if (obj instanceof Fixnum)
            return new SingleFloat(((Fixnum)obj).value);
        if (obj instanceof SingleFloat)
            return (SingleFloat) obj;
        if (obj instanceof DoubleFloat) {
            float result = (float)((DoubleFloat)obj).value;
            if (Float.isInfinite(result) && TRAP_OVERFLOW)
                type_error(obj, Symbol.SINGLE_FLOAT);

            return new SingleFloat(result);
        }
        if (obj instanceof Bignum)
            return new SingleFloat(((Bignum)obj).floatValue());
        if (obj instanceof Ratio)
            return new SingleFloat(((Ratio)obj).floatValue());
        error(new TypeError("The value " + obj.princToString() +
                             " cannot be converted to type SINGLE-FLOAT."));
        // Not reached.
        return null;
    }
}
