/*
 * Primitives.java
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

import java.math.BigInteger;
import java.util.ArrayList;

public final class Primitives {
    // ### *
    public static final Primitive MULTIPLY = new pf_multiply();
    private static final class pf_multiply extends Primitive {
        pf_multiply() {
            super(Symbol.STAR, "&rest numbers");
        }

        @Override
        public LispObject execute() {
            return Fixnum.ONE;
        }
        @Override
        public LispObject execute(LispObject arg) {
            if (arg.numberp())
                return arg;
            return type_error(arg, Symbol.NUMBER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.multiplyBy(second);
        }
        @Override
        public LispObject execute(LispObject[] args) {
            LispObject result = Fixnum.ONE;
            for (int i = 0; i < args.length; i++)
                result = result.multiplyBy(args[i]);
            return result;
        }
    };

    // ### /
    public static final Primitive DIVIDE = new pf_divide();
    private static final class pf_divide extends Primitive {
        pf_divide() {
            super(Symbol.SLASH, "numerator &rest denominators");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return Fixnum.ONE.divideBy(arg);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.divideBy(second);
        }
        @Override
        public LispObject execute(LispObject[] args) {
            LispObject result = args[0];
            for (int i = 1; i < args.length; i++)
                result = result.divideBy(args[i]);
            return result;
        }
    };

    // ### min
    public static final Primitive MIN = new pf_min();
    private static final class pf_min extends Primitive {
        pf_min() {
            super(Symbol.MIN, "&rest reals");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            if (arg.realp())
                return arg;
            return type_error(arg, Symbol.REAL);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.isLessThan(second) ? first : second;
        }
        @Override
        public LispObject execute(LispObject[] args) {
            LispObject result = args[0];
            if (!result.realp())
                type_error(result, Symbol.REAL);
            for (int i = 1; i < args.length; i++) {
                if (args[i].isLessThan(result))
                    result = args[i];
            }
            return result;
        }
    };

    // ### max
    public static final Primitive MAX = new pf_max();
    private static final class pf_max extends Primitive {
        pf_max() {
            super(Symbol.MAX, "&rest reals");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            if (arg.realp())
                return arg;
            return type_error(arg, Symbol.REAL);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.isGreaterThan(second) ? first : second;
        }
        @Override
        public LispObject execute(LispObject[] args) {
            LispObject result = args[0];
            if (!result.realp())
                type_error(result, Symbol.REAL);
            for (int i = 1; i < args.length; i++) {
                if (args[i].isGreaterThan(result))
                    result = args[i];
            }
            return result;
        }
    };

    // ### identity
    private static final Primitive IDENTITY = new pf_identity();
    private static final class pf_identity extends Primitive {
        pf_identity() {
            super(Symbol.IDENTITY, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg;
        }
    };

    // ### compiled-function-p
    private static final Primitive COMPILED_FUNCTION_P = new pf_compiled_function_p();
    private static final class pf_compiled_function_p extends Primitive {
        pf_compiled_function_p() {
            super(Symbol.COMPILED_FUNCTION_P, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.typep(Symbol.COMPILED_FUNCTION);
        }
    };

    // ### compiled-lisp-function-p
    private static final Primitive COMPILED_LISP_FUNCTION_P =
        new pf_compiled_lisp_function_p();
    private static final class pf_compiled_lisp_function_p extends Primitive {
        pf_compiled_lisp_function_p() {
            super(Symbol.COMPILED_LISP_FUNCTION_P, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return (arg instanceof CompiledClosure
                    || arg instanceof CompiledPrimitive) ? T : NIL;
        }
    }

    // ### consp
    private static final Primitive CONSP = new pf_consp();
    private static final class pf_consp extends Primitive {
        pf_consp() {
            super(Symbol.CONSP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof Cons ? T : NIL;
        }
    };

    // ### listp
    private static final Primitive LISTP = new pf_listp();
    private static final class pf_listp extends Primitive {
        pf_listp() {
            super(Symbol.LISTP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.LISTP();
        }
    };

    // ### abs
    private static final Primitive ABS = new pf_abs();
    private static final class pf_abs extends Primitive {
        pf_abs() {
            super(Symbol.ABS, "number");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.ABS();
        }
    };

    // ### arrayp
    private static final Primitive ARRAYP = new pf_arrayp();
    private static final class pf_arrayp extends Primitive {
        pf_arrayp() {
            super(Symbol.ARRAYP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof AbstractArray ? T : NIL;
        }
    };

    // ### array-has-fill-pointer-p
    private static final Primitive ARRAY_HAS_FILL_POINTER_P = new pf_array_has_fill_pointer_p();
    private static final class pf_array_has_fill_pointer_p extends Primitive {
        pf_array_has_fill_pointer_p() {
            super(Symbol.ARRAY_HAS_FILL_POINTER_P, "array");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkArray(arg).hasFillPointer() ? T : NIL;
        }
    };

    // ### vectorp
    private static final Primitive VECTORP = new pf_vectorp();
    private static final class pf_vectorp extends Primitive {
        pf_vectorp() {
            super(Symbol.VECTORP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.VECTORP();
        }
    };

    // ### simple-vector-p
    private static final Primitive SIMPLE_VECTOR_P = new pf_simple_vector_p();
    private static final class pf_simple_vector_p extends Primitive {
        pf_simple_vector_p() {
            super(Symbol.SIMPLE_VECTOR_P, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof SimpleVector ? T : NIL;
        }
    };

    // ### bit-vector-p
    private static final Primitive BIT_VECTOR_P = new pf_bit_vector_p();
    private static final class pf_bit_vector_p extends Primitive {
        pf_bit_vector_p() {
            super(Symbol.BIT_VECTOR_P, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof AbstractBitVector ? T : NIL;
        }
    };

    // ### simple-bit-vector-p
    private static final Primitive SIMPLE_BIT_VECTOR_P = new pf_simple_bit_vector_p();
    private static final class pf_simple_bit_vector_p extends Primitive {
        pf_simple_bit_vector_p() {
            super(Symbol.SIMPLE_BIT_VECTOR_P, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.typep(Symbol.SIMPLE_BIT_VECTOR);
        }
    };

    // ### %eval
    private static final Primitive _EVAL = new pf__eval();
    private static final class pf__eval extends Primitive {
        pf__eval() {
            super("%eval", PACKAGE_SYS, false, "form");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return eval(arg, new Environment(), LispThread.currentThread());
        }
    };

    // ### eq
    private static final Primitive EQ = new pf_eq();
    private static final class pf_eq extends Primitive {
        pf_eq() {
            super(Symbol.EQ, "x y");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first == second ? T : NIL;
        }
    };

    // ### eql
    static final Primitive EQL = new pf_eql();
    private static final class pf_eql extends Primitive {
        pf_eql() {
            super(Symbol.EQL, "x y");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.eql(second) ? T : NIL;
        }
    };

    // ### equal
    private static final Primitive EQUAL = new pf_equal();
    private static final class pf_equal extends Primitive {
        pf_equal() {
            super(Symbol.EQUAL, "x y");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.equal(second) ? T : NIL;
        }
    };

    // ### equalp
    private static final Primitive EQUALP = new pf_equalp();
    private static final class pf_equalp extends Primitive {
        pf_equalp() {
            super(Symbol.EQUALP, "x y");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.equalp(second) ? T : NIL;
        }
    };

    // ### values
    private static final Primitive VALUES = new pf_values();
    private static final class pf_values extends Primitive {
        pf_values() {
            super(Symbol.VALUES, "&rest object");
        }

        @Override
        public LispObject execute() {
            return LispThread.currentThread().setValues();
        }
        @Override
        public LispObject execute(LispObject arg) {
            return LispThread.currentThread().setValues(arg);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            return LispThread.currentThread().setValues(first, second);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third) {
            return LispThread.currentThread().setValues(first, second, third);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth) {
            return LispThread.currentThread().setValues(first, second, third,
                    fourth);
        }
        @Override
        public LispObject execute(LispObject[] args) {
            return LispThread.currentThread().setValues(args);
        }
    };

    // ### values-list list => element*
    // Returns the elements of the list as multiple values.
    private static final Primitive VALUES_LIST = new pf_values_list();
    private static final class pf_values_list extends Primitive {
        pf_values_list() {
            super(Symbol.VALUES_LIST, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg == NIL)
                return LispThread.currentThread().setValues();
            if (arg.cdr() == NIL)
                return arg.car();
            return LispThread.currentThread().setValues(arg.copyToArray());
        }
    };

    // ### cons
    private static final Primitive CONS = new pf_cons();
    private static final class pf_cons extends Primitive {
        pf_cons() {
            super(Symbol.CONS, "object-1 object-2");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return new Cons(first, second);
        }
    };

    // ### length
    private static final Primitive LENGTH = new pf_length();
    private static final class pf_length extends Primitive {
        pf_length() {
            super("%LENGTH", PACKAGE_SYS, false, "sequence");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.LENGTH();
        }
    };

    // ### elt
    private static final Primitive ELT = new pf_elt();
    private static final class pf_elt extends Primitive {
        pf_elt() {
            super("%ELT", PACKAGE_SYS, false, "sequence index");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.elt(Fixnum.getValue(second));
        }
    };

    // ### atom
    private static final Primitive ATOM = new pf_atom();
    private static final class pf_atom extends Primitive {
        pf_atom() {
            super(Symbol.ATOM, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof Cons ? NIL : T;
        }
    };

    // ### constantp
    private static final Primitive CONSTANTP = new pf_constantp();
    private static final class pf_constantp extends Primitive {
        pf_constantp() {
            super(Symbol.CONSTANTP, "form &optional environment");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.constantp() ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.constantp() ? T : NIL;
        }
    };

    // ### functionp
    private static final Primitive FUNCTIONP = new pf_functionp();
    private static final class pf_functionp extends Primitive {
        pf_functionp() {
            super(Symbol.FUNCTIONP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return (arg instanceof Function || arg instanceof StandardGenericFunction) ? T : NIL;
        }
    };

    // ### special-operator-p
    private static final Primitive SPECIAL_OPERATOR_P = new pf_special_operator_p();
    private static final class pf_special_operator_p extends Primitive {
        pf_special_operator_p() {
            super(Symbol.SPECIAL_OPERATOR_P, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.isSpecialOperator() ? T : NIL;
        }
    };

    // ### symbolp
    private static final Primitive SYMBOLP = new pf_symbolp();
    private static final class pf_symbolp extends Primitive {
        pf_symbolp() {
            super(Symbol.SYMBOLP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof Symbol ? T : NIL;
        }
    };

    // ### endp
    private static final Primitive ENDP = new pf_endp();
    private static final class pf_endp extends Primitive {
        pf_endp() {
            super(Symbol.ENDP, "list");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.endp() ? T : NIL;
        }
    };

    // ### null
    private static final Primitive NULL = new pf_null();
    private static final class pf_null extends Primitive {
        pf_null() {
            super(Symbol.NULL, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg == NIL ? T : NIL;
        }
    };

    // ### not
    private static final Primitive NOT = new pf_not();
    private static final class pf_not extends Primitive {
        pf_not() {
            super(Symbol.NOT, "x");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg == NIL ? T : NIL;
        }
    };

    // ### plusp
    private static final Primitive PLUSP = new pf_plusp();
    private static final class pf_plusp extends Primitive {
        pf_plusp() {
            super(Symbol.PLUSP, "real");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.PLUSP();
        }
    };

    // ### minusp
    private static final Primitive MINUSP = new pf_minusp();
    private static final class pf_minusp extends Primitive {
        pf_minusp() {
            super(Symbol.MINUSP, "real");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.MINUSP();
        }
    };

    // ### zerop
    private static final Primitive ZEROP = new pf_zerop();
    private static final class pf_zerop extends Primitive {
        pf_zerop() {
            super(Symbol.ZEROP, "number");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.ZEROP();
        }
    };

    // ### fixnump
    private static final Primitive FIXNUMP = new pf_fixnump();
    private static final class pf_fixnump extends Primitive {
        pf_fixnump() {
            super("fixnump", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof Fixnum ? T : NIL;
        }
    };

    // ### symbol-value
    private static final Primitive SYMBOL_VALUE = new pf_symbol_value();
    private static final class pf_symbol_value extends Primitive {
        pf_symbol_value() {
            super(Symbol.SYMBOL_VALUE, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            final LispObject value;
            value = checkSymbol(arg).symbolValue();
            if (value instanceof SymbolMacro)
                return error(new LispError(arg.writeToString() +
                                           " has no dynamic value."));
            return value;
        }
    };

    // ### set symbol value => value
    private static final Primitive SET = new pf_set();
    private static final class pf_set extends Primitive {
        pf_set() {
            super(Symbol.SET, "symbol value");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return LispThread.currentThread().setSpecialVariable(checkSymbol(first),
                    second);
        }
    };

    // ### rplaca
    private static final Primitive RPLACA = new pf_rplaca();
    private static final class pf_rplaca extends Primitive {
        pf_rplaca() {
            super(Symbol.RPLACA, "cons object");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            first.setCar(second);
            return first;
        }
    };

    // ### rplacd
    private static final Primitive RPLACD = new pf_rplacd();
    private static final class pf_rplacd extends Primitive {
        pf_rplacd() {
            super(Symbol.RPLACD, "cons object");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            first.setCdr(second);
            return first;
        }
    };

    // ### +
    private static final Primitive ADD = new pf_add();
    private static final class pf_add extends Primitive {
        pf_add() {
            super(Symbol.PLUS, "&rest numbers");
        }

        @Override
        public LispObject execute() {
            return Fixnum.ZERO;
        }
        @Override
        public LispObject execute(LispObject arg) {
            if (arg.numberp())
                return arg;
            return type_error(arg, Symbol.NUMBER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.add(second);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return first.add(second).add(third);
        }
        @Override
        public LispObject execute(LispObject[] args) {
            LispObject result = Fixnum.ZERO;
            final int length = args.length;
            for (int i = 0; i < length; i++)
                result = result.add(args[i]);
            return result;
        }
    };

    // ### 1+
    private static final Primitive ONE_PLUS = new pf_one_plus();
    private static final class pf_one_plus extends Primitive {
        pf_one_plus() {
            super(Symbol.ONE_PLUS, "number");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.incr();
        }
    };

    // ### -
    private static final Primitive SUBTRACT = new pf_subtract();
    private static final class pf_subtract extends Primitive {
        pf_subtract() {
            super(Symbol.MINUS, "minuend &rest subtrahends");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return arg.negate();
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.subtract(second);
        }
        @Override
        public LispObject execute(LispObject[] args) {
            LispObject result = args[0];
            for (int i = 1; i < args.length; i++)
                result = result.subtract(args[i]);
            return result;
        }
    };

    // ### 1-
    private static final Primitive ONE_MINUS = new pf_one_minus();
    private static final class pf_one_minus extends Primitive {
        pf_one_minus() {
            super(Symbol.ONE_MINUS, "number");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.decr();
        }
    };

    // ### when
    private static final SpecialOperator WHEN = new sf_when();
    private static final class sf_when extends SpecialOperator {
        sf_when() {
            super(Symbol.WHEN);
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args == NIL)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            if (eval(args.car(), env, thread) != NIL) {
                args = args.cdr();
                thread.clearValues();
                return progn(args, env, thread);
            }
            return thread.setValues(NIL);
        }
    };

    // ### unless
    private static final SpecialOperator UNLESS = new sf_unless();
    private static final class sf_unless extends SpecialOperator {
        sf_unless() {
            super(Symbol.UNLESS);
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args == NIL)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            if (eval(args.car(), env, thread) == NIL) {
                args = args.cdr();
                thread.clearValues();
                return progn(args, env, thread);
            }
            return thread.setValues(NIL);
        }
    };

    // ### %stream-output-object object stream => object
    private static final Primitive _STREAM_OUTPUT_OBJECT = new pf__stream_output_object();
    private static final class pf__stream_output_object extends Primitive {
        pf__stream_output_object() {
            super("%stream-output-object", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            checkStream(second)._writeString(first.writeToString());
            return first;
        }
    };

    // ### %output-object object stream => object
    private static final Primitive _OUTPUT_OBJECT = new pf__output_object();
    private static final class pf__output_object extends Primitive {
        pf__output_object() {
            super("%output-object", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final LispObject out;
            if (second == T)
                out = Symbol.TERMINAL_IO.symbolValue();
            else if (second == NIL)
                out = Symbol.STANDARD_OUTPUT.symbolValue();
            else
                out = second;
            String output = first.writeToString();
            if (Symbol.PRINT_READABLY.symbolValue(LispThread.currentThread()) != NIL
                && output.contains("#<")) {
                LispObject args = NIL;
                args = args.push(first);
                args = args.push(Keyword.OBJECT);
                args = args.nreverse();
                return error(new PrintNotReadable(args));
            }
            checkStream(out)._writeString(output);
            return first;
        }
    };

    // ### %write-to-string object => string
    private static final Primitive _WRITE_TO_STRING = new pf__write_to_string();
    private static final class pf__write_to_string extends Primitive {
        pf__write_to_string() {
            super("%write-to-string", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return new SimpleString(arg.writeToString());
        }
    };

    // ### %stream-terpri output-stream => nil
    private static final Primitive _STREAM_TERPRI = new pf__stream_terpri();
    private static final class pf__stream_terpri extends Primitive {
        pf__stream_terpri() {
            super("%stream-terpri", PACKAGE_SYS, true, "output-stream");
        }

        @Override
        public LispObject execute(LispObject arg) {
            checkStream(arg)._writeChar('\n');
            return NIL;
        }
    };

    // ### %terpri output-stream => nil
    private static final Primitive _TERPRI = new pf__terpri();
    private static final class pf__terpri extends Primitive {
        pf__terpri() {
            super("%terpri", PACKAGE_SYS, false, "output-stream");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg == T)
                arg = Symbol.TERMINAL_IO.symbolValue();
            else if (arg == NIL)
                arg = Symbol.STANDARD_OUTPUT.symbolValue();
            final Stream stream;
            stream = checkStream(arg);
            return stream.terpri();
        }
    };

    // ### %fresh-line
    // %fresh-line &optional output-stream => generalized-boolean
    private static final Primitive _FRESH_LINE = new pf__fresh_line();
    private static final class pf__fresh_line extends Primitive {
        pf__fresh_line() {
            super("%fresh-line", PACKAGE_SYS, false, "output-stream");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg == T)
                arg = Symbol.TERMINAL_IO.symbolValue();
            else if (arg == NIL)
                arg = Symbol.STANDARD_OUTPUT.symbolValue();
            final Stream stream;
            stream = checkStream(arg);
            return stream.freshLine();
        }
    };

    // ### boundp
    // Determines only whether a symbol has a value in the global environment;
    // any lexical bindings are ignored.
    private static final Primitive BOUNDP = new pf_boundp();
    private static final class pf_boundp extends Primitive {
        pf_boundp() {
            super(Symbol.BOUNDP, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            final Symbol symbol;
            symbol = checkSymbol(arg);
            // PROGV: "If too few values are supplied, the remaining symbols
            // are bound and then made to have no value." So BOUNDP must
            // explicitly check for a binding with no value.
            SpecialBinding binding =
                LispThread.currentThread().getSpecialBinding(symbol);
            if (binding != null)
                return binding.value != null ? T : NIL;
            // No binding.
            return symbol.getSymbolValue() != null ? T : NIL;
        }
    };

    // ### fboundp
    private static final Primitive FBOUNDP = new pf_fboundp();
    private static final class pf_fboundp extends Primitive {
        pf_fboundp() {
            super(Symbol.FBOUNDP, "name");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Symbol)
                return arg.getSymbolFunction() != null ? T : NIL;
            if (isValidSetfFunctionName(arg)) {
                LispObject f = get(arg.cadr(), Symbol.SETF_FUNCTION, null);
                return f != null ? T : NIL;
            }
            return type_error(arg, FUNCTION_NAME);
        }
    };

    // ### fmakunbound name => name
    private static final Primitive FMAKUNBOUND = new pf_fmakunbound();
    private static final class pf_fmakunbound extends Primitive {
        pf_fmakunbound() {
            super(Symbol.FMAKUNBOUND, "name");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Symbol) {
                checkSymbol(arg).setSymbolFunction(null);
                return arg;
            }
            if (isValidSetfFunctionName(arg)) {
                remprop((Symbol)arg.cadr(), Symbol.SETF_FUNCTION);
                return arg;
            }
            return type_error(arg, FUNCTION_NAME);
        }
    };

    // ### setf-function-name-p
    private static final Primitive SETF_FUNCTION_NAME_P = new pf_setf_function_name_p();
    private static final class pf_setf_function_name_p extends Primitive {
        pf_setf_function_name_p() {
            super("setf-function-name-p", PACKAGE_SYS, true, "thing");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return isValidSetfFunctionName(arg) ? T : NIL;
        }
    };

    // ### remprop
    private static final Primitive REMPROP = new pf_remprop();
    private static final class pf_remprop extends Primitive {
        pf_remprop() {
            super(Symbol.REMPROP, "symbol indicator");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return remprop(checkSymbol(first), second);
        }
    };

    // ### append
    public static final Primitive APPEND = new pf_append();
    private static final class pf_append extends Primitive {
        pf_append() {
            super(Symbol.APPEND, "&rest lists");
        }

        @Override
        public LispObject execute() {
            return NIL;
        }
        @Override
        public LispObject execute(LispObject arg) {
            return arg;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first == NIL)
                return second;
            // APPEND is required to copy its first argument.
            Cons result = new Cons(first.car());
            Cons splice = result;
            first = first.cdr();
            while (first != NIL) {
                Cons temp = new Cons(first.car());
                splice.cdr = temp;
                splice = temp;
                first = first.cdr();
            }
            splice.cdr = second;
            return result;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first == NIL)
                return execute(second, third);
            Cons result = new Cons(first.car());
            Cons splice = result;
            first = first.cdr();
            while (first != NIL) {
                Cons temp = new Cons(first.car());
                splice.cdr = temp;
                splice = temp;
                first = first.cdr();
            }
            while (second != NIL) {
                Cons temp = new Cons(second.car());
                splice.cdr = temp;
                splice = temp;
                second = second.cdr();
            }
            splice.cdr = third;
            return result;
        }
        @Override
        public LispObject execute(LispObject[] args) {
            Cons result = null;
            Cons splice = null;
            final int limit = args.length - 1;
            int i;
            for (i = 0; i < limit; i++) {
                LispObject top = args[i];
                if (top == NIL)
                    continue;
                result = new Cons(top.car());
                splice = result;
                top = top.cdr();
                while (top != NIL) {
                    Cons temp = new Cons(top.car());
                    splice.cdr = temp;
                    splice = temp;
                    top = top.cdr();
                }
                break;
            }
            if (result == null)
                return args[i];
            for (++i; i < limit; i++) {
                LispObject top = args[i];
                while (top != NIL) {
                    Cons temp = new Cons(top.car());
                    splice.cdr = temp;
                    splice = temp;
                    top = top.cdr();
                }
            }
            splice.cdr = args[i];
            return result;
        }
    };

    // ### nconc
    private static final Primitive NCONC = new pf_nconc();
    private static final class pf_nconc extends Primitive {
        pf_nconc() {
            super(Symbol.NCONC, "&rest lists");
        }

        @Override
        public LispObject execute() {
            return NIL;
        }
        @Override
        public LispObject execute(LispObject arg) {
            return arg;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first == NIL)
                return second;
            if (first instanceof Cons) {
                LispObject result = first;
                Cons splice = null;
                while (first instanceof Cons) {
                    splice = (Cons) first;
                    first = splice.cdr;
                }
                splice.cdr = second;
                return result;
            }
            return type_error(first, Symbol.LIST);
        }
        @Override
        public LispObject execute(LispObject[] array) {
            LispObject result = null;
            Cons splice = null;
            final int limit = array.length - 1;
            int i;
            for (i = 0; i < limit; i++) {
                LispObject list = array[i];
                if (list == NIL)
                    continue;
                if (list instanceof Cons) {
                    if (splice != null) {
                        splice.cdr = list;
                        splice = (Cons) list;
                    }
                    while (list instanceof Cons) {
                        if (result == null) {
                            result = list;
                            splice = (Cons) result;
                        } else
                            splice = (Cons) list;
                        list = splice.cdr;
                    }
                } else
                    type_error(list, Symbol.LIST);
            }
            if (result == null)
                return array[i];
            splice.cdr = array[i];
            return result;
        }
    };

    // ### =
    // Numeric equality.
    private static final Primitive EQUALS = new pf_equals();
    private static final class pf_equals extends Primitive {
        pf_equals() {
            super(Symbol.EQUALS, "&rest numbers");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return T;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.isEqualTo(second) ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first.isEqualTo(second) && second.isEqualTo(third))
                return T;
            else
                return NIL;
        }
        @Override
        public LispObject execute(LispObject[] array) {
            final int length = array.length;
            final LispObject obj = array[0];
            for (int i = 1; i < length; i++) {
                if (array[i].isNotEqualTo(obj))
                    return NIL;
            }
            return T;
        }
    };

    // ### /=
    // Returns true if no two numbers are the same; otherwise returns false.
    private static final Primitive NOT_EQUALS = new pf_not_equals();
    private static final class pf_not_equals extends Primitive {
        pf_not_equals() {
            super(Symbol.NOT_EQUALS, "&rest numbers");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return T;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.isNotEqualTo(second) ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first.isEqualTo(second))
                return NIL;
            if (first.isEqualTo(third))
                return NIL;
            if (second.isEqualTo(third))
                return NIL;
            return T;
        }
        @Override
        public LispObject execute(LispObject[] array) {
            final int length = array.length;
            for (int i = 0; i < length; i++) {
                final LispObject obj = array[i];
                for (int j = i+1; j < length; j++) {
                    if (array[j].isEqualTo(obj))
                        return NIL;
                }
            }
            return T;
        }
    };

    // ### <
    // Numeric comparison.
    private static final Primitive LT = new pf_lt();
    private static final class pf_lt extends Primitive {
        pf_lt() {
            super(Symbol.LT, "&rest numbers");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return T;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.isLessThan(second) ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first.isLessThan(second) && second.isLessThan(third))
                return T;
            else
                return NIL;
        }
        @Override
        public LispObject execute(LispObject[] array) {
            final int length = array.length;
            for (int i = 1; i < length; i++) {
                if (array[i].isLessThanOrEqualTo(array[i-1]))
                    return NIL;
            }
            return T;
        }
    };

    // ### <=
    private static final Primitive LE = new pf_le();
    private static final class pf_le extends Primitive {
        pf_le() {
            super(Symbol.LE, "&rest numbers");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return T;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.isLessThanOrEqualTo(second) ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first.isLessThanOrEqualTo(second) && second.isLessThanOrEqualTo(third))
                return T;
            else
                return NIL;
        }
        @Override
        public LispObject execute(LispObject[] array) {
            final int length = array.length;
            for (int i = 1; i < length; i++) {
                if (array[i].isLessThan(array[i-1]))
                    return NIL;
            }
            return T;
        }
    };

    // ### >
    private static final Primitive GT = new pf_gt();
    private static final class pf_gt extends Primitive {
        pf_gt() {
            super(Symbol.GT, "&rest numbers");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return T;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.isGreaterThan(second) ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first.isGreaterThan(second) && second.isGreaterThan(third))
                return T;
            else
                return NIL;
        }
        @Override
        public LispObject execute(LispObject[] array) {
            final int length = array.length;
            for (int i = 1; i < length; i++) {
                if (array[i].isGreaterThanOrEqualTo(array[i-1]))
                    return NIL;
            }
            return T;
        }
    };

    // ### >=
    private static final Primitive GE = new pf_ge();
    private static final class pf_ge extends Primitive {
        pf_ge() {
            super(Symbol.GE, "&rest numbers");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return T;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.isGreaterThanOrEqualTo(second) ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first.isGreaterThanOrEqualTo(second) && second.isGreaterThanOrEqualTo(third))
                return T;
            else
                return NIL;
        }
        @Override
        public LispObject execute(LispObject[] array) {
            final int length = array.length;
            for (int i = 1; i < length; i++) {
                if (array[i].isGreaterThan(array[i-1]))
                    return NIL;
            }
            return T;
        }
    };

    // ### nth n list => object
    private static final Primitive NTH = new pf_nth();
    private static final class pf_nth extends Primitive {
        pf_nth() {
            super(Symbol.NTH, "n list");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return second.NTH(first);
        }
    };

    // ### %set-nth n list new-object => new-object
    private static final Primitive _SET_NTH = new pf__set_nth();
    private static final class pf__set_nth extends Primitive {
        pf__set_nth() {
            super("%set-nth", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            int index = Fixnum.getValue(first);
            if (index < 0)
                error(new TypeError("(SETF NTH): invalid index " + index + "."));
            int i = 0;
            while (true) {
                if (i == index) {
                    second.setCar(third);
                    return third;
                }
                second = second.cdr();
                if (second == NIL) {
                    return error(new LispError("(SETF NTH): the index " +
                                               index + "is too large."));
                }
                ++i;
            }
        }
    };

    // ### nthcdr
    private static final Primitive NTHCDR = new pf_nthcdr();
    private static final class pf_nthcdr extends Primitive {
        pf_nthcdr() {
            super(Symbol.NTHCDR, "n list");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final int index = Fixnum.getValue(first);
            if (index < 0)
                return type_error(first,
                                  list(Symbol.INTEGER, Fixnum.ZERO));
            for (int i = 0; i < index; i++) {
                second = second.cdr();
                if (second == NIL)
                    return NIL;
            }
            return second;
        }
    };

    /** Stub to be replaced later when signal.lisp has been loaded. */
    // ### error
    private static final Primitive ERROR = new pf_error();
    private static final class pf_error extends Primitive {
        pf_error() {
            super(Symbol.ERROR, "datum &rest arguments");
        }

        @Override
        @SuppressWarnings("CallToThreadDumpStack")
        public LispObject execute(LispObject[] args) {
            Error e = new IntegrityError();

            e.printStackTrace();

            System.out.println("ERROR placeholder called with arguments:");

            if (args.length == 1 && args[0] instanceof Condition) {
                System.out.println(args[0].writeToString());
                System.out.println(((Condition)args[0]).getConditionReport());
            } else
            for (LispObject a : args)
                System.out.println(a.writeToString());

            throw e;
        }
    };

    /** Stub replaced when compiler-pass2.lisp has been loaded */
    // ### autocompile
    private static final Primitive AUTOCOMPILE = new pf_autocompile();
    private static final class pf_autocompile extends Primitive {
        pf_autocompile() {
            super(Symbol.AUTOCOMPILE, "function");
        }

        @Override
        public LispObject execute(LispObject function) {
            return NIL;
        }
    };

    // ### signal
    /** Placeholder function, to be replaced by the function
     * defined in signal.lisp
     *
     * Calling this function is an error: we're not set up for
     * signalling yet.
     */
    private static final Primitive SIGNAL = new pf_signal();
    private static final class pf_signal extends Primitive {
        pf_signal() {
            super(Symbol.SIGNAL, "datum &rest arguments");
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length < 1)
                return error(new WrongNumberOfArgumentsException(this));
            if (args[0] instanceof Condition)
                return error((Condition)args[0]);
            return error(new SimpleCondition());
        }
    };

    // ### undefined-function-called
    // Redefined in restart.lisp.
    private static final Primitive UNDEFINED_FUNCTION_CALLED = new pf_undefined_function_called();
    private static final class pf_undefined_function_called extends Primitive {
        pf_undefined_function_called() {
            super(Symbol.UNDEFINED_FUNCTION_CALLED, "name arguments");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return error(new UndefinedFunction(first));
        }
    };

    // ### %format
    private static final Primitive _FORMAT = new pf__format();
    private static final class pf__format extends Primitive {
        pf__format() {
            super("%format", PACKAGE_SYS, false,
                  "destination control-string &rest args");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            LispObject destination = first;
            // Copy remaining arguments.
            LispObject[] _args = new LispObject[2];
            _args[0] = second;
            _args[1] = third;
            String s = _format(_args);
            return outputFormattedString(s, destination);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth)

        {
            LispObject destination = first;
            // Copy remaining arguments.
            LispObject[] _args = new LispObject[3];
            _args[0] = second;
            _args[1] = third;
            _args[2] = fourth;
            String s = _format(_args);
            return outputFormattedString(s, destination);
        }
        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length < 2)
                return error(new WrongNumberOfArgumentsException(this));
            LispObject destination = args[0];
            // Copy remaining arguments.
            LispObject[] _args = new LispObject[args.length - 1];
            for (int i = 0; i < _args.length; i++)
                _args[i] = args[i+1];
            String s = _format(_args);
            return outputFormattedString(s, destination);
        }
        private final String _format(LispObject[] args)

        {
            LispObject formatControl = args[0];
            LispObject formatArguments = NIL;
            for (int i = 1; i < args.length; i++)
                formatArguments = new Cons(args[i], formatArguments);
            formatArguments = formatArguments.nreverse();
            return format(formatControl, formatArguments);
        }
        private final LispObject outputFormattedString(String s,
                LispObject destination)

        {
            if (destination == T) {
                checkCharacterOutputStream(Symbol.STANDARD_OUTPUT.symbolValue())._writeString(s);
                return NIL;
            }
            if (destination == NIL)
                return new SimpleString(s);
            if (destination instanceof TwoWayStream) {
                Stream out = ((TwoWayStream)destination).getOutputStream();
                if (out instanceof Stream) {
                    (out)._writeString(s);
                    return NIL;
                }
                error(new TypeError("The value " +
                                    destination.writeToString() +
                                    " is not a character output stream."));
            }
            if (destination instanceof Stream) {
                ((Stream)destination)._writeString(s);
                return NIL;
            }
            return NIL;
        }
    };

    static void checkRedefinition(LispObject arg)
    {
        final LispThread thread = LispThread.currentThread();
        if (_WARN_ON_REDEFINITION_.symbolValue(thread) != NIL) {
            if (arg instanceof Symbol) {
                LispObject oldDefinition = arg.getSymbolFunction();
                if (oldDefinition != null
                        && !(oldDefinition instanceof Autoload)
                        && !(oldDefinition instanceof AutoloadedFunctionProxy)) {
                    LispObject oldSource =
                        Extensions.SOURCE_PATHNAME.execute(arg);
                    LispObject currentSource = _SOURCE_.symbolValue(thread);
                    if (currentSource == NIL)
                        currentSource = Keyword.TOP_LEVEL;
                    if (oldSource != NIL) {
                        if (currentSource.equal(oldSource))
                            return; // OK
                    }
                    if (currentSource == Keyword.TOP_LEVEL) {
                        Symbol.STYLE_WARN.execute(new SimpleString("redefining ~S at top level"),
                                                  arg);

                    } else {
                        SpecialBindingsMark mark = thread.markSpecialBindings();
                        thread.bindSpecial(Symbol._PACKAGE_, PACKAGE_CL);
                        try {
                            Symbol.STYLE_WARN.execute(new SimpleString("redefining ~S in ~S"),
                                                      arg, currentSource);
                        }
                        finally {
                            thread.resetSpecialBindings(mark);
                        }
                    }
                }
            }
        }
    }

    // ### %defun name definition => name
    private static final Primitive _DEFUN = new pf__defun();
    private static final class pf__defun extends Primitive {
        pf__defun() {
            super("%defun", PACKAGE_SYS, true, "name definition");
        }

        @Override
        public LispObject execute(LispObject name, LispObject definition)

        {
            if (name instanceof Symbol) {
                Symbol symbol = (Symbol) name;
                if (symbol.getSymbolFunction() instanceof SpecialOperator) {
                    String message =
                        symbol.getName() + " is a special operator and may not be redefined.";
                    return error(new ProgramError(message));
                }
            } else if (!isValidSetfFunctionName(name))
                return type_error(name, FUNCTION_NAME);
            if (definition instanceof Function) {
                Symbol.FSET.execute(name, definition, NIL,
                                    ((Function)definition).getLambdaList());
                return name;
            }
            return type_error(definition, Symbol.FUNCTION);
        }
    };

    // ### fdefinition-block-name
    private static final Primitive FDEFINITION_BLOCK_NAME = new pf_fdefinition_block_name();
    private static final class pf_fdefinition_block_name extends Primitive {
        pf_fdefinition_block_name() {
            super("fdefinition-block-name", PACKAGE_SYS, true, "function-name");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Symbol)
                return arg;
            if (isValidSetfFunctionName(arg))
                return arg.cadr();
            return type_error(arg, FUNCTION_NAME);
        }
    };

    // ### macro-function
    private static final Primitive MACRO_FUNCTION = new pf_macro_function();
    private static final class pf_macro_function extends Primitive {
        pf_macro_function() {
            super(Symbol.MACRO_FUNCTION, "symbol &optional environment");
        }

        @Override
        public LispObject execute(LispObject arg) {
            LispObject obj = arg.getSymbolFunction();
            if (obj instanceof AutoloadMacro) {
                ((AutoloadMacro)obj).load();
                obj = arg.getSymbolFunction();
            }
            if (obj instanceof MacroObject)
                return ((MacroObject)obj).expander;
            if (obj instanceof SpecialOperator) {
                obj = get(arg, Symbol.MACROEXPAND_MACRO, NIL);
                if (obj instanceof AutoloadMacro) {
                    ((AutoloadMacro)obj).load();
                    obj = get(arg, Symbol.MACROEXPAND_MACRO, NIL);
                }
                if (obj instanceof MacroObject)
                    return ((MacroObject)obj).expander;
            }
            return NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            LispObject obj;
            if (second != NIL) {
                Environment env = checkEnvironment(second);
                obj = env.lookupFunction(first);
            } else
                obj = first.getSymbolFunction();
            if (obj instanceof AutoloadMacro) {
                ((AutoloadMacro)obj).load();
                obj = first.getSymbolFunction();
            }
            if (obj instanceof MacroObject)
                return ((MacroObject)obj).expander;
            if (obj instanceof SpecialOperator) {
                obj = get(first, Symbol.MACROEXPAND_MACRO, NIL);
                if (obj instanceof AutoloadMacro) {
                    ((AutoloadMacro)obj).load();
                    obj = get(first, Symbol.MACROEXPAND_MACRO, NIL);
                }
                if (obj instanceof MacroObject)
                    return ((MacroObject)obj).expander;
            }
            return NIL;
        }
    };

    // ### defmacro
    private static final SpecialOperator DEFMACRO = new sf_defmacro();
    private static final class sf_defmacro extends SpecialOperator {
        sf_defmacro() {
            super(Symbol.DEFMACRO);
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            Symbol symbol = checkSymbol(args.car());
            LispObject lambdaList = checkList(args.cadr());
            LispObject body = args.cddr();
            LispObject block = new Cons(Symbol.BLOCK, new Cons(symbol, body));
            LispObject toBeApplied =
                list(Symbol.FUNCTION, list(Symbol.LAMBDA, lambdaList, block));
            final LispThread thread = LispThread.currentThread();
            LispObject formArg = gensym("FORM-", thread);
            LispObject envArg = gensym("ENV-", thread); // Ignored.
            LispObject expander =
                list(Symbol.LAMBDA, list(formArg, envArg),
                     list(Symbol.APPLY, toBeApplied,
                          list(Symbol.CDR, formArg)));
            Closure expansionFunction = new Closure(expander, env);
            MacroObject macroObject =
                new MacroObject(symbol, expansionFunction);
            if (symbol.getSymbolFunction() instanceof SpecialOperator)
                put(symbol, Symbol.MACROEXPAND_MACRO, macroObject);
            else
                symbol.setSymbolFunction(macroObject);
            macroObject.setLambdaList(lambdaList);
            thread._values = null;
            return symbol;
        }
    };

    // ### make-macro
    private static final Primitive MAKE_MACRO = new pf_make_macro();
    private static final class pf_make_macro extends Primitive {
        pf_make_macro() {
            super("make-macro", PACKAGE_SYS, true, "name expansion-function");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return new MacroObject(first, second);
        }
    };

    // ### macro-function-p
    private static final Primitive MACRO_FUNCTION_P = new pf_macro_function_p();
    private static final class pf_macro_function_p extends Primitive {
        pf_macro_function_p() {
            super("macro-function-p", PACKAGE_SYS, true, "value");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return (arg instanceof MacroObject) ? T : NIL;
        }
    };


    // ### make-symbol-macro
    private static final Primitive MAKE_SYMBOL_MACRO = new pf_make_symbol_macro();
    private static final class pf_make_symbol_macro extends Primitive {
        pf_make_symbol_macro() {
            super("make-symbol-macro", PACKAGE_SYS, true, "expansion");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return new SymbolMacro(arg);
        }
    };

    // ### symbol-macro-p
    private static final Primitive SYMBOL_MACRO_P = new pf_symbol_macro_p();
    private static final class pf_symbol_macro_p extends Primitive {
        pf_symbol_macro_p() {
            super("symbol-macro-p", PACKAGE_SYS, true, "value");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return (arg instanceof SymbolMacro) ? T : NIL;
        }
    };

    // ### %defparameter
    private static final Primitive _DEFPARAMETER = new pf__defparameter();
    private static final class pf__defparameter extends Primitive {
        pf__defparameter() {
            super("%defparameter", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            final Symbol symbol;
            symbol = checkSymbol(first);
            if (third instanceof AbstractString)
                symbol.setDocumentation(Symbol.VARIABLE, third);
            else if (third != NIL)
                type_error(third, Symbol.STRING);
            symbol.initializeSpecial(second);
            return symbol;
        }
    };

    // ### %defvar
    private static final Primitive _DEFVAR = new pf__defvar();
    private static final class pf__defvar extends Primitive {
        pf__defvar() {
            super("%defvar", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject arg) {
            final Symbol symbol;
            symbol = checkSymbol(arg);
            symbol.setSpecial(true);
            return symbol;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final Symbol symbol;
            symbol = checkSymbol(first);
            symbol.initializeSpecial(second);
            return symbol;
        }
    };

    // ### %defconstant name initial-value documentation => name
    private static final Primitive _DEFCONSTANT = new pf__defconstant();
    private static final class pf__defconstant extends Primitive {
        pf__defconstant() {
            super("%defconstant", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            final Symbol symbol;
            symbol = checkSymbol(first);
            if (third != NIL) {
                if (third instanceof AbstractString)
                    symbol.setDocumentation(Symbol.VARIABLE, third);
                else
                    return type_error(third, Symbol.STRING);
            }
            symbol.initializeConstant(second);
            return symbol;
        }
    };

    // ### cond
    private static final SpecialOperator COND = new sf_cond();
    private static final class sf_cond extends SpecialOperator {
        sf_cond() {
            super(Symbol.COND, "&rest clauses");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject result = NIL;
            while (args != NIL) {
                LispObject clause = args.car();
                result = eval(clause.car(), env, thread);
                thread._values = null;
                if (result != NIL) {
                    LispObject body = clause.cdr();
                    while (body != NIL) {
                        result = eval(body.car(), env, thread);
                        body = ((Cons)body).cdr;
                    }
                    return result;
                }
                args = ((Cons)args).cdr;
            }
            return result;
        }
    };

    // ### case
    private static final SpecialOperator CASE = new sf_case();
    private static final class sf_case extends SpecialOperator {
        sf_case() {
            super(Symbol.CASE, "keyform &body cases");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject key = eval(args.car(), env, thread);
            args = args.cdr();
            while (args != NIL) {
                LispObject clause = args.car();
                LispObject keys = clause.car();
                boolean match = false;
                if (keys.listp()) {
                    while (keys != NIL) {
                        LispObject candidate = keys.car();
                        if (key.eql(candidate)) {
                            match = true;
                            break;
                        }
                        keys = keys.cdr();
                    }
                } else {
                    LispObject candidate = keys;
                    if (candidate == T || candidate == Symbol.OTHERWISE)
                        match = true;
                    else if (key.eql(candidate))
                        match = true;
                }
                if (match) {
                    return progn(clause.cdr(), env, thread);
                }
                args = args.cdr();
            }
            return NIL;
        }
    };

    // ### ecase
    private static final SpecialOperator ECASE = new sf_ecase();
    private static final class sf_ecase extends SpecialOperator {
        sf_ecase() {
            super(Symbol.ECASE, "keyform &body cases");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject key = eval(args.car(), env, thread);
            LispObject clauses = args.cdr();
            while (clauses != NIL) {
                LispObject clause = clauses.car();
                LispObject keys = clause.car();
                boolean match = false;
                if (keys.listp()) {
                    while (keys != NIL) {
                        LispObject candidate = keys.car();
                        if (key.eql(candidate)) {
                            match = true;
                            break;
                        }
                        keys = keys.cdr();
                    }
                } else {
                    LispObject candidate = keys;
                    if (key.eql(candidate))
                        match = true;
                }
                if (match) {
                    return progn(clause.cdr(), env, thread);
                }
                clauses = clauses.cdr();
            }
            LispObject expectedType = NIL;
            clauses = args.cdr();
            while (clauses != NIL) {
                LispObject clause = clauses.car();
                LispObject keys = clause.car();
                if (keys.listp()) {
                    while (keys != NIL) {
                        expectedType = expectedType.push(keys.car());
                        keys = keys.cdr();
                    }
                } else
                    expectedType = expectedType.push(keys);
                clauses = clauses.cdr();
            }
            expectedType = expectedType.nreverse();
            expectedType = expectedType.push(Symbol.MEMBER);
            return type_error(key, expectedType);
        }
    };

    // ### upgraded-array-element-type typespec &optional environment
    // => upgraded-typespec
    private static final Primitive UPGRADED_ARRAY_ELEMENT_TYPE = new pf_upgraded_array_element_type();
    private static final class pf_upgraded_array_element_type extends Primitive {
        pf_upgraded_array_element_type() {
            super(Symbol.UPGRADED_ARRAY_ELEMENT_TYPE,
                  "typespec &optional environment");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return getUpgradedArrayElementType(arg);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            // Ignore environment.
            return getUpgradedArrayElementType(first);
        }
    };

    // ### array-rank array => rank
    private static final Primitive ARRAY_RANK = new pf_array_rank();
    private static final class pf_array_rank extends Primitive {
        pf_array_rank() {
            super(Symbol.ARRAY_RANK, "array");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return Fixnum.getInstance(checkArray(arg).getRank());

        }
    };

    // ### array-dimensions array => dimensions
    // Returns a list of integers. Fill pointer (if any) is ignored.
    private static final Primitive ARRAY_DIMENSIONS = new pf_array_dimensions();
    private static final class pf_array_dimensions extends Primitive {
        pf_array_dimensions() {
            super(Symbol.ARRAY_DIMENSIONS, "array");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkArray(arg).getDimensions();
        }
    };

    // ### array-dimension array axis-number => dimension
    private static final Primitive ARRAY_DIMENSION = new pf_array_dimension();
    private static final class pf_array_dimension extends Primitive {
        pf_array_dimension() {
            super(Symbol.ARRAY_DIMENSION, "array axis-number");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final AbstractArray array = checkArray(first);
            return Fixnum.getInstance(array.getDimension(Fixnum.getValue(second)));
        }
    };

    // ### array-total-size array => size
    private static final Primitive ARRAY_TOTAL_SIZE = new pf_array_total_size();
    private static final class pf_array_total_size extends Primitive {
        pf_array_total_size() {
            super(Symbol.ARRAY_TOTAL_SIZE, "array");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return Fixnum.getInstance(checkArray(arg).getTotalSize());
        }
    };


    // ### array-element-type
    // array-element-type array => typespec
    private static final Primitive ARRAY_ELEMENT_TYPE = new pf_array_element_type();
    private static final class pf_array_element_type extends Primitive {
        pf_array_element_type() {
            super(Symbol.ARRAY_ELEMENT_TYPE, "array");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkArray(arg).getElementType();
        }
    };

    // ### adjustable-array-p
    private static final Primitive ADJUSTABLE_ARRAY_P = new pf_adjustable_array_p();
    private static final class pf_adjustable_array_p extends Primitive {
        pf_adjustable_array_p() {
            super(Symbol.ADJUSTABLE_ARRAY_P, "array");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkArray(arg).isAdjustable() ? T : NIL;
        }
    };

    // ### array-displacement array => displaced-to, displaced-index-offset
    private static final Primitive ARRAY_DISPLACEMENT = new pf_array_displacement();
    private static final class pf_array_displacement extends Primitive {
        pf_array_displacement() {
            super(Symbol.ARRAY_DISPLACEMENT, "array");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkArray(arg).arrayDisplacement();

        }
    };

    // ### array-in-bounds-p array &rest subscripts => generalized-boolean
    private static final Primitive ARRAY_IN_BOUNDS_P = new pf_array_in_bounds_p();
    private static final class pf_array_in_bounds_p extends Primitive {
        pf_array_in_bounds_p() {
            super(Symbol.ARRAY_IN_BOUNDS_P, "array &rest subscripts");
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length < 1)
                return error(new WrongNumberOfArgumentsException(this));
            final AbstractArray array;
            LispObject r = args[0];
            array = checkArray(r);
            int rank = array.getRank();
            if (rank != args.length - 1) {
                StringBuilder sb =
                    new StringBuilder("ARRAY-IN-BOUNDS-P: ");
                sb.append("wrong number of subscripts (");
                sb.append(args.length - 1);
                sb.append(") for array of rank ");
                sb.append(rank);
                error(new ProgramError(sb.toString()));
            }
            for (int i = 0; i < rank; i++) {
                LispObject arg = args[i+1];
                if (arg instanceof Fixnum) {
                    int subscript = ((Fixnum)arg).value;
                    if (subscript < 0 || subscript >= array.getDimension(i))
                        return NIL;
                } else if (arg instanceof Bignum)
                    return NIL;
                else
                    type_error(arg, Symbol.INTEGER);
            }
            return T;
        }
    };

    // ### %array-row-major-index array subscripts => index
    private static final Primitive _ARRAY_ROW_MAJOR_INDEX = new pf__array_row_major_index();
    private static final class pf__array_row_major_index extends Primitive {
        pf__array_row_major_index() {
            super("%array-row-major-index", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final AbstractArray array;
            array = checkArray(first);
            LispObject[] subscripts = second.copyToArray();
            return number(array.getRowMajorIndex(subscripts));
        }
    };

    // ### aref array &rest subscripts => element
    private static final Primitive AREF = new pf_aref();
    private static final class pf_aref extends Primitive {
        pf_aref() {
            super(Symbol.AREF, "array &rest subscripts");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            final AbstractArray array;
            array = checkArray( arg);
            if (array.getRank() == 0)
                return array.AREF(0);
            StringBuilder sb =
                new StringBuilder("Wrong number of subscripts (0) for array of rank ");
            sb.append(array.getRank());
            sb.append('.');
            return error(new ProgramError(sb.toString()));
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.AREF(second);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return checkArray(first).get(new int[] {Fixnum.getValue(second),Fixnum.getValue(third)} );
        }
        @Override
        public LispObject execute(LispObject[] args) {
            final AbstractArray array = checkArray(args[0]);
            final int[] subs = new int[args.length - 1];
            for (int i = subs.length; i-- > 0;) {
                subs[i] = Fixnum.getValue(args[i+1]);
            }
            return array.get(subs);
        }
    };

    // ### aset array subscripts new-element => new-element
    private static final Primitive ASET = new pf_aset();
    private static final class pf_aset extends Primitive {
        pf_aset() {
            super("aset", PACKAGE_SYS, true,
                  "array subscripts new-element");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            // Rank zero array.
            final ZeroRankArray array;
            if (first instanceof ZeroRankArray) {
                array = (ZeroRankArray) first;
            } else {
                return error(new TypeError("The value " +
                                           first.writeToString() +
                                           " is not an array of rank 0."));
            }
            array.aset(0, second);
            return second;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            first.aset(second, third);
            return third;
        }
        @Override
        public LispObject execute(LispObject[] args) {
            final AbstractArray array = checkArray(args[0]);
            final int nsubs = args.length - 2;
            final int[] subs = new int[nsubs];
            for (int i = nsubs; i-- > 0;)
                subs[i] = Fixnum.getValue(args[i+1]);
            final LispObject newValue = args[args.length - 1];
            array.set(subs, newValue);
            return newValue;
        }
    };

    // ### row-major-aref array index => element
    private static final Primitive ROW_MAJOR_AREF = new pf_row_major_aref();
    private static final class pf_row_major_aref extends Primitive {
        pf_row_major_aref() {
            super(Symbol.ROW_MAJOR_AREF, "array index");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return checkArray(first).AREF(Fixnum.getValue(second));
        }
    };

    // ### vector
    private static final Primitive VECTOR = new pf_vector();
    private static final class pf_vector extends Primitive {
        pf_vector() {
            super(Symbol.VECTOR, "&rest objects");
        }

        @Override
        public LispObject execute(LispObject[] args) {
            return new SimpleVector(args);
        }
    };

    // ### fill-pointer
    private static final Primitive FILL_POINTER = new pf_fill_pointer();
    private static final class pf_fill_pointer extends Primitive {
        pf_fill_pointer() {
            super(Symbol.FILL_POINTER, "vector");
        }

        @Override
        public LispObject execute(LispObject arg)

        {
            if (arg instanceof AbstractArray) {
                AbstractArray aa = (AbstractArray)arg;
                if (aa.hasFillPointer())
                    return Fixnum.getInstance(aa.getFillPointer());
            }
            return type_error(arg, list(Symbol.AND, Symbol.VECTOR,
                                        list(Symbol.SATISFIES,
                                             Symbol.ARRAY_HAS_FILL_POINTER_P)));
        }
    };

    // ### %set-fill-pointer vector new-fill-pointer
    private static final Primitive _SET_FILL_POINTER = new pf__set_fill_pointer();
    private static final class pf__set_fill_pointer extends Primitive {
        pf__set_fill_pointer() {
            super("%set-fill-pointer", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {

            if (first instanceof AbstractVector) {
                AbstractVector v = (AbstractVector) first;
                if (v.hasFillPointer())
                    v.setFillPointer(second);
                else
                    v.noFillPointer();
                return second;
            }

            return type_error(first, list(Symbol.AND, Symbol.VECTOR,
                                          list(Symbol.SATISFIES,
                                               Symbol.ARRAY_HAS_FILL_POINTER_P)));
        }
    };

    // ### vector-push new-element vector => index-of-new-element
    private static final Primitive VECTOR_PUSH = new pf_vector_push();
    private static final class pf_vector_push extends Primitive {
        pf_vector_push() {
            super(Symbol.VECTOR_PUSH, "new-element vector");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final AbstractVector v = checkVector(second);
            int fillPointer = v.getFillPointer();
            if (fillPointer < 0)
                v.noFillPointer();
            if (fillPointer >= v.capacity())
                return NIL;
            v.aset(fillPointer, first);
            v.setFillPointer(fillPointer + 1);
            return Fixnum.getInstance(fillPointer);
        }
    };

    // ### vector-push-extend new-element vector &optional extension
    // => index-of-new-element
    private static final Primitive VECTOR_PUSH_EXTEND = new pf_vector_push_extend();
    private static final class pf_vector_push_extend extends Primitive {
        pf_vector_push_extend() {
            super(Symbol.VECTOR_PUSH_EXTEND,
                  "new-element vector &optional extension");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return second.VECTOR_PUSH_EXTEND(first);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return second.VECTOR_PUSH_EXTEND(first, third);
        }
    };

    // ### vector-pop vector => element
    private static final Primitive VECTOR_POP = new pf_vector_pop();
    private static final class pf_vector_pop extends Primitive {
        pf_vector_pop() {
            super(Symbol.VECTOR_POP, "vector");
        }

        @Override
        public LispObject execute(LispObject arg) {
            final AbstractVector v = checkVector( arg);
            int fillPointer = v.getFillPointer();
            if (fillPointer < 0)
                v.noFillPointer();
            if (fillPointer == 0)
                error(new LispError("nothing left to pop"));
            int newFillPointer = v.checkIndex(fillPointer - 1);
            LispObject element = v.AREF(newFillPointer);
            v.setFillPointer(newFillPointer);
            return element;
        }
    };

    // ### type-of
    private static final Primitive TYPE_OF = new pf_type_of();
    private static final class pf_type_of extends Primitive {
        pf_type_of() {
            super(Symbol.TYPE_OF, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.typeOf();
        }
    };

    // ### class-of
    private static final Primitive CLASS_OF = new pf_class_of();
    private static final class pf_class_of extends Primitive {
        pf_class_of() {
            super(Symbol.CLASS_OF, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.classOf();
        }
    };

    // ### simple-typep
    private static final Primitive SIMPLE_TYPEP = new pf_simple_typep();
    private static final class pf_simple_typep extends Primitive {
        pf_simple_typep() {
            super("simple-typep", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.typep(second);
        }
    };

    // ### function-lambda-expression function =>
    // lambda-expression, closure-p, name
    private static final Primitive FUNCTION_LAMBDA_EXPRESSION = new pf_function_lambda_expression();
    private static final class pf_function_lambda_expression extends Primitive {
        pf_function_lambda_expression() {
            super(Symbol.FUNCTION_LAMBDA_EXPRESSION, "function");
        }

        @Override
        public LispObject execute(LispObject arg) {
            final LispObject value1, value2, value3;
            if (arg instanceof CompiledClosure) {
                value1 = NIL;
                value2 = T;
                LispObject name = ((CompiledClosure)arg).getLambdaName();
                value3 = name != null ? name : NIL;
            } else if (arg instanceof Closure) {
                Closure closure = (Closure) arg;
                LispObject expr = closure.getBody();
                expr = new Cons(closure.getLambdaList(), expr);
                expr = new Cons(Symbol.LAMBDA, expr);
                value1 = expr;
                Environment env = closure.getEnvironment();
                if (env == null || env.isEmpty())
                    value2 = NIL;
                else
                    value2 = env; // Return environment as closure-p.
                LispObject name = ((Closure)arg).getLambdaName();
                value3 = name != null ? name : NIL;
            } else if (arg instanceof Function) {
                value1 = NIL;
                value2 = T;
                value3 = ((Function)arg).getLambdaName();
            } else if (arg instanceof StandardGenericFunction) {
                value1 = NIL;
                value2 = T;
                value3 = ((StandardGenericFunction)arg).getGenericFunctionName();
            } else
                return type_error(arg, Symbol.FUNCTION);
            return LispThread.currentThread().setValues(value1, value2, value3);
        }
    };

    // ### funcall
    // This needs to be public for LispAPI.java.
    public static final Primitive FUNCALL = new pf_funcall();
    private static final class pf_funcall extends Primitive {
        pf_funcall() {
            super(Symbol.FUNCALL, "function &rest args");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return LispThread.currentThread().execute(arg);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return LispThread.currentThread().execute(first, second);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return LispThread.currentThread().execute(first, second, third);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth)

        {
            return LispThread.currentThread().execute(first, second, third,
                    fourth);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth)

        {
            return LispThread.currentThread().execute(first, second, third,
                    fourth, fifth);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth)

        {
            return LispThread.currentThread().execute(first, second, third,
                    fourth, fifth, sixth);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth,
                                  LispObject seventh)

        {
            return LispThread.currentThread().execute(first, second, third,
                    fourth, fifth, sixth,
                    seventh);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth,
                                  LispObject seventh, LispObject eigth)

        {
            return LispThread.currentThread().execute(first, second, third,
                    fourth, fifth, sixth,
                    seventh, eigth);
        }
        @Override
        public LispObject execute(LispObject[] args) {
            final int length = args.length - 1; // Number of arguments.
            if (length == 8) {
                return LispThread.currentThread().execute(args[0], args[1],
                        args[2], args[3],
                        args[4], args[5],
                        args[6], args[7],
                        args[8]);
            } else {
                LispObject[] newArgs = new LispObject[length];
                System.arraycopy(args, 1, newArgs, 0, length);
                return LispThread.currentThread().execute(args[0], newArgs);
            }
        }
    };

    // ### apply
    public static final Primitive APPLY = new pf_apply();
    private static final class pf_apply extends Primitive {
        pf_apply() {
            super(Symbol.APPLY, "function &rest args");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject fun, LispObject args)

        {
            final LispThread thread = LispThread.currentThread();
            final int length = args.length();
            switch (length) {
            case 0:
                return thread.execute(fun);
            case 1:
                return thread.execute(fun, ((Cons)args).car);
            case 2: {
                Cons cons = (Cons) args;
                return thread.execute(fun, cons.car, ((Cons)cons.cdr).car);
            }
            case 3:
                return thread.execute(fun, args.car(), args.cadr(),
                                      args.cdr().cdr().car());
            default: {
                final LispObject[] funArgs = new LispObject[length];
                int j = 0;
                while (args != NIL) {
                    funArgs[j++] = args.car();
                    args = args.cdr();
                }
                return funcall(fun, funArgs, thread);
            }
            }
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (third.listp()) {
                final int numFunArgs = 1 + third.length();
                final LispObject[] funArgs = new LispObject[numFunArgs];
                funArgs[0] = second;
                int j = 1;
                while (third != NIL) {
                    funArgs[j++] = third.car();
                    third = third.cdr();
                }
                return funcall(first, funArgs, LispThread.currentThread());
            }
            return type_error(third, Symbol.LIST);
        }
        @Override
        public LispObject execute(final LispObject[] args) {
            final int numArgs = args.length;
            LispObject spread = args[numArgs - 1];
            if (spread.listp()) {
                final int numFunArgs = numArgs - 2 + spread.length();
                final LispObject[] funArgs = new LispObject[numFunArgs];
                int j = 0;
                for (int i = 1; i < numArgs - 1; i++)
                    funArgs[j++] = args[i];
                while (spread != NIL) {
                    funArgs[j++] = spread.car();
                    spread = spread.cdr();
                }
                return funcall(args[0], funArgs, LispThread.currentThread());
            }
            return type_error(spread, Symbol.LIST);
        }
    };

    // ### mapcar
    private static final Primitive MAPCAR = new pf_mapcar();
    private static final class pf_mapcar extends Primitive {
        pf_mapcar() {
            super(Symbol.MAPCAR, "function &rest lists");
        }

        @Override
        public LispObject execute(LispObject fun, LispObject list)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject result = NIL;
            Cons splice = null;
            while (list != NIL) {
                Cons cons;
                if (list instanceof Cons)
                    cons = (Cons) list;
                else
                    return type_error(list, Symbol.LIST);
                LispObject obj = thread.execute(fun, cons.car);
                if (splice == null) {
                    splice = new Cons(obj, result);
                    result = splice;
                } else {
                    Cons c = new Cons(obj);
                    splice.cdr = c;
                    splice = c;
                }
                list = cons.cdr;
            }
            thread._values = null;
            return result;
        }
        @Override
        public LispObject execute(LispObject fun, LispObject list1,
                                  LispObject list2)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject result = NIL;
            Cons splice = null;
            while (list1 != NIL && list2 != NIL) {
                LispObject obj =
                    thread.execute(fun, list1.car(), list2.car());
                if (splice == null) {
                    splice = new Cons(obj, result);
                    result = splice;
                } else {
                    Cons cons = new Cons(obj);
                    splice.cdr = cons;
                    splice = cons;
                }
                list1 = list1.cdr();
                list2 = list2.cdr();
            }
            thread._values = null;
            return result;
        }
        @Override
        public LispObject execute(final LispObject[] args)

        {
            final int numArgs = args.length;
            if (numArgs < 2)
                return error(new WrongNumberOfArgumentsException(this));
            int commonLength = -1;
            for (int i = 1; i < numArgs; i++) {
                if (!args[i].listp())
                    type_error(args[i], Symbol.LIST);
                int len = args[i].length();
                if (commonLength < 0)
                    commonLength = len;
                else if (commonLength > len)
                    commonLength = len;
            }
            final LispThread thread = LispThread.currentThread();
            LispObject[] results = new LispObject[commonLength];
            final int numFunArgs = numArgs - 1;
            final LispObject[] funArgs = new LispObject[numFunArgs];
            for (int i = 0; i < commonLength; i++) {
                for (int j = 0; j < numFunArgs; j++)
                    funArgs[j] = args[j+1].car();
                results[i] = funcall(args[0], funArgs, thread);
                for (int j = 1; j < numArgs; j++)
                    args[j] = args[j].cdr();
            }
            thread._values = null;
            LispObject result = NIL;
            for (int i = commonLength; i-- > 0;)
                result = new Cons(results[i], result);
            return result;
        }
    };

    // ### mapc
    private static final Primitive MAPC = new pf_mapc();
    private static final class pf_mapc extends Primitive {
        pf_mapc() {
            super(Symbol.MAPC, "function &rest lists");
        }

        @Override
        public LispObject execute(LispObject fun, LispObject list)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject result = list;
            while (list != NIL) {
                Cons cons;
                if (list instanceof Cons)
                    cons = (Cons) list;
                else
                    return type_error(list, Symbol.LIST);
                thread.execute(fun, cons.car);
                list = cons.cdr;
            }
            thread._values = null;
            return result;
        }
        @Override
        public LispObject execute(LispObject fun, LispObject list1,
                                  LispObject list2)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject result = list1;
            while (list1 != NIL && list2 != NIL) {
                thread.execute(fun, list1.car(), list2.car());
                list1 = ((Cons)list1).cdr;
                list2 = ((Cons)list2).cdr;
            }
            thread._values = null;
            return result;
        }
        @Override
        public LispObject execute(final LispObject[] args)

        {
            final int numArgs = args.length;
            if (numArgs < 2)
                return error(new WrongNumberOfArgumentsException(this));
            int commonLength = -1;
            for (int i = 1; i < numArgs; i++) {
                if (!args[i].listp())
                    type_error(args[i], Symbol.LIST);
                int len = args[i].length();
                if (commonLength < 0)
                    commonLength = len;
                else if (commonLength > len)
                    commonLength = len;
            }
            final LispThread thread = LispThread.currentThread();
            LispObject result = args[1];
            final int numFunArgs = numArgs - 1;
            final LispObject[] funArgs = new LispObject[numFunArgs];
            for (int i = 0; i < commonLength; i++) {
                for (int j = 0; j < numFunArgs; j++)
                    funArgs[j] = args[j+1].car();
                funcall(args[0], funArgs, thread);
                for (int j = 1; j < numArgs; j++)
                    args[j] = args[j].cdr();
            }
            thread._values = null;
            return result;
        }
    };

    // ### macroexpand
    private static final Primitive MACROEXPAND = new pf_macroexpand();
    private static final class pf_macroexpand extends Primitive {
        pf_macroexpand() {
            super(Symbol.MACROEXPAND, "form &optional env");
        }

        @Override
        public LispObject execute(LispObject form) {
            return macroexpand(form,
                               new Environment(),
                               LispThread.currentThread());
        }
        @Override
        public LispObject execute(LispObject form, LispObject env)

        {
            return macroexpand(form,
                               env != NIL ? checkEnvironment(env) : new Environment(),
                               LispThread.currentThread());
        }
    };

    // ### macroexpand-1
    private static final Primitive MACROEXPAND_1 = new pf_macroexpand_1();
    private static final class pf_macroexpand_1 extends Primitive {
        pf_macroexpand_1() {
            super(Symbol.MACROEXPAND_1, "form &optional env");
        }

        @Override
        public LispObject execute(LispObject form) {
            return macroexpand_1(form,
                                 new Environment(),
                                 LispThread.currentThread());
        }
        @Override
        public LispObject execute(LispObject form, LispObject env)

        {
            return macroexpand_1(form,
                                 env != NIL ? checkEnvironment(env) : new Environment(),
                                 LispThread.currentThread());
        }
    };

    // ### gensym
    private static final Primitive GENSYM = new pf_gensym();
    private static final class pf_gensym extends Primitive {
        pf_gensym() {
            super(Symbol.GENSYM, "&optional x");
        }

        @Override
        public LispObject execute() {
            return gensym("G", LispThread.currentThread());
        }
        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Fixnum) {
                int n = ((Fixnum)arg).value;
                if (n >= 0) {
                    StringBuilder sb = new StringBuilder("G");
                    sb.append(n); // Decimal representation.
                    return new Symbol(new SimpleString(sb));
                }
            } else if (arg instanceof Bignum) {
                BigInteger n = ((Bignum)arg).value;
                if (n.signum() >= 0) {
                    StringBuilder sb = new StringBuilder("G");
                    sb.append(n.toString()); // Decimal representation.
                    return new Symbol(new SimpleString(sb));
                }
            } else if (arg instanceof AbstractString)
                return gensym(arg.getStringValue(), LispThread.currentThread());
            return type_error(arg,
                              list(Symbol.OR,
                                   Symbol.STRING,
                                   Symbol.UNSIGNED_BYTE));
        }
    };

    // ### string
    private static final Primitive STRING = new pf_string();
    private static final class pf_string extends Primitive {
        pf_string() {
            super(Symbol.STRING, "x");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.STRING();
        }
    };

    // ### intern string &optional package => symbol, status
    // STATUS is one of :INHERITED, :EXTERNAL, :INTERNAL or NIL.
    // "It is implementation-dependent whether the string that becomes the new
    // symbol's name is the given string or a copy of it."
    private static final Primitive INTERN = new pf_intern();
    private static final class pf_intern extends Primitive {
        pf_intern() {
            super(Symbol.INTERN, "string &optional package");
        }

        @Override
        public LispObject execute(LispObject arg) {
            final SimpleString s;
            if (arg instanceof SimpleString)
                s = (SimpleString) arg;
            else
                s = new SimpleString(arg.getStringValue());
            final LispThread thread = LispThread.currentThread();
            Package pkg = (Package) Symbol._PACKAGE_.symbolValue(thread);
            return pkg.intern(s, thread);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final SimpleString s;
            if (first instanceof SimpleString)
                s = (SimpleString) first;
            else
                s = new SimpleString(first.getStringValue());
            Package pkg = coerceToPackage(second);
            return pkg.intern(s, LispThread.currentThread());
        }
    };

    // ### unintern
    // unintern symbol &optional package => generalized-boolean
    private static final Primitive UNINTERN = new pf_unintern();
    private static final class pf_unintern extends Primitive {
        pf_unintern() {
            super(Symbol.UNINTERN, "symbol &optional package");
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length == 0 || args.length > 2)
                return error(new WrongNumberOfArgumentsException(this));
            Symbol symbol = checkSymbol(args[0]);
            Package pkg;
            if (args.length == 2)
                pkg = coerceToPackage(args[1]);
            else
                pkg = getCurrentPackage();
            return pkg.unintern(symbol);
        }
    };

    // ### find-package
    private static final Primitive FIND_PACKAGE = new pf_find_package();
    private static final class pf_find_package extends Primitive {
        pf_find_package() {
            super(Symbol.FIND_PACKAGE, "name");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Package)
                return arg;
            if (arg instanceof AbstractString) {
                Package pkg =
                    Packages.findPackage(arg.getStringValue());
                return pkg != null ? pkg : NIL;
            }
            if (arg instanceof Symbol) {
                Package pkg = Packages.findPackage(checkSymbol(arg).getName());
                return pkg != null ? pkg : NIL;
            }
            if (arg instanceof LispCharacter) {
                String packageName =
                    String.valueOf(new char[] {((LispCharacter)arg).getValue()});
                Package pkg = Packages.findPackage(packageName);
                return pkg != null ? pkg : NIL;
            }
            return NIL;
        }
    };

    // ### %make-package
    // %make-package package-name nicknames use => package
    private static final Primitive _MAKE_PACKAGE = new pf__make_package();
    private static final class pf__make_package extends Primitive {
        pf__make_package() {
            super("%make-package", PACKAGE_SYS, false);
        }

        /**
         * This invocation is solely used to be able to create
         * a package to bind to *FASL-ANONYMOUS-PACKAGE*
         */
        @Override
        public LispObject execute()

        {
            return new Package();
        }

        /**
         * This invocation is used by MAKE-PACKAGE to create a package
         */
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            String packageName = javaString(first);
            Package pkg = Packages.findPackage(packageName);
            if (pkg != null)
                error(new LispError("Package " + packageName +
                                    " already exists."));
            LispObject nicknames = checkList(second);
            if (nicknames != NIL) {
                LispObject list = nicknames;
                while (list != NIL) {
                    String nick = javaString(list.car());
                    if (Packages.findPackage(nick) != null) {
                        error(new PackageError("A package named " + nick +
                                               " already exists."));
                    }
                    list = list.cdr();
                }
            }
            LispObject use = checkList(third);
            if (use != NIL) {
                LispObject list = use;
                while (list != NIL) {
                    LispObject obj = list.car();
                    if (obj instanceof Package) {
                        // OK.
                    } else {
                        String s = javaString(obj);
                        Package p = Packages.findPackage(s);
                        if (p == null) {
                            error(new LispError(obj.writeToString() +
                                                " is not the name of a package."));
                            return NIL;
                        }
                    }
                    list = list.cdr();
                }
            }
            // Now create the package.
            pkg = Packages.createPackage(packageName);
            // Add the nicknames.
            while (nicknames != NIL) {
                String nick = javaString(nicknames.car());
                pkg.addNickname(nick);
                nicknames = nicknames.cdr();
            }
            // Create the use list.
            while (use != NIL) {
                LispObject obj = use.car();
                if (obj instanceof Package)
                    pkg.usePackage((Package)obj);
                else {
                    String s = javaString(obj);
                    Package p = Packages.findPackage(s);
                    if (p == null) {
                        error(new LispError(obj.writeToString() +
                                            " is not the name of a package."));
                        return NIL;
                    }
                    pkg.usePackage(p);
                }
                use = use.cdr();
            }
            return pkg;
        }
    };

    // ### %in-package
    private static final Primitive _IN_PACKAGE = new pf__in_package();
    private static final class pf__in_package extends Primitive {
        pf__in_package() {
            super("%in-package", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            final String packageName = javaString(arg);
            final Package pkg = Packages.findPackage(packageName);
            if (pkg == null)
                return error(new PackageError("The name " + packageName +
                                              " does not designate any package."));
            SpecialBinding binding =
                LispThread.currentThread().getSpecialBinding(Symbol._PACKAGE_);
            if (binding != null)
                binding.value = pkg;
            else
                // No dynamic binding.
                Symbol._PACKAGE_.setSymbolValue(pkg);
            return pkg;
        }
    };

    // ### use-package packages-to-use &optional package => t
    private static final Primitive USE_PACKAGE = new pf_use_package();
    private static final class pf_use_package extends Primitive {
        pf_use_package() {
            super(Symbol.USE_PACKAGE, "packages-to-use &optional package");
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length < 1 || args.length > 2)
                return error(new WrongNumberOfArgumentsException(this));
            Package pkg;
            if (args.length == 2)
                pkg = coerceToPackage(args[1]);
            else
                pkg = getCurrentPackage();
            if (args[0].listp()) {
                LispObject list = args[0];
                while (list != NIL) {
                    pkg.usePackage(coerceToPackage(list.car()));
                    list = list.cdr();
                }
            } else
                pkg.usePackage(coerceToPackage(args[0]));
            return T;
        }
    };

    // ### package-symbols
    private static final Primitive PACKAGE_SYMBOLS = new pf_package_symbols();
    private static final class pf_package_symbols extends Primitive {
        pf_package_symbols() {
            super("package-symbols", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPackage(arg).getSymbols();
        }
    };

    // ### package-internal-symbols
    private static final Primitive PACKAGE_INTERNAL_SYMBOLS = new pf_package_internal_symbols();
    private static final class pf_package_internal_symbols extends Primitive {
        pf_package_internal_symbols() {
            super("package-internal-symbols", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPackage(arg).PACKAGE_INTERNAL_SYMBOLS();
        }
    };

    // ### package-external-symbols
    private static final Primitive PACKAGE_EXTERNAL_SYMBOLS = new pf_package_external_symbols();
    private static final class pf_package_external_symbols extends Primitive {
        pf_package_external_symbols() {
            super("package-external-symbols", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPackage(arg).PACKAGE_EXTERNAL_SYMBOLS();
        }
    };

    // ### package-inherited-symbols
    private static final Primitive PACKAGE_INHERITED_SYMBOLS = new pf_package_inherited_symbols();
    private static final class pf_package_inherited_symbols extends Primitive {
        pf_package_inherited_symbols() {
            super("package-inherited-symbols", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return coerceToPackage(arg).PACKAGE_INHERITED_SYMBOLS();
        }
    };

    // ### export symbols &optional package
    private static final Primitive EXPORT = new pf_export();
    private static final class pf_export extends Primitive {
        pf_export() {
            super(Symbol.EXPORT, "symbols &optional package");
        }

        @Override
        public LispObject execute(LispObject arg) {
            final Package pkg = (Package) Symbol._PACKAGE_.symbolValue();
            if (arg instanceof Cons) {
                for (LispObject list = arg; list != NIL; list = list.cdr())
                    pkg.export(checkSymbol(list.car()));
            } else
                pkg.export(checkSymbol(arg));
            return T;
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first instanceof Cons) {
                Package pkg = coerceToPackage(second);
                for (LispObject list = first; list != NIL; list = list.cdr())
                    pkg.export(checkSymbol(list.car()));
            } else
                coerceToPackage(second).export(checkSymbol(first));
            return T;
        }
    };

    // ### find-symbol string &optional package => symbol, status
    private static final Primitive FIND_SYMBOL = new pf_find_symbol();
    private static final class pf_find_symbol extends Primitive {
        pf_find_symbol() {
            super(Symbol.FIND_SYMBOL, "string &optional package");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return getCurrentPackage()
                   .findSymbol(checkString(arg).getStringValue());
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return coerceToPackage(second)
                   .findSymbol(checkString(first).getStringValue());
        }
    };

    // ### fset name function &optional source-position arglist documentation
    // => function
    private static final Primitive FSET = new pf_fset();
    private static final class pf_fset extends Primitive {
        pf_fset() {
            super("fset", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return execute(first, second, NIL, NIL, NIL);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return execute(first, second, third, NIL, NIL);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth)

        {
            return execute(first, second, third, fourth, NIL);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth)

        {
            if (first instanceof Symbol) {
                checkRedefinition(first);
                Symbol symbol = checkSymbol(first);
                symbol.setSymbolFunction(second);
                final LispThread thread = LispThread.currentThread();
                LispObject sourcePathname = _SOURCE_.symbolValue(thread);
                LispObject sourcePosition = third;
                if (sourcePathname != NIL)
                    sourcePosition = _SOURCE_POSITION_.symbolValue(thread);
                if (sourcePathname == NIL)
                    sourcePathname = Keyword.TOP_LEVEL;
                if (sourcePathname != Keyword.TOP_LEVEL)
                    put(symbol, Symbol._SOURCE, new Cons(sourcePathname, third));
                else
                    put(symbol, Symbol._SOURCE, sourcePathname);
            } else if (isValidSetfFunctionName(first)) {
                // SETF function
                checkRedefinition(first);
                Symbol symbol = checkSymbol(first.cadr());
                put(symbol, Symbol.SETF_FUNCTION, second);
            } else
                return type_error(first, FUNCTION_NAME);
            if (second instanceof Operator) {
                Operator op = (Operator) second;
                op.setLambdaName(first);
                if (fourth != NIL)
                    op.setLambdaList(fourth);
                if (fifth != NIL)
                    op.setDocumentation(Symbol.FUNCTION, fifth);
            }
            return second;
        }
    };

    // ### %set-symbol-plist
    private static final Primitive _SET_SYMBOL_PLIST = new pf__set_symbol_plist();
    private static final class pf__set_symbol_plist extends Primitive {
        pf__set_symbol_plist() {
            super("%set-symbol-plist", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            checkSymbol(first).setPropertyList(checkList(second));
            return second;
        }
    };

    // ### getf plist indicator &optional default => value
    private static final Primitive GETF = new pf_getf();
    private static final class pf_getf extends Primitive {
        pf_getf() {
            super(Symbol.GETF, "plist indicator &optional default");
        }

        @Override
        public LispObject execute(LispObject plist, LispObject indicator)

        {
            return getf(plist, indicator, NIL);
        }
        @Override
        public LispObject execute(LispObject plist, LispObject indicator,
                                  LispObject defaultValue)

        {
            return getf(plist, indicator, defaultValue);
        }
    };

    // ### get symbol indicator &optional default => value
    private static final Primitive GET = new pf_get();
    private static final class pf_get extends Primitive {
        pf_get() {
            super(Symbol.GET, "symbol indicator &optional default");
        }

        @Override
        public LispObject execute(LispObject symbol, LispObject indicator)

        {
            return get(symbol, indicator, NIL);
        }
        @Override
        public LispObject execute(LispObject symbol, LispObject indicator,
                                  LispObject defaultValue)

        {
            return get(symbol, indicator, defaultValue);
        }
    };

    // ### put symbol indicator value => value
    private static final Primitive PUT = new pf_put();
    private static final class pf_put extends Primitive {
        pf_put() {
            super("put", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject symbol, LispObject indicator,
                                  LispObject value)

        {
            return put(checkSymbol(symbol), indicator, value);
        }
        @Override
        public LispObject execute(LispObject symbol, LispObject indicator,
                                  LispObject defaultValue, LispObject value)

        {
            return put(checkSymbol(symbol), indicator, value);
        }
    };

    // ### macrolet
    private static final SpecialOperator MACROLET = new sf_macrolet();
    private static final class sf_macrolet extends SpecialOperator {
        sf_macrolet() {
            super(Symbol.MACROLET, "definitions &rest body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            LispObject defs = checkList(args.car());
            final LispThread thread = LispThread.currentThread();
            final SpecialBindingsMark mark = thread.markSpecialBindings();

            try {
                Environment ext = new Environment(env);
                while (defs != NIL) {
                    LispObject def = checkList(defs.car());
                    Symbol symbol = checkSymbol(def.car());
                    Symbol make_expander_for_macrolet =
                        PACKAGE_SYS.intern("MAKE-EXPANDER-FOR-MACROLET");
                    LispObject expander =
                        make_expander_for_macrolet.execute(def);
                    Closure expansionFunction = new Closure(expander, env);
                    MacroObject macroObject =
                        new MacroObject(symbol, expansionFunction);
                    ext.addFunctionBinding(symbol, macroObject);
                    defs = defs.cdr();
                }
                return progn(ext.processDeclarations(args.cdr()), ext, thread);
            }
            finally {
                thread.resetSpecialBindings(mark);
            }
        }
    };

    private static final Primitive MAKE_EXPANDER_FOR_MACROLET = new pf_make_expander_for_macrolet();
    private static final class pf_make_expander_for_macrolet extends Primitive {
        pf_make_expander_for_macrolet() {
            super("make-expander-for-macrolet", PACKAGE_SYS, true,
                  "definition");
        }

        @Override
        public LispObject execute(LispObject definition)

        {
            Symbol symbol = checkSymbol(definition.car());
            LispObject lambdaList = definition.cadr();
            LispObject body = definition.cddr();
            LispObject block =
                new Cons(Symbol.BLOCK, new Cons(symbol, body));
            LispObject toBeApplied =
                list(Symbol.LAMBDA, lambdaList, block);
            final LispThread thread = LispThread.currentThread();
            LispObject formArg = gensym("WHOLE-", thread);
            LispObject envArg = gensym("ENVIRONMENT-", thread); // Ignored.
            LispObject expander =
                list(Symbol.LAMBDA, list(formArg, envArg),
                     list(Symbol.APPLY, toBeApplied,
                          list(Symbol.CDR, formArg)));
            return expander;
        }
    };

    // ### tagbody
    private static final SpecialOperator TAGBODY = new sf_tagbody();
    private static final class sf_tagbody extends SpecialOperator {
        sf_tagbody() {
            super(Symbol.TAGBODY, "&rest statements");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            Environment ext = new Environment(env);
            try {
                return processTagBody(args, preprocessTagBody(args, ext), ext);
            }
            finally {
                ext.inactive = true;
            }
        }
    };

    // ### go
    private static final SpecialOperator GO = new sf_go();
    private static final class sf_go extends SpecialOperator {
        sf_go() {
            super(Symbol.GO, "tag");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() != 1)
                return error(new WrongNumberOfArgumentsException(this));
            Binding binding = env.getTagBinding(args.car());
            if (binding == null)
                return error(new ControlError("No tag named " +
                                              args.car().writeToString() +
                                              " is currently visible."));

            return nonLocalGo(binding, args.car());
        }
    };

    // ### block
    private static final SpecialOperator BLOCK = new sf_block();
    private static final class sf_block extends SpecialOperator {
        sf_block() {
            super(Symbol.BLOCK, "name &rest forms");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args == NIL)
                return error(new WrongNumberOfArgumentsException(this));
            LispObject tag;
            tag = checkSymbol(args.car());
            LispObject body = ((Cons)args).cdr();
            Environment ext = new Environment(env);
            final LispObject block = new LispObject();
            ext.addBlock(tag, block);
            LispObject result = NIL;
            final LispThread thread = LispThread.currentThread();
            try {
                return progn(body, ext, thread);
            } catch (Return ret) {
                if (ret.getBlock() == block) {
                    return ret.getResult();
                }
                throw ret;
            }
            finally {
                ext.inactive = true;
            }
        }
    };

    // ### return-from
    private static final SpecialOperator RETURN_FROM = new sf_return_from();
    private static final class sf_return_from extends SpecialOperator {
        sf_return_from() {
            super(Symbol.RETURN_FROM, "name &optional value");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final int length = args.length();
            if (length < 1 || length > 2)
                return error(new WrongNumberOfArgumentsException(this));
            Symbol symbol;
            symbol = checkSymbol(args.car());

            return nonLocalReturn(env.getBlockBinding(symbol), symbol,
                                  (length == 2) ? eval(args.cadr(), env,
                                                       LispThread.currentThread())
                                  : NIL);
        }
    };

    // ### catch
    private static final SpecialOperator CATCH = new sf_catch();
    private static final class sf_catch extends SpecialOperator {
        sf_catch() {
            super(Symbol.CATCH, "tag &body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() < 1)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            LispObject tag = eval(args.car(), env, thread);
            thread.pushCatchTag(tag);
            LispObject body = args.cdr();
            LispObject result = NIL;
            try {
                return progn(body, env, thread);
            } catch (Throw t) {
                if (t.tag == tag) {
                    return t.getResult(thread);
                }
                throw t;
            } catch (Return ret) {
                throw ret;
            }
            finally {
                thread.popCatchTag();
            }
        }
    };

    // ### throw
    private static final SpecialOperator THROW = new sf_throw();
    private static final class sf_throw extends SpecialOperator {
        sf_throw() {
            super(Symbol.THROW, "tag result");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() != 2)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            thread.throwToTag(eval(args.car(), env, thread),
                              eval(args.cadr(), env, thread));
            // Not reached.
            return NIL;
        }
    };

    // ### unwind-protect
    private static final SpecialOperator UNWIND_PROTECT = new sf_unwind_protect();
    private static final class sf_unwind_protect extends SpecialOperator {
        sf_unwind_protect() {
            super(Symbol.UNWIND_PROTECT, "protected &body cleanup");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject result;
            LispObject[] values;
            try {
                result = eval(args.car(), env, thread);
            }
            finally {
                values = thread._values;
                LispObject body = args.cdr();
                while (body != NIL) {
                    eval(body.car(), env, thread);
                    body = ((Cons)body).cdr;
                }
                thread._values = values;
            }
            if (values != null)
                thread.setValues(values);
            else
                thread._values = null;
            return result;
        }
    };

    // ### eval-when
    private static final SpecialOperator EVAL_WHEN = new sf_eval_when();
    private static final class sf_eval_when extends SpecialOperator {
        sf_eval_when() {
            super(Symbol.EVAL_WHEN, "situations &rest forms");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            LispObject situations = args.car();
            if (situations != NIL) {
                if (memq(Keyword.EXECUTE, situations) ||
                        memq(Symbol.EVAL, situations)) {
                    return progn(args.cdr(), env, LispThread.currentThread());
                }
            }
            return NIL;
        }
    };

    // ### multiple-value-bind
    // multiple-value-bind (var*) values-form declaration* form*
    // Should be a macro.
    private static final SpecialOperator MULTIPLE_VALUE_BIND = new sf_multiple_value_bind();
    private static final class sf_multiple_value_bind extends SpecialOperator {
        sf_multiple_value_bind() {
            super(Symbol.MULTIPLE_VALUE_BIND,
                  "vars value-form &body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            LispObject vars = args.car();
            args = args.cdr();
            LispObject valuesForm = args.car();
            LispObject body = args.cdr();
            final LispThread thread = LispThread.currentThread();
            LispObject value = eval(valuesForm, env, thread);
            LispObject[] values = thread._values;
            if (values == null) {
                // eval() did not return multiple values.
                values = new LispObject[1];
                values[0] = value;
            }
            // Process declarations.
            LispObject bodyAndDecls = parseBody(body, false);
            LispObject specials = parseSpecials(bodyAndDecls.NTH(1));
            body = bodyAndDecls.car();

            final SpecialBindingsMark mark = thread.markSpecialBindings();
            final Environment ext = new Environment(env);
            int i = 0;
            LispObject var = vars.car();
            while (var != NIL) {
                final Symbol sym;

                sym =  checkSymbol(var);

                LispObject val = i < values.length ? values[i] : NIL;
                if (specials != NIL && memq(sym, specials)) {
                    thread.bindSpecial(sym, val);
                    ext.declareSpecial(sym);
                } else if (sym.isSpecialVariable()) {
                    thread.bindSpecial(sym, val);
                } else
                    ext.bind(sym, val);
                vars = vars.cdr();
                var = vars.car();
                ++i;
            }
            // Make sure free special declarations are visible in the body.
            // "The scope of free declarations specifically does not include
            // initialization forms for bindings established by the form
            // containing the declarations." (3.3.4)
            while (specials != NIL) {
                Symbol symbol = (Symbol) specials.car();
                ext.declareSpecial(symbol);
                specials = ((Cons)specials).cdr;
            }
            thread._values = null;
            LispObject result = NIL;
            try {
                result  = progn(body, ext, thread);
            }
            finally {
                thread.resetSpecialBindings(mark);
            }
            return result;
        }
    };

    // ### multiple-value-prog1
    private static final SpecialOperator MULTIPLE_VALUE_PROG1 = new sf_multiple_value_prog1();
    private static final class sf_multiple_value_prog1 extends SpecialOperator {
        sf_multiple_value_prog1() {
            super(Symbol.MULTIPLE_VALUE_PROG1,
                  "values-form &rest forms");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() == 0)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            LispObject result = eval(args.car(), env, thread);
            LispObject[] values = thread._values;
            while ((args = args.cdr()) != NIL)
                eval(args.car(), env, thread);
            if (values != null)
                thread.setValues(values);
            else
                thread._values = null;
            return result;
        }
    };

    // ### multiple-value-call
    private static final SpecialOperator MULTIPLE_VALUE_CALL = new sf_multiple_value_call();
    private static final class sf_multiple_value_call extends SpecialOperator {
        sf_multiple_value_call() {
            super(Symbol.MULTIPLE_VALUE_CALL, "fun &rest args");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() == 0)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            LispObject function;
            LispObject obj = eval(args.car(), env, thread);
            args = args.cdr();
            if (obj instanceof Symbol) {
                function = obj.getSymbolFunction();
                if (function == null)
                    error(new UndefinedFunction(obj));
            } else if (obj instanceof Function) {
                function = obj;
            } else {
                error(new LispError(obj.writeToString() +
                                    " is not a function name."));
                return NIL;
            }
            ArrayList<LispObject> arrayList = new ArrayList<LispObject>();
            while (args != NIL) {
                LispObject form = args.car();
                LispObject result = eval(form, env, thread);
                LispObject[] values = thread._values;
                if (values != null) {
                    for (int i = 0; i < values.length; i++)
                        arrayList.add(values[i]);
                } else
                    arrayList.add(result);
                args = ((Cons)args).cdr;
            }
            LispObject[] argv = new LispObject[arrayList.size()];
            arrayList.toArray(argv);
            return funcall(function, argv, thread);
        }
    };

    // ### and
    // Should be a macro.
    private static final SpecialOperator AND = new sf_and();
    private static final class sf_and extends SpecialOperator {
        sf_and() {
            super(Symbol.AND, "&rest forms");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject result = T;
            while (args != NIL) {
                result = eval(args.car(), env, thread);
                if (result == NIL) {
                    if (((Cons)args).cdr != NIL) {
                        // Not the last form.
                        thread._values = null;
                    }
                    break;
                }
                args = ((Cons)args).cdr;
            }
            return result;
        }
    };

    // ### or
    // Should be a macro.
    private static final SpecialOperator OR = new sf_or();
    private static final class sf_or extends SpecialOperator {
        sf_or() {
            super(Symbol.OR, "&rest forms");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispThread thread = LispThread.currentThread();
            LispObject result = NIL;
            while (args != NIL) {
                result = eval(args.car(), env, thread);
                if (result != NIL) {
                    if (((Cons)args).cdr != NIL) {
                        // Not the last form.
                        thread._values = null;
                    }
                    break;
                }
                args = ((Cons)args).cdr;
            }
            return result;
        }
    };

    // ### multiple-value-list form => list
    // Evaluates form and creates a list of the multiple values it returns.
    // Should be a macro.
    private static final SpecialOperator MULTIPLE_VALUE_LIST = new sf_multiple_value_list();
    private static final class sf_multiple_value_list extends SpecialOperator {
        sf_multiple_value_list() {
            super(Symbol.MULTIPLE_VALUE_LIST, "value-form");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() != 1)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            LispObject result = eval(((Cons)args).car, env, thread);
            LispObject[] values = thread._values;
            if (values == null)
                return new Cons(result);
            thread._values = null;
            LispObject list = NIL;
            for (int i = values.length; i-- > 0;)
                list = new Cons(values[i], list);
            return list;
        }
    };

    // ### nth-value n form => object
    // Evaluates n and then form and returns the nth value returned by form, or
    // NIL if n >= number of values returned.
    // Should be a macro.
    private static final SpecialOperator NTH_VALUE = new sf_nth_value();
    private static final class sf_nth_value extends SpecialOperator {
        sf_nth_value() {
            super(Symbol.NTH_VALUE, "n form");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() != 2)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            int n = Fixnum.getValue(eval(args.car(), env, thread));
            if (n < 0)
                n = 0;
            LispObject result = eval(args.cadr(), env, thread);
            LispObject[] values = thread._values;
            thread._values = null;
            if (values == null) {
                // A single value was returned.
                return n == 0 ? result : NIL;
            }
            if (n < values.length)
                return values[n];
            return NIL;
        }
    };

    // ### call-count
    private static final Primitive CALL_COUNT = new pf_call_count();
    private static final class pf_call_count extends Primitive {
        pf_call_count() {
            super("call-count", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return Fixnum.getInstance(arg.getCallCount());
        }
    };

    // ### set-call-count
    private static final Primitive SET_CALL_COUNT = new pf_set_call_count();
    private static final class pf_set_call_count extends Primitive {
        pf_set_call_count() {
            super("set-call-count", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            first.setCallCount(Fixnum.getValue(second));
            return second;
        }
    };

    // ### hot-count
    private static final Primitive HOT_COUNT = new pf_hot_count();
    private static final class pf_hot_count extends Primitive {
        pf_hot_count() {
            super("hot-count", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return Fixnum.getInstance(arg.getHotCount());
        }
    };

    // ### set-hot-count
    private static final Primitive SET_HOT_COUNT = new pf_set_hot_count();
    private static final class pf_set_hot_count extends Primitive {
        pf_set_hot_count() {
            super("set-hot-count", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            first.setHotCount(Fixnum.getValue(second));
            return second;
        }
    };

    // ### lambda-name
    private static final Primitive LAMBDA_NAME = new pf_lambda_name();
    private static final class pf_lambda_name extends Primitive {
        pf_lambda_name() {
            super("lambda-name", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Operator) {
                return ((Operator)arg).getLambdaName();
            }
            if (arg instanceof StandardGenericFunction) {
                return ((StandardGenericFunction)arg).getGenericFunctionName();
            }
            return type_error(arg, Symbol.FUNCTION);
        }
    };

    // ### %set-lambda-name
    private static final Primitive _SET_LAMBDA_NAME = new pf__set_lambda_name();
    private static final class pf__set_lambda_name extends Primitive {
        pf__set_lambda_name() {
            super("%set-lambda-name", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first instanceof Operator) {
                ((Operator)first).setLambdaName(second);
                return second;
            }
            if (first instanceof StandardGenericFunction) {
                ((StandardGenericFunction)first).setGenericFunctionName(second);
                return second;
            }
            return type_error(first, Symbol.FUNCTION);
        }
    };

    // ### shrink-vector vector new-size => vector
    // Destructively alters the vector, changing its length to NEW-SIZE, which
    // must be less than or equal to its current length.
    // shrink-vector vector new-size => vector
    private static final Primitive SHRINK_VECTOR = new pf_shrink_vector();
    private static final class pf_shrink_vector extends Primitive {
        pf_shrink_vector() {
            super("shrink-vector", PACKAGE_SYS, true, "vector new-size");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            checkVector(first).shrink(Fixnum.getValue(second));
            return first;
        }
    };

    // ### subseq sequence start &optional end
    private static final Primitive SUBSEQ = new pf_subseq();
    private static final class pf_subseq extends Primitive {
        pf_subseq() {
            super(PACKAGE_SYS.intern("%SUBSEQ"), "sequence start &optional end");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final int start = Fixnum.getValue(second);
            if (start < 0) {
                StringBuilder sb = new StringBuilder("Bad start index (");
                sb.append(start);
                sb.append(") for SUBSEQ.");
                error(new TypeError(sb.toString()));
            }
            if (first.listp())
                return list_subseq(first, start, -1);
            if (first instanceof AbstractVector) {
                final AbstractVector v = (AbstractVector) first;
                return v.subseq(start, v.length());
            }
            return type_error(first, Symbol.SEQUENCE);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            final int start = Fixnum.getValue(second);
            if (start < 0) {
                StringBuilder sb = new StringBuilder("Bad start index (");
                sb.append(start);
                sb.append(").");
                error(new TypeError(sb.toString()));
            }
            int end;
            if (third != NIL) {
                end = Fixnum.getValue(third);
                if (start > end) {
                    StringBuilder sb = new StringBuilder("Start index (");
                    sb.append(start);
                    sb.append(") is greater than end index (");
                    sb.append(end);
                    sb.append(") for SUBSEQ.");
                    error(new TypeError(sb.toString()));
                }
            } else
                end = -1;
            if (first.listp())
                return list_subseq(first, start, end);
            if (first instanceof AbstractVector) {
                final AbstractVector v = (AbstractVector) first;
                if (end < 0)
                    end = v.length();
                return v.subseq(start, end);
            }
            return type_error(first, Symbol.SEQUENCE);
        }
    };

    static final LispObject list_subseq(LispObject list, int start,
            int end)

    {
        int index = 0;
        LispObject result = NIL;
        while (list != NIL) {
            if (end >= 0 && index == end)
                return result.nreverse();
            if (index++ >= start)
                result = new Cons(list.car(), result);
            list = list.cdr();
        }
        return result.nreverse();
    }

    // ### list
    private static final Primitive LIST = new pf_list();
    private static final class pf_list extends Primitive {
        pf_list() {
            super(Symbol.LIST, "&rest objects");
        }

        @Override
        public LispObject execute() {
            return NIL;
        }
        @Override
        public LispObject execute(LispObject arg) {
            return new Cons(arg);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second) {
            return new Cons(first, new Cons(second));
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third) {
            return new Cons(first, new Cons(second, new Cons(third)));
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth) {
            return new Cons(first,
                            new Cons(second,
                                     new Cons(third,
                                              new Cons(fourth))));
        }
        @Override
        public LispObject execute(LispObject[] args) {
            LispObject result = NIL;
            for (int i = args.length; i-- > 0;)
                result = new Cons(args[i], result);
            return result;
        }
    };

    // ### list*
    private static final Primitive LIST_STAR = new pf_list_star();
    private static final class pf_list_star extends Primitive {
        pf_list_star() {
            super(Symbol.LIST_STAR, "&rest objects");
        }

        @Override
        public LispObject execute() {
            return error(new WrongNumberOfArgumentsException(this));
        }
        @Override
        public LispObject execute(LispObject arg) {
            return arg;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return new Cons(first, second);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            return new Cons(first, new Cons(second, third));
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth)

        {
            return new Cons(first,
                            new Cons(second,
                                     new Cons(third, fourth)));
        }
        @Override
        public LispObject execute(LispObject[] args) {
            int i = args.length - 1;
            LispObject result = args[i];
            while (i-- > 0)
                result = new Cons(args[i], result);
            return result;
        }
    };

    // ### nreverse
    public static final Primitive NREVERSE = new pf_nreverse();
    private static final class pf_nreverse extends Primitive {
        pf_nreverse() {
            super("%NREVERSE", PACKAGE_SYS, false, "sequence");
        }

        @Override
        public LispObject execute (LispObject arg) {
            return arg.nreverse();
        }
    };

    // ### nreconc
    private static final Primitive NRECONC = new pf_nreconc();
    private static final class pf_nreconc extends Primitive {
        pf_nreconc() {
            super(Symbol.NRECONC, "list tail");
        }

        @Override
        public LispObject execute(LispObject list, LispObject obj)

        {
            if (list instanceof Cons) {
                LispObject list3 = list.cdr();
                if (list3 instanceof Cons) {
                    if (list3.cdr() instanceof Cons) {
                        LispObject list1 = list3;
                        LispObject list2 = NIL;
                        do {
                            LispObject h = list3.cdr();
                            list3.setCdr(list2);
                            list2 = list3;
                            list3 = h;
                        } while (list3.cdr() instanceof Cons);
                        list.setCdr(list2);
                        list1.setCdr(list3);
                    }
                    LispObject h = list.car();
                    list.setCar(list3.car());
                    list3.setCar(h);
                    list3.setCdr(obj);
                } else if (list3 == NIL) {
                    list.setCdr(obj);
                } else
                    type_error(list3, Symbol.LIST);
                return list;
            } else if (list == NIL)
                return obj;
            else
                return type_error(list, Symbol.LIST);
        }
    };

    // ### reverse
    private static final Primitive REVERSE = new pf_reverse();
    private static final class pf_reverse extends Primitive {
        pf_reverse() {
            super("%reverse", PACKAGE_SYS, false, "sequence");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.reverse();
        }
    };

    // ### delete-eq item sequence => result-sequence
    private static final Primitive DELETE_EQ = new pf_delete_eq();
    private static final class pf_delete_eq extends Primitive {
        pf_delete_eq() {
            super("delete-eq", PACKAGE_SYS, true, "item sequence");
        }

        @Override
        public LispObject execute(LispObject item, LispObject sequence)

        {
            if (sequence instanceof AbstractVector)
                return ((AbstractVector)sequence).deleteEq(item);
            else
                return LIST_DELETE_EQ.execute(item, sequence);
        }
    };

    // ### delete-eql item seqluence => result-seqluence
    private static final Primitive DELETE_EQL = new pf_delete_eql();
    private static final class pf_delete_eql extends Primitive {
        pf_delete_eql() {
            super("delete-eql", PACKAGE_SYS, true, "item sequence");
        }

        @Override
        public LispObject execute(LispObject item, LispObject sequence)

        {
            if (sequence instanceof AbstractVector)
                return ((AbstractVector)sequence).deleteEql(item);
            else
                return LIST_DELETE_EQL.execute(item, sequence);
        }
    };

    // ### list-delete-eq item list => result-list
    static final Primitive LIST_DELETE_EQ = new pf_list_delete_eq();
    private static final class pf_list_delete_eq extends Primitive {
        pf_list_delete_eq() {
            super("list-delete-eq", PACKAGE_SYS, true, "item list");
        }

        @Override
        public LispObject execute(LispObject item, LispObject list)

        {
            if (list instanceof Cons) {
                LispObject tail = list;
                LispObject splice = list;
                while (tail instanceof Cons) {
                    LispObject car = tail.car();
                    if (car == item) {
                        if (tail.cdr() != NIL) {
                            LispObject temp = tail;
                            tail.setCar(temp.cadr());
                            tail.setCdr(temp.cddr());
                        } else {
                            // Last item.
                            if (tail == list)
                                return NIL;
                            splice.setCdr(NIL);
                            return list;
                        }
                    } else {
                        splice = tail;
                        tail = tail.cdr();
                    }
                }
                if (tail == NIL)
                    return list;
                else
                    return type_error(tail, Symbol.LIST);
            } else if (list == NIL)
                return list;
            else
                return type_error(list, Symbol.LIST);
        }
    };

    // ### list-delete-eql item list => result-list
    static final Primitive LIST_DELETE_EQL = new pf_list_delete_eql();
    private static final class pf_list_delete_eql extends Primitive {
        pf_list_delete_eql() {
            super("list-delete-eql", PACKAGE_SYS, true, "item list");
        }

        @Override
        public LispObject execute(LispObject item, LispObject list)

        {
            if (list instanceof Cons) {
                LispObject tail = list;
                LispObject splice = list;
                while (tail instanceof Cons) {
                    LispObject car = tail.car();
                    if (car.eql(item)) {
                        if (tail.cdr() != NIL) {
                            LispObject temp = tail;
                            tail.setCar(temp.cadr());
                            tail.setCdr(temp.cddr());
                        } else {
                            // Last item.
                            if (tail == list)
                                return NIL;
                            splice.setCdr(NIL);
                            return list;
                        }
                    } else {
                        splice = tail;
                        tail = tail.cdr();
                    }
                }
                if (tail == NIL)
                    return list;
                else
                    return type_error(tail, Symbol.LIST);
            } else if (list == NIL)
                return list;
            else
                return type_error(list, Symbol.LIST);
        }
    };

    // ### vector-delete-eq item vector => result-vector
    private static final Primitive VECTOR_DELETE_EQ = new pf_vector_delete_eq();
    private static final class pf_vector_delete_eq extends Primitive {
        pf_vector_delete_eq() {
            super("vector-delete-eq", PACKAGE_SYS, true, "item vector");
        }

        @Override
        public LispObject execute(LispObject item, LispObject vector)

        {
            checkVector(vector).deleteEq(item);
            return vector;
        }
    };

    // ### vector-delete-eql item vector => result-vector
    private static final Primitive VECTOR_DELETE_EQL = new pf_vector_delete_eql();
    private static final class pf_vector_delete_eql extends Primitive {
        pf_vector_delete_eql() {
            super("vector-delete-eql", PACKAGE_SYS, true, "item vector");
        }

        @Override
        public LispObject execute(LispObject item, LispObject vector)

        {
            checkVector(vector).deleteEql(item);
            return vector;
        }
    };

    // ### %set-elt
    // %setelt sequence index newval => newval
    private static final Primitive _SET_ELT = new pf__set_elt();
    private static final class pf__set_elt extends Primitive {
        pf__set_elt() {
            super("%set-elt", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first instanceof AbstractVector) {
                ((AbstractVector)first).aset(Fixnum.getValue(second), third);
                return third;
            }
            if (first instanceof Cons) {
                int index = Fixnum.getValue(second);
                if (index < 0)
                    error(new TypeError());
                LispObject list = first;
                int i = 0;
                while (true) {
                    if (i == index) {
                        list.setCar(third);
                        return third;
                    }
                    list = list.cdr();
                    if (list == NIL)
                        error(new TypeError());
                    ++i;
                }
            }
            return type_error(first, Symbol.SEQUENCE);
        }
    };

    // ### %make-list
    private static final Primitive _MAKE_LIST = new pf__make_list();
    private static final class pf__make_list extends Primitive {
        pf__make_list() {
            super("%make-list", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            int size = Fixnum.getValue(first);
            if (size < 0)
                return type_error(first, list(Symbol.INTEGER, Fixnum.ZERO,
                                              Symbol.MOST_POSITIVE_FIXNUM.getSymbolValue()));
            LispObject result = NIL;
            for (int i = size; i-- > 0;)
                result = new Cons(second, result);
            return result;
        }
    };

    // ### %member item list key test test-not => tail
    private static final Primitive _MEMBER = new pf__member();
    private static final class pf__member extends Primitive {
        pf__member() {
            super("%member", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject item, LispObject list,
                                  LispObject key, LispObject test,
                                  LispObject testNot)

        {
            LispObject tail = checkList(list);
            if (test != NIL && testNot != NIL)
                error(new LispError("MEMBER: test and test-not both supplied"));
            if (testNot == NIL) {
                if (test == NIL || test == Symbol.EQL)
                    test = EQL;
            }
            if (key == NIL) {
                if (test == EQL) {
                    while (tail instanceof Cons) {
                        if (item.eql(((Cons)tail).car))
                            return tail;
                        tail = ((Cons)tail).cdr;
                    }
                } else if (test != NIL) {
                    while (tail instanceof Cons) {
                        LispObject candidate = ((Cons)tail).car;
                        if (test.execute(item, candidate) != NIL)
                            return tail;
                        tail = ((Cons)tail).cdr;
                    }
                } else {
                    // test == NIL
                    while (tail instanceof Cons) {
                        LispObject candidate = ((Cons)tail).car;
                        if (testNot.execute(item, candidate) == NIL)
                            return tail;
                        tail = ((Cons)tail).cdr;
                    }
                }
            } else {
                // key != NIL
                while (tail instanceof Cons) {
                    LispObject candidate = key.execute(((Cons)tail).car);
                    if (test != NIL) {
                        if (test.execute(item, candidate) != NIL)
                            return tail;
                    } else {
                        if (testNot.execute(item, candidate) == NIL)
                            return tail;
                    }
                    tail = ((Cons)tail).cdr;
                }
            }
            if (tail != NIL)
                type_error(tail, Symbol.LIST);
            return NIL;
        }
    };

    // ### funcall-key function-or-nil element
    private static final Primitive FUNCALL_KEY = new pf_funcall_key();
    private static final class pf_funcall_key extends Primitive {
        pf_funcall_key() {
            super("funcall-key", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first != NIL)
                return LispThread.currentThread().execute(first, second);
            return second;
        }
    };

    // ### coerce-to-function
    private static final Primitive COERCE_TO_FUNCTION = new pf_coerce_to_function();
    private static final class pf_coerce_to_function extends Primitive {
        pf_coerce_to_function() {
            super("coerce-to-function", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return coerceToFunction(arg);
        }
    };

    // ### make-closure lambda-form environment => closure
    private static final Primitive MAKE_CLOSURE = new pf_make_closure();
    private static final class pf_make_closure extends Primitive {
        pf_make_closure() {
            super("make-closure", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first instanceof Cons && ((Cons)first).car == Symbol.LAMBDA) {
                final Environment env;
                if (second == NIL)
                    env = new Environment();
                else
                    env = checkEnvironment(second);
                return new Closure(first, env);
            }
            return error(new TypeError("The argument to MAKE-CLOSURE is not a lambda form."));
        }
    };

    // ### streamp
    private static final Primitive STREAMP = new pf_streamp();
    private static final class pf_streamp extends Primitive {
        pf_streamp() {
            super(Symbol.STREAMP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof Stream ? T : NIL;
        }
    };

    // ### integerp
    private static final Primitive INTEGERP = new pf_integerp();
    private static final class pf_integerp extends Primitive {
        pf_integerp() {
            super(Symbol.INTEGERP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.INTEGERP();
        }
    };

    // ### evenp
    private static final Primitive EVENP = new pf_evenp();
    private static final class pf_evenp extends Primitive {
        pf_evenp() {
            super(Symbol.EVENP, "integer");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.EVENP();
        }
    };

    // ### oddp
    private static final Primitive ODDP = new pf_oddp();
    private static final class pf_oddp extends Primitive {
        pf_oddp() {
            super(Symbol.ODDP, "integer");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.ODDP();
        }
    };

    // ### numberp
    private static final Primitive NUMBERP = new pf_numberp();
    private static final class pf_numberp extends Primitive {
        pf_numberp() {
            super(Symbol.NUMBERP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.NUMBERP();
        }
    };

    // ### realp
    private static final Primitive REALP = new pf_realp();
    private static final class pf_realp extends Primitive {
        pf_realp() {
            super(Symbol.REALP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.REALP();
        }
    };

    // ### rationalp
    private static final Primitive RATIONALP = new pf_rationalp();
    private static final class pf_rationalp extends Primitive {
        pf_rationalp() {
            super(Symbol.RATIONALP,"object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.RATIONALP();
        }
    };

    // ### complex
    private static final Primitive COMPLEX = new pf_complex();
    private static final class pf_complex extends Primitive {
        pf_complex() {
            super(Symbol.COMPLEX, "realpart &optional imagpart");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof SingleFloat)
                return Complex.getInstance(arg, SingleFloat.ZERO);
            if (arg instanceof DoubleFloat)
                return Complex.getInstance(arg, DoubleFloat.ZERO);
            if (arg.realp())
                return arg;
            return type_error(arg, Symbol.REAL);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return Complex.getInstance(first, second);
        }
    };

    // ### complexp
    private static final Primitive COMPLEXP = new pf_complexp();
    private static final class pf_complexp extends Primitive {
        pf_complexp() {
            super(Symbol.COMPLEXP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.COMPLEXP();
        }
    };

    // ### numerator
    private static final Primitive NUMERATOR = new pf_numerator();
    private static final class pf_numerator extends Primitive {
        pf_numerator() {
            super(Symbol.NUMERATOR, "rational");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.NUMERATOR();
        }
    };

    // ### denominator
    private static final Primitive DENOMINATOR = new pf_denominator();
    private static final class pf_denominator extends Primitive {
        pf_denominator() {
            super(Symbol.DENOMINATOR, "rational");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.DENOMINATOR();
        }
    };

    // ### realpart
    private static final Primitive REALPART = new pf_realpart();
    private static final class pf_realpart extends Primitive {
        pf_realpart() {
            super(Symbol.REALPART, "number");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Complex)
                return ((Complex)arg).getRealPart();
            if (arg.numberp())
                return arg;
            return type_error(arg, Symbol.NUMBER);
        }
    };

    // ### imagpart
    private static final Primitive IMAGPART = new pf_imagpart();
    private static final class pf_imagpart extends Primitive {
        pf_imagpart() {
            super(Symbol.IMAGPART, "number");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Complex)
                return ((Complex)arg).getImaginaryPart();
            return arg.multiplyBy(Fixnum.ZERO);
        }
    };

    // ### integer-length
    private static final Primitive INTEGER_LENGTH = new pf_integer_length();
    private static final class pf_integer_length extends Primitive {
        pf_integer_length() {
            super(Symbol.INTEGER_LENGTH, "integer");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Fixnum) {
                int n = ((Fixnum)arg).value;
                if (n < 0)
                    n = ~n;
                int count = 0;
                while (n > 0) {
                    n = n >>> 1;
                    ++count;
                }
                return Fixnum.getInstance(count);
            }
            if (arg instanceof Bignum)
                return Fixnum.getInstance(((Bignum)arg).value.bitLength());
            return type_error(arg, Symbol.INTEGER);
        }
    };

    // ### gcd-2
    private static final Primitive GCD_2 = new pf_gcd_2();
    private static final class pf_gcd_2 extends Primitive {
        pf_gcd_2() {
            super("gcd-2", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            BigInteger n1, n2;
            if (first instanceof Fixnum)
                n1 = BigInteger.valueOf(((Fixnum)first).value);
            else if (first instanceof Bignum)
                n1 = ((Bignum)first).value;
            else
                return type_error(first, Symbol.INTEGER);
            if (second instanceof Fixnum)
                n2 = BigInteger.valueOf(((Fixnum)second).value);
            else if (second instanceof Bignum)
                n2 = ((Bignum)second).value;
            else
                return type_error(second, Symbol.INTEGER);
            return number(n1.gcd(n2));
        }
    };

    // ### identity-hash-code
    private static final Primitive IDENTITY_HASH_CODE = new pf_identity_hash_code();
    private static final class pf_identity_hash_code extends Primitive {
        pf_identity_hash_code() {
            super("identity-hash-code", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return Fixnum.getInstance(System.identityHashCode(arg));
        }
    };

    // ### simple-vector-search pattern vector => position
    // Searches vector for pattern.
    private static final Primitive SIMPLE_VECTOR_SEARCH = new pf_simple_vector_search();
    private static final class pf_simple_vector_search extends Primitive {
        pf_simple_vector_search() {
            super("simple-vector-search", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            AbstractVector v = checkVector(second);
            if (first.length() == 0)
                return Fixnum.ZERO;
            final int patternLength = first.length();
            final int limit = v.length() - patternLength;
            if (first instanceof AbstractVector) {
                AbstractVector pattern = (AbstractVector) first;
                LispObject element = pattern.AREF(0);
                for (int i = 0; i <= limit; i++) {
                    if (v.AREF(i).eql(element)) {
                        // Found match for first element of pattern.
                        boolean match = true;
                        // We've already checked the first element.
                        int j = i + 1;
                        for (int k = 1; k < patternLength; k++) {
                            if (v.AREF(j).eql(pattern.AREF(k))) {
                                ++j;
                            } else {
                                match = false;
                                break;
                            }
                        }
                        if (match)
                            return Fixnum.getInstance(i);
                    }
                }
            } else {
                // Pattern is a list.
                LispObject element = first.car();
                for (int i = 0; i <= limit; i++) {
                    if (v.AREF(i).eql(element)) {
                        // Found match for first element of pattern.
                        boolean match = true;
                        // We've already checked the first element.
                        int j = i + 1;
                        for (LispObject rest = first.cdr(); rest != NIL; rest = rest.cdr()) {
                            if (v.AREF(j).eql(rest.car())) {
                                ++j;
                            } else {
                                match = false;
                                break;
                            }
                        }
                        if (match)
                            return Fixnum.getInstance(i);
                    }
                }
            }
            return NIL;
        }
    };

    // ### uptime
    private static final Primitive UPTIME = new pf_uptime();
    private static final class pf_uptime extends Primitive {
        pf_uptime() {
            super("uptime", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute() {
            return number(System.currentTimeMillis() - Main.startTimeMillis);
        }
    };

    // ### built-in-function-p
    private static final Primitive BUILT_IN_FUNCTION_P = new pf_built_in_function_p();
    private static final class pf_built_in_function_p extends Primitive {
        pf_built_in_function_p() {
            super("built-in-function-p", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkSymbol(arg).isBuiltInFunction() ? T : NIL;
        }
    };

    // ### inspected-parts
    private static final Primitive INSPECTED_PARTS = new pf_inspected_parts();
    private static final class pf_inspected_parts extends Primitive {
        pf_inspected_parts() {
            super("inspected-parts", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.getParts();
        }
    };

    // ### inspected-description
    private static final Primitive INSPECTED_DESCRIPTION = new pf_inspected_description();
    private static final class pf_inspected_description extends Primitive {
        pf_inspected_description() {
            super("inspected-description", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.getDescription();
        }
    };

    // ### symbol-name
    public static final Primitive SYMBOL_NAME = new pf_symbol_name();
    private static final class pf_symbol_name extends Primitive {
        pf_symbol_name() {
            super(Symbol.SYMBOL_NAME, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkSymbol(arg).name;
        }
    };

    // ### symbol-package
    public static final Primitive SYMBOL_PACKAGE = new pf_symbol_package();
    private static final class pf_symbol_package extends Primitive {
        pf_symbol_package() {
            super(Symbol.SYMBOL_PACKAGE, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkSymbol(arg).getPackage();
        }
    };

    // ### symbol-function
    public static final Primitive SYMBOL_FUNCTION = new pf_symbol_function();
    private static final class pf_symbol_function extends Primitive {
        pf_symbol_function() {
            super(Symbol.SYMBOL_FUNCTION, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            LispObject function = checkSymbol(arg).getSymbolFunction();
            if (function != null)
                return function;
            return error(new UndefinedFunction(arg));

        }
    };

    // ### %set-symbol-function
    public static final Primitive _SET_SYMBOL_FUNCTION = new pf__set_symbol_function();
    private static final class pf__set_symbol_function extends Primitive {
        pf__set_symbol_function() {
            super("%set-symbol-function", PACKAGE_SYS, false, "symbol function");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            checkSymbol(first).setSymbolFunction(second);
            return second;
        }
    };

    // ### symbol-plist
    public static final Primitive SYMBOL_PLIST = new pf_symbol_plist();
    private static final class pf_symbol_plist extends Primitive {
        pf_symbol_plist() {
            super(Symbol.SYMBOL_PLIST, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkSymbol(arg).getPropertyList();
        }
    };

    // ### keywordp
    public static final Primitive KEYWORDP = new pf_keywordp();
    private static final class pf_keywordp extends Primitive {
        pf_keywordp() {
            super(Symbol.KEYWORDP, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Symbol) {
                if (checkSymbol(arg).getPackage() == PACKAGE_KEYWORD)
                    return T;
            }
            return NIL;
        }
    };

    // ### make-symbol
    public static final Primitive MAKE_SYMBOL = new pf_make_symbol();
    private static final class pf_make_symbol extends Primitive {
        pf_make_symbol() {
            super(Symbol.MAKE_SYMBOL, "name");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof SimpleString)
                return new Symbol((SimpleString)arg);
            // Not a simple string.
            if (arg instanceof AbstractString)
                return new Symbol(arg.getStringValue());
            return type_error(arg, Symbol.STRING);
        }
    };

    // ### makunbound
    public static final Primitive MAKUNBOUND = new pf_makunbound();
    private static final class pf_makunbound extends Primitive {
        pf_makunbound() {
            super(Symbol.MAKUNBOUND, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            checkSymbol(arg).setSymbolValue(null);
            return arg;
        }
    };

    // ### %class-name
    private static final Primitive _CLASS_NAME = new pf__class_name();
    private static final class pf__class_name extends Primitive {
        pf__class_name() {
            super("%class-name", PACKAGE_SYS, true, "class");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof LispClass)
                return ((LispClass)arg).getName();

            return ((StandardObject)arg).getInstanceSlotValue(StandardClass.symName);
        }
    };

    // ### %set-class-name
    private static final Primitive _SET_CLASS_NAME = new pf__set_class_name();
    private static final class pf__set_class_name extends Primitive {
        pf__set_class_name() {
            super("%set-class-name", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (second instanceof LispClass)
                ((LispClass)second).setName(checkSymbol(first));
            else
                ((StandardObject)second).setInstanceSlotValue(StandardClass.symName,
                                                           checkSymbol(first));
            return first;
        }
    };

    // ### class-layout
    private static final Primitive CLASS_LAYOUT = new pf__class_layout();
    private static final class pf__class_layout extends Primitive {
        pf__class_layout() {
            super("%class-layout", PACKAGE_SYS, true, "class");
        }

        @Override
        public LispObject execute(LispObject arg) {
            Layout layout;
            if (arg instanceof LispClass)
              layout = ((LispClass)arg).getClassLayout();
            else
              layout = (Layout)((StandardObject)arg).getInstanceSlotValue(StandardClass.symLayout);

            return layout != null ? layout : NIL;
        }
    };

    // ### %set-class-layout
    private static final Primitive _SET_CLASS_LAYOUT = new pf__set_class_layout();
    private static final class pf__set_class_layout extends Primitive {
        pf__set_class_layout() {
            super("%set-class-layout", PACKAGE_SYS, true, "class layout");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first == NIL || first instanceof Layout) {
                if (second instanceof LispClass)
                  ((LispClass)second).setClassLayout(first);
                else
                  ((StandardObject)second).setInstanceSlotValue(StandardClass.symLayout, first);
                return first;
            }
            return type_error(first, Symbol.LAYOUT);
        }
    };

    // ### %class-direct-superclasses
    private static final Primitive _CLASS_DIRECT_SUPERCLASSES = new pf__class_direct_superclasses();
    private static final class pf__class_direct_superclasses extends Primitive {
        pf__class_direct_superclasses() {
            super("%class-direct-superclasses", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof LispClass)
              return ((LispClass)arg).getDirectSuperclasses();
            else
              return ((StandardObject)arg).getInstanceSlotValue(StandardClass.symDirectSuperclasses);
        }
    };

    // ### %set-class-direct-superclasses
    private static final Primitive _SET_CLASS_DIRECT_SUPERCLASSES = new pf__set_class_direct_superclasses();
    private static final class pf__set_class_direct_superclasses extends Primitive {
        pf__set_class_direct_superclasses() {
            super("%set-class-direct-superclasses", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof LispClass)
              ((LispClass)second).setDirectSuperclasses(first);
            else
              ((StandardObject)second).setInstanceSlotValue(StandardClass.symDirectSuperclasses, first);
            return first;
        }
    };

    // ### %class-direct-subclasses
    private static final Primitive _CLASS_DIRECT_SUBCLASSES = new pf__class_direct_subclasses();
    private static final class pf__class_direct_subclasses extends Primitive {
        pf__class_direct_subclasses() {
            super("%class-direct-subclasses", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof LispClass)
                return ((LispClass)arg).getDirectSubclasses();
            else
                return ((StandardObject)arg).getInstanceSlotValue(StandardClass.symDirectSubclasses);
        }
    };

    // ### %set-class-direct-subclasses
    private static final Primitive _SET_CLASS_DIRECT_SUBCLASSES = new pf__set_class_direct_subclasses();
    private static final class pf__set_class_direct_subclasses extends Primitive {
        pf__set_class_direct_subclasses() {
            super("%set-class-direct-subclasses", PACKAGE_SYS, true,
                  "class direct-subclasses");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof LispClass)
                ((LispClass)second).setDirectSubclasses(first);
            else
                ((StandardObject)second).setInstanceSlotValue(StandardClass.symDirectSubclasses, first);
            return first;
        }
    };

    // ### %class-precedence-list
    private static final Primitive _CLASS_PRECEDENCE_LIST = new pf__class_precedence_list();
    private static final class pf__class_precedence_list extends Primitive {
        pf__class_precedence_list() {
            super("%class-precedence-list", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof LispClass)
                return ((LispClass)arg).getCPL();
            else
                return ((StandardObject)arg).getInstanceSlotValue(StandardClass.symPrecedenceList);
        }
    };

    // ### %set-class-precedence-list
    private static final Primitive _SET_CLASS_PRECEDENCE_LIST = new pf__set_class_precedence_list();
    private static final class pf__set_class_precedence_list extends Primitive {
        pf__set_class_precedence_list() {
            super("%set-class-precedence-list", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof LispClass)
                ((LispClass)second).setCPL(first);
            else
                ((StandardObject)second).setInstanceSlotValue(StandardClass.symPrecedenceList, first);
            return first;
        }
    };

    // ### %class-direct-methods
    private static final Primitive _CLASS_DIRECT_METHODS = new pf__class_direct_methods();
    private static final class pf__class_direct_methods extends Primitive {
        pf__class_direct_methods() {
            super("%class-direct-methods", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispClass)
                return ((LispClass)arg).getDirectMethods();
            else
                return ((StandardObject)arg).getInstanceSlotValue(StandardClass.symDirectMethods);
        }
    };

    // ### %set-class-direct-methods
    private static final Primitive _SET_CLASS_DIRECT_METHODS = new pf__set_class_direct_methods();
    private static final class pf__set_class_direct_methods extends Primitive {
        pf__set_class_direct_methods() {
            super("%set-class-direct-methods", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof LispClass)
                ((LispClass)second).setDirectMethods(first);
            else
                ((StandardObject)second).setInstanceSlotValue(StandardClass.symDirectMethods, first);
            return first;
        }
    };

    // ### class-documentation
    private static final Primitive CLASS_DOCUMENTATION
        = new pf_class_documentation();
    private static final class pf_class_documentation extends Primitive {
        pf_class_documentation() {
            super("class-documentation", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg)

        {
            if (arg instanceof LispClass)
                return ((LispClass)arg).getDocumentation();
            else
                return ((StandardObject)arg).getInstanceSlotValue(StandardClass.symDocumentation);
        }
    };

    // ### %set-class-documentation
    private static final Primitive _SET_CLASS_DOCUMENTATION
        = new pf__set_class_documentation();
    private static final class pf__set_class_documentation extends Primitive {
        pf__set_class_documentation() {
            super("%set-class-documentation", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (first instanceof LispClass)
                ((LispClass)first).setDocumentation(second);
            else
                ((StandardObject)first).setInstanceSlotValue(StandardClass.symDocumentation, second);
            return second;
        }
    };

    // ### %class-finalized-p
    private static final Primitive _CLASS_FINALIZED_P = new pf__class_finalized_p();
    private static final class pf__class_finalized_p extends Primitive {
        pf__class_finalized_p() {
            super("%class-finalized-p", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof LispClass)
                return ((LispClass)arg).isFinalized() ? T : NIL;
            else
                return ((StandardObject)arg).getInstanceSlotValue(StandardClass.symFinalizedP);
        }
    };

    // ### %set-class-finalized-p
    private static final Primitive _SET_CLASS_FINALIZED_P = new pf__set_class_finalized_p();
    private static final class pf__set_class_finalized_p extends Primitive {
        pf__set_class_finalized_p() {
            super("%set-class-finalized-p", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)
        {
            if (second instanceof LispClass)
                ((LispClass)second).setFinalized(first != NIL);
            else
                ((StandardObject)second).setInstanceSlotValue(StandardClass.symFinalizedP, first);
            return first;
        }
    };

    // ### classp
    private static final Primitive CLASSP = new pf_classp();
    private static final class pf_classp extends Primitive {
        pf_classp() {
            super("classp", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            return (arg instanceof LispClass) ? T : arg.typep(Symbol.CLASS);
        }
    };

    // ### char-to-utf8 char => octets
    private static final Primitive CHAR_TO_UTF8 = new pf_char_to_utf8();
    private static final class pf_char_to_utf8 extends Primitive {
        pf_char_to_utf8() {
            super("char-to-utf8", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject arg) {
            final LispCharacter c;
            c = checkCharacter( arg);
            char[] chars = new char[1];
            chars[0] = c.value;
            String s = new String(chars);
            final byte[] bytes;
            try {
                bytes = s.getBytes("UTF8");
            } catch (java.io.UnsupportedEncodingException e) {
                return error(new LispError("UTF8 is not a supported encoding."));
            }
            LispObject[] objects = new LispObject[bytes.length];
            for (int i = bytes.length; i-- > 0;) {
                int n = bytes[i];
                if (n < 0)
                    n += 256;
                objects[i] = Fixnum.getInstance(n);
            }
            return new SimpleVector(objects);
        }
    };

    // ### %documentation
    private static final Primitive _DOCUMENTATION = new pf__documentation();
    private static final class pf__documentation extends Primitive {
        pf__documentation() {
            super("%documentation", PACKAGE_SYS, true,
                  "object doc-type");
        }

        @Override
        public LispObject execute(LispObject object, LispObject docType)

        {
            LispObject doc = object.getDocumentation(docType);
            if (doc == NIL) {
                if (docType == Symbol.FUNCTION && object instanceof Symbol) {
                    LispObject function = object.getSymbolFunction();
                    if (function != null)
                        doc = function.getDocumentation(docType);
                }
            }
            return doc;
        }
    };

    // ### %set-documentation
    private static final Primitive _SET_DOCUMENTATION = new pf__set_documentation();
    private static final class pf__set_documentation extends Primitive {
        pf__set_documentation() {
            super("%set-documentation", PACKAGE_SYS, true,
                  "object doc-type documentation");
        }

        @Override
        public LispObject execute(LispObject object, LispObject docType,
                                  LispObject documentation)

        {
            object.setDocumentation(docType, documentation);
            return documentation;
        }
    };

    // ### %putf
    private static final Primitive _PUTF = new pf__putf();
    private static final class pf__putf extends Primitive {
        pf__putf() {
            super("%putf", PACKAGE_SYS, true,
                  "plist indicator new-value");
        }

        @Override
        public LispObject execute(LispObject plist, LispObject indicator,
                                  LispObject newValue)

        {
            return putf(plist, indicator, newValue);
        }
    };

    // ### function-plist
    private static final Primitive FUNCTION_PLIST = new pf_function_plist();
    private static final class pf_function_plist extends Primitive {
        pf_function_plist() {
            super("function-plist", PACKAGE_SYS, true, "function");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return checkFunction(arg).getPropertyList();
        }
    };

    // ### make-keyword
    private static final Primitive MAKE_KEYWORD = new pf_make_keyword();
    private static final class pf_make_keyword extends Primitive {
        pf_make_keyword() {
            super("make-keyword", PACKAGE_SYS, true, "symbol");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return PACKAGE_KEYWORD.intern(checkSymbol(arg).name);
        }
    };

    // ### standard-object-p object => generalized-boolean
    private static final Primitive STANDARD_OBJECT_P = new pf_standard_object_p();
    private static final class pf_standard_object_p extends Primitive {
        pf_standard_object_p() {
            super("standard-object-p", PACKAGE_SYS, true, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg instanceof StandardObject ? T : NIL;
        }
    };

    // ### copy-tree
    private static final Primitive COPY_TREE = new pf_copy_tree();
    private static final class pf_copy_tree extends Primitive {
        pf_copy_tree() {
            super(Symbol.COPY_TREE, "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            if (arg instanceof Cons) {
                Cons cons = (Cons) arg;
                return new Cons(execute(cons.car), execute(cons.cdr));
            } else
                return arg;
        }
    };

    /* Added to ABCL because Maxima wants to be able to turn off
     * underflow conditions. However, the Hyperspec says we have to
     * signal them. So, we went for CLHS compliant with a switch for
     * Maxima.
     */
    // ### float-underflow-mode
    private static final Primitive FLOAT_UNDERFLOW_MODE
        = new pf_float_underflow_mode();
    private static final class pf_float_underflow_mode extends Primitive {
        pf_float_underflow_mode() {
            super(Symbol.FLOAT_UNDERFLOW_MODE, "&optional boolean");
        }

        @Override
        public LispObject execute() {
            return Lisp.TRAP_UNDERFLOW ? T : NIL;
        }

        @Override
        public LispObject execute(LispObject arg) {
            Lisp.TRAP_UNDERFLOW = (arg != NIL);
            return arg;
        }
    };

    /* Implemented for symmetry with the underflow variant. */
    // ### float-overflow-mode
    private static final Primitive FLOAT_OVERFLOW_MODE
        = new pf_float_overflow_mode();
    private static final class pf_float_overflow_mode extends Primitive {
        pf_float_overflow_mode() {
            super(Symbol.FLOAT_OVERFLOW_MODE, "&optional boolean");
        }

        @Override
        public LispObject execute() {
            return Lisp.TRAP_OVERFLOW ? T : NIL;
        }

        @Override
        public LispObject execute(LispObject arg) {
            Lisp.TRAP_OVERFLOW = (arg != NIL);
            return arg;
        }
    };

}
