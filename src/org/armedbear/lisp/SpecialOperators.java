/*
 * SpecialOperators.java
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

import java.util.ArrayList;
import java.util.LinkedList;
public final class SpecialOperators {
    // ### quote
    private static final SpecialOperator QUOTE = new sf_quote();
    private static final class sf_quote extends SpecialOperator {
        sf_quote() {
            super(Symbol.QUOTE, "thing");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.cdr() != NIL)
                return error(new WrongNumberOfArgumentsException(this));
            return args.car();
        }
    };

    // ### if
    private static final SpecialOperator IF = new sf_if();
    private static final class sf_if extends SpecialOperator {
        sf_if() {
            super(Symbol.IF, "test then &optional else");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispThread thread = LispThread.currentThread();
            switch (args.length()) {
            case 2: {
                if (eval(((Cons)args).car, env, thread) != NIL)
                    return eval(args.cadr(), env, thread);
                thread.clearValues();
                return NIL;
            }
            case 3: {
                if (eval(((Cons)args).car, env, thread) != NIL)
                    return eval(args.cadr(), env, thread);
                return eval((((Cons)args).cdr).cadr(), env, thread);
            }
            default:
                return error(new WrongNumberOfArgumentsException(this));
            }
        }
    };

    // ### let
    private static final SpecialOperator LET = new sf_let();
    private static final class sf_let extends SpecialOperator {
        sf_let() {
            super(Symbol.LET, "bindings &body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args == NIL)
                return error(new WrongNumberOfArgumentsException(this));
            return _let(args, env, false);
        }
    };

    // ### let*
    private static final SpecialOperator LET_STAR = new sf_let_star();
    private static final class sf_let_star extends SpecialOperator {
        sf_let_star() {
            super(Symbol.LET_STAR, "bindings &body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args == NIL)
                return error(new WrongNumberOfArgumentsException(this));
            return _let(args, env, true);
        }
    };

    static final LispObject _let(LispObject args, Environment env,
                                         boolean sequential)

    {
        final LispThread thread = LispThread.currentThread();
        final SpecialBindingsMark mark = thread.markSpecialBindings();
        try {
            LispObject varList = checkList(args.car());
            LispObject bodyAndDecls = parseBody(args.cdr(), false);
            LispObject specials = parseSpecials(bodyAndDecls.NTH(1));
            LispObject body = bodyAndDecls.car();

            Environment ext = new Environment(env);
            LinkedList<Cons> nonSequentialVars = new LinkedList<Cons>();
            while (varList != NIL) {
                final Symbol symbol;
                LispObject value;
                LispObject obj = varList.car();
                if (obj instanceof Cons) {
                    if (obj.length() > 2)
                        return error(new LispError("The " + (sequential ? "LET*" : "LET")
                                                   + " binding specification " +
                                                   obj.writeToString() + " is invalid."));
                    symbol = checkSymbol(((Cons)obj).car);
                    value = eval(obj.cadr(), sequential ? ext : env, thread);
                } else {
                    symbol = checkSymbol(obj);
                    value = NIL;
                }
                if (sequential) {
                    ext = new Environment(ext);
                    bindArg(specials, symbol, value, ext, thread);
                } else
                    nonSequentialVars.add(new Cons(symbol, value));
                varList = ((Cons)varList).cdr;
            }
            if (!sequential)
for (Cons x : nonSequentialVars)
                    bindArg(specials, (Symbol)x.car(), x.cdr(), ext, thread);

            // Make sure free special declarations are visible in the body.
            // "The scope of free declarations specifically does not include
            // initialization forms for bindings established by the form
            // containing the declarations." (3.3.4)
            for (; specials != NIL; specials = specials.cdr())
                ext.declareSpecial((Symbol)specials.car());

            return progn(body, ext, thread);
        }
        finally {
            thread.resetSpecialBindings(mark);
        }
    }

    // ### symbol-macrolet
    private static final SpecialOperator SYMBOL_MACROLET = new sf_symbol_macrolet();
    private static final class sf_symbol_macrolet extends SpecialOperator {
        sf_symbol_macrolet() {
            super(Symbol.SYMBOL_MACROLET, "macrobindings &body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            LispObject varList = checkList(args.car());
            final LispThread thread = LispThread.currentThread();
            final SpecialBindingsMark mark = thread.markSpecialBindings();
            Environment ext = new Environment(env);
            try {
                // Declare our free specials, this will correctly raise
                LispObject body = ext.processDeclarations(args.cdr());

                for (int i = varList.length(); i-- > 0;) {
                    LispObject obj = varList.car();
                    varList = varList.cdr();
                    if (obj instanceof Cons && obj.length() == 2) {
                        Symbol symbol = checkSymbol(obj.car());
                        if (symbol.isSpecialVariable()
                                || ext.isDeclaredSpecial(symbol)) {
                            return error(new ProgramError(
                                             "Attempt to bind the special variable " +
                                             symbol.writeToString() +
                                             " with SYMBOL-MACROLET."));
                        }
                        bindArg(null, symbol, new SymbolMacro(obj.cadr()), ext, thread);
                    } else {
                        return error(new ProgramError(
                                         "Malformed symbol-expansion pair in SYMBOL-MACROLET: " +
                                         obj.writeToString()));
                    }
                }
                return progn(body, ext, thread);
            }
            finally {
                thread.resetSpecialBindings(mark);
            }
        }
    };

    // ### load-time-value form &optional read-only-p => object
    private static final SpecialOperator LOAD_TIME_VALUE = new sf_load_time_value();
    private static final class sf_load_time_value extends SpecialOperator {
        sf_load_time_value() {
            super(Symbol.LOAD_TIME_VALUE,
                  "form &optional read-only-p");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            switch (args.length()) {
            case 1:
            case 2:
                return eval(args.car(), new Environment(),
                            LispThread.currentThread());
            default:
                return error(new WrongNumberOfArgumentsException(this));
            }
        }
    };

    // ### locally
    private static final SpecialOperator LOCALLY = new sf_locally();
    private static final class sf_locally extends SpecialOperator {
        sf_locally() {
            super(Symbol.LOCALLY, "&body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispThread thread = LispThread.currentThread();
            final Environment ext = new Environment(env);
            args = ext.processDeclarations(args);
            return progn(args, ext, thread);
        }
    };

    // ### progn
    private static final SpecialOperator PROGN = new sf_progn();
    private static final class sf_progn extends SpecialOperator {
        sf_progn() {
            super(Symbol.PROGN, "&rest forms");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            LispThread thread = LispThread.currentThread();
            return progn(args, env, thread);
        }
    };

    // ### flet
    private static final SpecialOperator FLET = new sf_flet();
    private static final class sf_flet extends SpecialOperator {
        sf_flet() {
            super(Symbol.FLET, "definitions &body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            return _flet(args, env, false);
        }
    };

    // ### labels
    private static final SpecialOperator LABELS = new sf_labels();
    private static final class sf_labels extends SpecialOperator {
        sf_labels() {
            super(Symbol.LABELS, "definitions &body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            return _flet(args, env, true);
        }
    };

    static final LispObject _flet(LispObject args, Environment env,
                                          boolean recursive)

    {
        // First argument is a list of local function definitions.
        LispObject defs = checkList(args.car());
        final LispThread thread = LispThread.currentThread();
        final SpecialBindingsMark mark = thread.markSpecialBindings();
        final Environment funEnv = new Environment(env);
        while (defs != NIL) {
            final LispObject def = checkList(defs.car());
            final LispObject name = def.car();
            final Symbol symbol;
            if (name instanceof Symbol) {
                symbol = checkSymbol(name);
                if (symbol.getSymbolFunction() instanceof SpecialOperator) {
                    String message =
                        symbol.getName() + " is a special operator and may not be redefined";
                    return error(new ProgramError(message));
                }
            } else if (isValidSetfFunctionName(name))
                symbol = checkSymbol(name.cadr());
            else
                return type_error(name, FUNCTION_NAME);
            LispObject rest = def.cdr();
            LispObject parameters = rest.car();
            LispObject body = rest.cdr();
            LispObject decls = NIL;
            while (body.car() instanceof Cons && body.car().car() == Symbol.DECLARE) {
                decls = new Cons(body.car(), decls);
                body = body.cdr();
            }
            body = new Cons(symbol, body);
            body = new Cons(Symbol.BLOCK, body);
            body = new Cons(body, NIL);
            while (decls != NIL) {
                body = new Cons(decls.car(), body);
                decls = decls.cdr();
            }
            LispObject lambda_expression =
                new Cons(Symbol.LAMBDA, new Cons(parameters, body));
            LispObject lambda_name =
                list(recursive ? Symbol.LABELS : Symbol.FLET, name);
            Closure closure =
                new Closure(lambda_name, lambda_expression,
                            recursive ? funEnv : env);
            funEnv.addFunctionBinding(name, closure);
            defs = defs.cdr();
        }
        try {
            final Environment ext = new Environment(funEnv);
            LispObject body = args.cdr();
            body = ext.processDeclarations(body);
            return progn(body, ext, thread);
        }
        finally {
            thread.resetSpecialBindings(mark);
        }
    }

    // ### the value-type form => result*
    private static final SpecialOperator THE = new sf_the();
    private static final class sf_the extends SpecialOperator {
        sf_the() {
            super(Symbol.THE, "type value");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() != 2)
                return error(new WrongNumberOfArgumentsException(this));
            LispObject rv = eval(args.cadr(), env, LispThread.currentThread());

            // check only the most simple types: single symbols
            // (class type specifiers/primitive types)
            // DEFTYPE-d types need expansion;
            // doing so would slow down our execution too much

            // An implementation is allowed not to check the type,
            // the fact that we do so here is mainly driven by the
            // requirement to verify argument types in structure-slot
            // accessors (defstruct.lisp)

            // The policy below is in line with the level of verification
            // in the compiler at *safety* levels below 3
            LispObject type = args.car();
            if ((type instanceof Symbol
                    && get(type, Symbol.DEFTYPE_DEFINITION) == NIL)
                    || type instanceof BuiltInClass)
                if (rv.typep(type) == NIL)
                    type_error(rv, type);

            return rv;
        }
    };

    // ### progv
    private static final SpecialOperator PROGV = new sf_progv();
    private static final class sf_progv extends SpecialOperator {
        sf_progv() {
            super(Symbol.PROGV, "symbols values &body body");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            if (args.length() < 2)
                return error(new WrongNumberOfArgumentsException(this));
            final LispThread thread = LispThread.currentThread();
            final LispObject symbols = checkList(eval(args.car(), env, thread));
            LispObject values = checkList(eval(args.cadr(), env, thread));
            final SpecialBindingsMark mark = thread.markSpecialBindings();
            try {
                // Set up the new bindings.
                progvBindVars(symbols, values, thread);
                // Implicit PROGN.
                return progn(args.cdr().cdr(), env, thread);
            }
            finally {
                thread.resetSpecialBindings(mark);
            }
        }
    };

    // ### declare
    private static final SpecialOperator DECLARE = new sf_declare();
    private static final class sf_declare extends SpecialOperator {
        sf_declare() {
            super(Symbol.DECLARE, "&rest declaration-specifiers");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            return NIL;
        }
    };

    // ### function
    private static final SpecialOperator FUNCTION = new sf_function();
    private static final class sf_function extends SpecialOperator {
        sf_function() {
            super(Symbol.FUNCTION, "thing");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            final LispObject arg = args.car();
            if (arg instanceof Symbol) {
                LispObject operator = env.lookupFunction(arg);
                if (operator instanceof Autoload) {
                    Autoload autoload = (Autoload) operator;
                    autoload.load();
                    operator = autoload.getSymbol().getSymbolFunction();
                }
                if (operator instanceof Function)
                    return operator;
                if (operator instanceof StandardGenericFunction)
                    return operator;
                return error(new UndefinedFunction(arg));
            }
            if (arg instanceof Cons) {
                LispObject car = ((Cons)arg).car;
                if (car == Symbol.SETF) {
                    LispObject f = env.lookupFunction(arg);
                    if (f != null)
                        return f;
                    Symbol symbol = checkSymbol(arg.cadr());
                    f = get(symbol, Symbol.SETF_FUNCTION, null);
                    if (f != null)
                        return f;
                    f = get(symbol, Symbol.SETF_INVERSE, null);
                    if (f != null)
                        return f;
                }
                if (car == Symbol.LAMBDA)
                    return new Closure(arg, env);
                if (car == Symbol.NAMED_LAMBDA) {
                    LispObject name = arg.cadr();
                    if (name instanceof Symbol || isValidSetfFunctionName(name)) {
                        return new Closure(name,
                                           new Cons(Symbol.LAMBDA, arg.cddr()),
                                           env);
                    }
                    return type_error(name, FUNCTION_NAME);
                }
            }
            return error(new UndefinedFunction(list(Keyword.NAME, arg)));
        }
    };

    // ### setq
    private static final SpecialOperator SETQ = new sf_setq();
    private static final class sf_setq extends SpecialOperator {
        sf_setq() {
            super(Symbol.SETQ, "&rest vars-and-values");
        }

        @Override
        public LispObject execute(LispObject args, Environment env)

        {
            LispObject value = Nil.NIL;
            final LispThread thread = LispThread.currentThread();
            while (args != NIL) {
                Symbol symbol = checkSymbol(args.car());
                if (symbol.isConstant()) {
                    return error(new ProgramError(symbol.writeToString() +
                                                  " is a constant and thus cannot be set."));
                }
                args = args.cdr();
                if (symbol.isSpecialVariable() || env.isDeclaredSpecial(symbol)) {
                    SpecialBinding binding = thread.getSpecialBinding(symbol);
                    if (binding != null) {
                        if (binding.value instanceof SymbolMacro) {
                            LispObject expansion =
                                ((SymbolMacro)binding.value).getExpansion();
                            LispObject form = list(Symbol.SETF, expansion, args.car());
                            value = eval(form, env, thread);
                        } else {
                            value = eval(args.car(), env, thread);
                            binding.value = value;
                        }
                    } else {
                        if (symbol.getSymbolValue() instanceof SymbolMacro) {
                            LispObject expansion =
                                ((SymbolMacro)symbol.getSymbolValue()).getExpansion();
                            LispObject form = list(Symbol.SETF, expansion, args.car());
                            value = eval(form, env, thread);
                        } else {
                            value = eval(args.car(), env, thread);
                            symbol.setSymbolValue(value);
                        }
                    }
                } else {
                    // Not special.
                    Binding binding = env.getBinding(symbol);
                    if (binding != null) {
                        if (binding.value instanceof SymbolMacro) {
                            LispObject expansion =
                                ((SymbolMacro)binding.value).getExpansion();
                            LispObject form = list(Symbol.SETF, expansion, args.car());
                            value = eval(form, env, thread);
                        } else {
                            value = eval(args.car(), env, thread);
                            binding.value = value;
                        }
                    } else {
                        if (symbol.getSymbolValue() instanceof SymbolMacro) {
                            LispObject expansion =
                                ((SymbolMacro)symbol.getSymbolValue()).getExpansion();
                            LispObject form = list(Symbol.SETF, expansion, args.car());
                            value = eval(form, env, thread);
                        } else {
                            value = eval(args.car(), env, thread);
                            symbol.setSymbolValue(value);
                        }
                    }
                }
                args = args.cdr();
            }
            // Return primary value only!
            thread._values = null;
            return value;
        }
    };
}
