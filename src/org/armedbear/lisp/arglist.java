/*
 * arglist.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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

public final class arglist
{
    static final Operator getOperator(LispObject obj)

    {
        if (obj instanceof Operator)
            return (Operator) obj;
        if (obj instanceof Symbol) {
            LispObject function = obj.getSymbolFunction();
            if (function instanceof Autoload) {
                Autoload autoload = (Autoload) function;
                autoload.load();
                function = autoload.getSymbol().getSymbolFunction();
            }
            if (function instanceof Operator) {
                Operator operator = (Operator) function;
                if (operator.getLambdaList() != null)
                    return operator;
                LispObject other = get(obj, Symbol.MACROEXPAND_MACRO, null);
                if (other != null)
                    return getOperator(other);
                else
                    return null;
            }
        } else if (obj instanceof Cons && obj.car() == Symbol.LAMBDA)
            return new Closure(obj, new Environment());
        return null;
    }

    // ### arglist
    private static final Primitive ARGLIST =
        new Primitive("arglist", PACKAGE_EXT, true, "extended-function-designator")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            LispThread thread = LispThread.currentThread();
            Operator operator = getOperator(arg);
            LispObject arglist = null;
            if (operator != null)
                arglist = operator.getLambdaList();
            final LispObject value1, value2;
            if (arglist instanceof AbstractString) {
                String s = arglist.getStringValue();
                // Give the string list syntax.
                s = "(" + s + ")";
                // Bind *PACKAGE* so we use the EXT package if we need
                // to intern any symbols.
                final SpecialBindingsMark mark = thread.markSpecialBindings();
                thread.bindSpecial(Symbol._PACKAGE_, PACKAGE_EXT);
                try {
                    arglist = readObjectFromString(s);
                }
                finally {
                    thread.resetSpecialBindings(mark);
                }
                operator.setLambdaList(arglist);
            }
            if (arglist != null) {
                value1 = arglist;
                value2 = T;
            } else {
                value1 = NIL;
                value2 = NIL;
            }
            return thread.setValues(value1, value2);
        }
    };

    // ### %set-arglist
    private static final Primitive _SET_ARGLIST =
        new Primitive("%set-arglist", PACKAGE_SYS, false)
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            Operator operator = null;
            if (first instanceof Operator) {
                operator = (Operator) first;
            } else if (first instanceof Symbol) {
                LispObject function = first.getSymbolFunction();
                if (function instanceof Operator)
                    operator = (Operator) function;
            }
            if (operator != null)
                operator.setLambdaList(second);
            return second;
        }
    };
}
