/*
 * CharacterFunctions.java
 *
 * Copyright (C) 2003-2006 Peter Graves
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

public final class CharacterFunctions extends Lisp
{
    // ### char=
    private static final Primitive CHAR_EQUALS =
        new Primitive("char=", "&rest characters")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return error(new WrongNumberOfArgumentsException(this));
        }
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            try {
                return ((LispCharacter)first).value == ((LispCharacter)second).value ? T : NIL;
            }
            catch (ClassCastException e) {
                LispObject datum;
                if (first instanceof LispCharacter)
                    datum = second;
                else
                    datum = first;
                return type_error(datum, Symbol.CHARACTER);
            }
        }
        public LispObject execute(LispObject[] array) throws ConditionThrowable
        {
            final int length = array.length;
            final char c0 = LispCharacter.getValue(array[0]);
            for (int i = 1; i < length; i++) {
                if (c0 != LispCharacter.getValue(array[i]))
                    return NIL;
            }
            return T;
        }
    };

    // ### char-equal
    private static final Primitive CHAR_EQUAL =
        new Primitive("char-equal", "&rest characters")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return error(new WrongNumberOfArgumentsException(this));
        }
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            final char c1, c2;
            try {
                c1 = ((LispCharacter)first).value;
            }
            catch (ClassCastException e) {
                return type_error(first, Symbol.CHARACTER);
            }
            try {
                c2 = ((LispCharacter)second).value;
            }
            catch (ClassCastException e) {
                return type_error(second, Symbol.CHARACTER);
            }
            if (c1 == c2)
                return T;
            if (LispCharacter.toUpperCase(c1) == LispCharacter.toUpperCase(c2))
                return T;
            if (LispCharacter.toLowerCase(c1) == LispCharacter.toLowerCase(c2))
                return T;
            return NIL;
        }
        public LispObject execute(LispObject[] array) throws ConditionThrowable
        {
            final int length = array.length;
            final char c0 = LispCharacter.getValue(array[0]);
            for (int i = 1; i < length; i++) {
                char c = LispCharacter.getValue(array[i]);
                if (c0 == c)
                    continue;
                if (LispCharacter.toUpperCase(c0) == LispCharacter.toUpperCase(c))
                    continue;
                if (LispCharacter.toLowerCase(c0) == LispCharacter.toLowerCase(c))
                    continue;
                return NIL;
            }
            return T;
        }
    };

    // ### char-greaterp
    private static final Primitive CHAR_GREATERP =
        new Primitive("char-greaterp", "&rest characters")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return error(new WrongNumberOfArgumentsException(this));
        }
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            char c1 = LispCharacter.toUpperCase(LispCharacter.getValue(first));
            char c2 = LispCharacter.toUpperCase(LispCharacter.getValue(second));
            return c1 > c2 ? T : NIL;
        }
        public LispObject execute(LispObject[] array) throws ConditionThrowable
        {
            final int length = array.length;
            char[] chars = new char[length];
            for (int i = 0; i < length; i++)
                chars[i] = LispCharacter.toUpperCase(LispCharacter.getValue(array[i]));
            for (int i = 1; i < length; i++) {
                if (chars[i-1] <= chars[i])
                    return NIL;
            }
            return T;
        }
    };

    // ### char-not-greaterp
    private static final Primitive CHAR_NOT_GREATERP =
        new Primitive("char-not-greaterp", "&rest characters")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return error(new WrongNumberOfArgumentsException(this));
        }
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            char c1 = LispCharacter.toUpperCase(LispCharacter.getValue(first));
            char c2 = LispCharacter.toUpperCase(LispCharacter.getValue(second));
            return c1 <= c2 ? T : NIL;
        }
        public LispObject execute(LispObject[] array) throws ConditionThrowable
        {
            final int length = array.length;
            char[] chars = new char[length];
            for (int i = 0; i < length; i++)
                chars[i] = LispCharacter.toUpperCase(LispCharacter.getValue(array[i]));
            for (int i = 1; i < length; i++) {
                if (chars[i] < chars[i-1])
                    return NIL;
            }
            return T;
        }
    };

    // ### char<
    private static final Primitive CHAR_LESS_THAN =
        new Primitive("char<", "&rest characters")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return error(new WrongNumberOfArgumentsException(this));
        }
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            try {
                return ((LispCharacter)first).value < ((LispCharacter)second).value ? T : NIL;
            }
            catch (ClassCastException e) {
                LispObject datum;
                if (first instanceof LispCharacter)
                    datum = second;
                else
                    datum = first;
                return type_error(datum, Symbol.CHARACTER);
            }
        }
        public LispObject execute(LispObject[] args) throws ConditionThrowable
        {
            final int length = args.length;
            char[] chars = new char[length];
            for (int i = 0; i < length; i++) {
                try {
                    chars[i] = ((LispCharacter)args[i]).value;
                }
                catch (ClassCastException e) {
                    return type_error(args[i], Symbol.CHARACTER);
                }
            }
            for (int i = 1; i < length; i++) {
                if (chars[i-1] >= chars[i])
                    return NIL;
            }
            return T;
        }
    };

    // ### char<=
    private static final Primitive CHAR_LE =
        new Primitive("char<=", "&rest characters")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return error(new WrongNumberOfArgumentsException(this));
        }
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            try {
                return ((LispCharacter)first).value <= ((LispCharacter)second).value ? T : NIL;
            }
            catch (ClassCastException e) {
                LispObject datum;
                if (first instanceof LispCharacter)
                    datum = second;
                else
                    datum = first;
                return type_error(datum, Symbol.CHARACTER);
            }
        }
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)
            throws ConditionThrowable
        {
            try {
                if (((LispCharacter)first).value > ((LispCharacter)second).value)
                    return NIL;
                if (((LispCharacter)second).value > ((LispCharacter)third).value)
                    return NIL;
                return T;
            }
            catch (ClassCastException e) {
                LispObject datum;
                if (!(first instanceof LispCharacter))
                    datum = first;
                else if (!(second instanceof LispCharacter))
                    datum = second;
                else
                    datum = third;
                return type_error(datum, Symbol.CHARACTER);
            }
        }
        public LispObject execute(LispObject[] args) throws ConditionThrowable
        {
            final int length = args.length;
            char[] chars = new char[length];
            for (int i = 0; i < length; i++) {
                try {
                    chars[i] = ((LispCharacter)args[i]).value;
                }
                catch (ClassCastException e) {
                    return type_error(args[i], Symbol.CHARACTER);
                }
            }
            for (int i = 1; i < length; i++) {
                if (chars[i-1] > chars[i])
                    return NIL;
            }
            return T;
        }
    };

    // ### char-lessp
    private static final Primitive CHAR_LESSP =
        new Primitive("char-lessp", "&rest characters")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return error(new WrongNumberOfArgumentsException(this));
        }
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            char c1 = LispCharacter.toUpperCase(LispCharacter.getValue(first));
            char c2 = LispCharacter.toUpperCase(LispCharacter.getValue(second));
            return c1 < c2 ? T : NIL;
        }
        public LispObject execute(LispObject[] array) throws ConditionThrowable
        {
            final int length = array.length;
            char[] chars = new char[length];
            for (int i = 0; i < length; i++)
                chars[i] = LispCharacter.toUpperCase(LispCharacter.getValue(array[i]));
            for (int i = 1; i < length; i++) {
                if (chars[i-1] >= chars[i])
                    return NIL;
            }
            return T;
        }
    };

    // ### char-not-lessp
    private static final Primitive CHAR_NOT_LESSP =
        new Primitive("char-not-lessp", "&rest characters")
    {
        public LispObject execute() throws ConditionThrowable
        {
            return error(new WrongNumberOfArgumentsException(this));
        }
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
        {
            char c1 = LispCharacter.toUpperCase(LispCharacter.getValue(first));
            char c2 = LispCharacter.toUpperCase(LispCharacter.getValue(second));
            return c1 >= c2 ? T : NIL;
        }
        public LispObject execute(LispObject[] array) throws ConditionThrowable
        {
            final int length = array.length;
            char[] chars = new char[length];
            for (int i = 0; i < length; i++)
                chars[i] = LispCharacter.toUpperCase(LispCharacter.getValue(array[i]));
            for (int i = 1; i < length; i++) {
                if (chars[i] > chars[i-1])
                    return NIL;
            }
            return T;
        }
    };
}
