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

import static org.armedbear.lisp.Lisp.*;

public final class CharacterFunctions
{
    // ### char=
    private static final Primitive CHAR_EQUALS =
        new Primitive("char=", "&rest characters")
    {
        @Override
        public LispObject execute()
        {
            return error(new WrongNumberOfArgumentsException(this, 1, -1));
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return LispCharacter.getValue(first) == LispCharacter.getValue(second) ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject[] array)
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
        @Override
        public LispObject execute()
        {
            return error(new WrongNumberOfArgumentsException(this, 1, -1));
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final char c1, c2;
            c1 = LispCharacter.getValue(first);
            c2 = LispCharacter.getValue(second);
            if (c1 == c2)
                return T;
            if (LispCharacter.toUpperCase(c1) == LispCharacter.toUpperCase(c2))
                return T;
            if (LispCharacter.toLowerCase(c1) == LispCharacter.toLowerCase(c2))
                return T;
            return NIL;
        }
        @Override
        public LispObject execute(LispObject[] array)
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
        @Override
        public LispObject execute()
        {
            return error(new WrongNumberOfArgumentsException(this, 1, -1));
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            char c1 = LispCharacter.toUpperCase(LispCharacter.getValue(first));
            char c2 = LispCharacter.toUpperCase(LispCharacter.getValue(second));
            return c1 > c2 ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject[] array)
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
        @Override
        public LispObject execute()
        {
            return error(new WrongNumberOfArgumentsException(this, 1, -1));
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            char c1 = LispCharacter.toUpperCase(LispCharacter.getValue(first));
            char c2 = LispCharacter.toUpperCase(LispCharacter.getValue(second));
            return c1 <= c2 ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject[] array)
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
        @Override
        public LispObject execute()
        {
            return error(new WrongNumberOfArgumentsException(this, 1, -1));
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return LispCharacter.getValue(first) < LispCharacter.getValue(second) ? T : NIL;
       }
        @Override
        public LispObject execute(LispObject[] args)
        {
            final int length = args.length;
            char[] chars = new char[length];
            for (int i = 0; i < length; i++) {
                chars[i] = LispCharacter.getValue(args[i]);
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
        @Override
        public LispObject execute()
        {
            return error(new WrongNumberOfArgumentsException(this, 1, -1));
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return LispCharacter.getValue(first) <= LispCharacter.getValue(second) ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (LispCharacter.getValue(first) > LispCharacter.getValue(second))
                return NIL;
            if (LispCharacter.getValue(second) > LispCharacter.getValue(third))
                return NIL;
            return T;
        }
        @Override
        public LispObject execute(LispObject[] args)
        {
            final int length = args.length;
            char[] chars = new char[length];
            for (int i = 0; i < length; i++) {
                chars[i] = LispCharacter.getValue(args[i]);
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
        @Override
        public LispObject execute()
        {
            return error(new WrongNumberOfArgumentsException(this, 1, -1));
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            char c1 = LispCharacter.toUpperCase(LispCharacter.getValue(first));
            char c2 = LispCharacter.toUpperCase(LispCharacter.getValue(second));
            return c1 < c2 ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject[] array)
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
        @Override
        public LispObject execute()
        {
            return error(new WrongNumberOfArgumentsException(this, 1, -1));
        }
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof LispCharacter)
                return T;
            return type_error(arg, Symbol.CHARACTER);
        }
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            char c1 = LispCharacter.toUpperCase(LispCharacter.getValue(first));
            char c2 = LispCharacter.toUpperCase(LispCharacter.getValue(second));
            return c1 >= c2 ? T : NIL;
        }
        @Override
        public LispObject execute(LispObject[] array)
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
