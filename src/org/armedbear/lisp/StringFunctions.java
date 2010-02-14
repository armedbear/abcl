/*
 * StringFunctions.java
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

public final class StringFunctions {
    // ### %string=
    // Case sensitive.
    private static final Primitive _STRING_EQUAL = new pf__string_equal();
    private static final class pf__string_equal extends Primitive {
        pf__string_equal() {
            super("%string=", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth)

        {
            char[] array1 = first.STRING().getStringChars();
            char[] array2 = second.STRING().getStringChars();
            int start1, end1, start2, end2;
            start1 = Fixnum.getValue(third);
            if (fourth == NIL) {
                end1 = array1.length;
            } else {
                end1 = Fixnum.getValue(fourth);
            }
            start2 = Fixnum.getValue(fifth);
            if (sixth == NIL) {
                end2 = array2.length;
            } else {
                end2 = Fixnum.getValue(sixth);
            }
            if ((end1 - start1) != (end2 - start2))
                return NIL;
            try {
                for (int i = start1, j = start2; i < end1; i++, j++) {
                    if (array1[i] != array2[j])
                        return NIL;
                }
            } catch (ArrayIndexOutOfBoundsException e) {
                // Shouldn't happen.
                Debug.trace(e);
                return NIL;
            }
            return T;
        }
    };

    // ### %%string=
    // Case sensitive.
    private static final Primitive __STRING_EQUAL = new pf___string_equal();
    private static final class pf___string_equal extends Primitive {
        pf___string_equal() {
            super("%%string=", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            char[] array1 = first.STRING().getStringChars();
            char[] array2 = second.STRING().getStringChars();
            if (array1.length != array2.length)
                return NIL;
            for (int i = array1.length; i-- > 0;) {
                if (array1[i] != array2[i])
                    return NIL;
            }
            return T;
        }
    };

    // ### %string/=
    // Case sensitive.
    private static final Primitive _STRING_NOT_EQUAL = new pf__string_not_equal();
    private static final class pf__string_not_equal extends Primitive {
        pf__string_not_equal() {
            super("%string/=", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    if (j == end2)
                        return NIL; // Strings are identical.
                    return Fixnum.getInstance(i);
                }
                if (j == end2) {
                    // Reached end of string2 before end of string1.
                    return Fixnum.getInstance(i);
                }
                if (array1[i] != array2[j])
                    return Fixnum.getInstance(i);
                ++i;
                ++j;
            }
        }
    };

    // ### %string-equal
    // Case insensitive.
    private static final Primitive _STRING_EQUAL_IGNORE_CASE = new pf__string_equal_ignore_case();
    private static final class pf__string_equal_ignore_case extends Primitive {
        pf__string_equal_ignore_case() {
            super("%string-equal", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third, LispObject fourth,
                                  LispObject fifth, LispObject sixth)

        {
            char[] array1 = first.STRING().getStringChars();
            char[] array2 = second.STRING().getStringChars();
            int start1 = Fixnum.getValue(third);
            int end1 = Fixnum.getValue(fourth);
            int start2 = Fixnum.getValue(fifth);
            int end2 = Fixnum.getValue(sixth);
            if ((end1 - start1) != (end2 - start2))
                return NIL;
            int i, j;
            for (i = start1, j = start2; i < end1; i++, j++) {
                char c1 = array1[i];
                char c2 = array2[j];
                if (c1 == c2)
                    continue;
                if (LispCharacter.toUpperCase(c1) == LispCharacter.toUpperCase(c2))
                    continue;
                if (LispCharacter.toLowerCase(c1) == LispCharacter.toLowerCase(c2))
                    continue;
                return NIL;
            }
            return T;
        }
    };

    // ### %string-not-equal
    // Case sensitive.
    private static final Primitive _STRING_NOT_EQUAL_IGNORE_CASE = new pf__string_not_equal_ignore_case();
    private static final class pf__string_not_equal_ignore_case extends Primitive {
        pf__string_not_equal_ignore_case() {
            super("%string-not-equal", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    if (j == end2)
                        return NIL; // Strings are identical.
                    return Fixnum.getInstance(i);
                }
                if (j == end2) {
                    // Reached end of string2.
                    return Fixnum.getInstance(i);
                }
                char c1 = array1[i];
                char c2 = array2[j];
                if (c1 == c2 ||
                        LispCharacter.toUpperCase(c1) == LispCharacter.toUpperCase(c2) ||
                        LispCharacter.toLowerCase(c1) == LispCharacter.toLowerCase(c2)) {
                    ++i;
                    ++j;
                    continue;
                }
                return Fixnum.getInstance(i);
            }
        }
    };

    // ### %string<
    // Case sensitive.
    private static final Primitive _STRING_LESS_THAN = new pf__string_less_than();
    private static final class pf__string_less_than extends Primitive {
        pf__string_less_than() {
            super("%string<", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    if (j == end2)
                        return NIL; // Strings are identical.
                    return Fixnum.getInstance(i);
                }
                if (j == end2) {
                    // Reached end of string2.
                    return NIL;
                }
                char c1 = array1[i];
                char c2 = array2[j];
                if (c1 == c2) {
                    ++i;
                    ++j;
                    continue;
                }
                if (c1 < c2)
                    return Fixnum.getInstance(i);
                // c1 > c2
                return NIL;
            }
        }
    };

    // ### %string<=
    // Case sensitive.
    private static final Primitive _STRING_GREATER_THAN = new pf__string_greater_than();
    private static final class pf__string_greater_than extends Primitive {
        pf__string_greater_than() {
            super("%string>", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    return NIL;
                }
                if (j == end2) {
                    // Reached end of string2.
                    return Fixnum.getInstance(i);
                }
                char c1 = array1[i];
                char c2 = array2[j];
                if (c1 == c2) {
                    ++i;
                    ++j;
                    continue;
                }
                if (c1 < c2)
                    return NIL;
                // c1 > c2
                return Fixnum.getInstance(i);
            }
        }
    };

    // ### %string<=
    // Case sensitive.
    private static final Primitive _STRING_LE = new pf__string_le();
    private static final class pf__string_le extends Primitive {
        pf__string_le() {
            super("%string<=", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    return Fixnum.getInstance(i);
                }
                if (j == end2) {
                    // Reached end of string2.
                    return NIL;
                }
                char c1 = array1[i];
                char c2 = array2[j];
                if (c1 == c2) {
                    ++i;
                    ++j;
                    continue;
                }
                if (c1 > c2)
                    return NIL;
                // c1 < c2
                return Fixnum.getInstance(i);
            }
        }
    };

    // ### %string<=
    // Case sensitive.
    private static final Primitive _STRING_GE = new pf__string_ge();
    private static final class pf__string_ge extends Primitive {
        pf__string_ge() {
            super("%string>=", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    if (j == end2)
                        return Fixnum.getInstance(i); // Strings are identical.
                    return NIL;
                }
                if (j == end2) {
                    // Reached end of string2.
                    return Fixnum.getInstance(i);
                }
                char c1 = array1[i];
                char c2 = array2[j];
                if (c1 == c2) {
                    ++i;
                    ++j;
                    continue;
                }
                if (c1 < c2)
                    return NIL;
                // c1 > c2
                return Fixnum.getInstance(i);
            }
        }
    };

    // ### %string-lessp
    // Case insensitive.
    private static final Primitive _STRING_LESSP = new pf__string_lessp();
    private static final class pf__string_lessp extends Primitive {
        pf__string_lessp() {
            super("%string-lessp", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    if (j == end2)
                        return NIL; // Strings are identical.
                    return Fixnum.getInstance(i);
                }
                if (j == end2) {
                    // Reached end of string2.
                    return NIL;
                }
                char c1 = LispCharacter.toUpperCase(array1[i]);
                char c2 = LispCharacter.toUpperCase(array2[j]);
                if (c1 == c2) {
                    ++i;
                    ++j;
                    continue;
                }
                if (c1 > c2)
                    return NIL;
                // c1 < c2
                return Fixnum.getInstance(i);
            }
        }
    };

    // ### %string-greaterp
    // Case insensitive.
    private static final Primitive _STRING_GREATERP = new pf__string_greaterp();
    private static final class pf__string_greaterp extends Primitive {
        pf__string_greaterp() {
            super("%string-greaterp", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    return NIL;
                }
                if (j == end2) {
                    // Reached end of string2.
                    return Fixnum.getInstance(i);
                }
                char c1 = LispCharacter.toUpperCase(array1[i]);
                char c2 = LispCharacter.toUpperCase(array2[j]);
                if (c1 == c2) {
                    ++i;
                    ++j;
                    continue;
                }
                if (c1 < c2)
                    return NIL;
                // c1 > c2
                return Fixnum.getInstance(i);
            }
        }
    };

    // ### %string-not-lessp
    // Case insensitive.
    private static final Primitive _STRING_NOT_LESSP = new pf__string_not_lessp();
    private static final class pf__string_not_lessp extends Primitive {
        pf__string_not_lessp() {
            super("%string-not-lessp", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    if (j == end2)
                        return Fixnum.getInstance(i); // Strings are identical.
                    return NIL;
                }
                if (j == end2) {
                    // Reached end of string2.
                    return Fixnum.getInstance(i);
                }
                char c1 = LispCharacter.toUpperCase(array1[i]);
                char c2 = LispCharacter.toUpperCase(array2[j]);
                if (c1 == c2) {
                    ++i;
                    ++j;
                    continue;
                }
                if (c1 > c2)
                    return Fixnum.getInstance(i);
                // c1 < c2
                return NIL;
            }
        }
    };

    // ### %string-not-greaterp
    // Case insensitive.
    private static final Primitive _STRING_NOT_GREATERP = new pf__string_not_greaterp();
    private static final class pf__string_not_greaterp extends Primitive {
        pf__string_not_greaterp() {
            super("%string-not-greaterp", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject[] args) {
            if (args.length != 6)
                return error(new WrongNumberOfArgumentsException(this));
            char[] array1 = args[0].STRING().getStringChars();
            char[] array2 = args[1].STRING().getStringChars();
            int start1 = Fixnum.getValue(args[2]);
            int end1 = Fixnum.getValue(args[3]);
            int start2 = Fixnum.getValue(args[4]);
            int end2 = Fixnum.getValue(args[5]);
            int i = start1;
            int j = start2;
            while (true) {
                if (i == end1) {
                    // Reached end of string1.
                    return Fixnum.getInstance(i);
                }
                if (j == end2) {
                    // Reached end of string2.
                    return NIL;
                }
                char c1 = LispCharacter.toUpperCase(array1[i]);
                char c2 = LispCharacter.toUpperCase(array2[j]);
                if (c1 == c2) {
                    ++i;
                    ++j;
                    continue;
                }
                if (c1 > c2)
                    return NIL;
                // c1 < c2
                return Fixnum.getInstance(i);
            }
        }
    };

    // ### %string-upcase
    private static final Primitive _STRING_UPCASE = new pf__string_upcase();
    private static final class pf__string_upcase extends Primitive {
        pf__string_upcase() {
            super("%string-upcase", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            LispObject s = first.STRING();
            final int length = s.length();
            int start = (int) Fixnum.getValue(second);
            if (start < 0 || start > length)
                return error(new TypeError("Invalid start position " + start + "."));
            int end;
            if (third == NIL)
                end = length;
            else
                end = (int) Fixnum.getValue(third);
            if (end < 0 || end > length)
                return error(new TypeError("Invalid end position " + start + "."));
            if (start > end)
                return error(new TypeError("Start (" + start + ") is greater than end (" + end + ")."));
            StringBuilder sb = new StringBuilder(length);
            char[] array = s.getStringChars();
            int i;
            for (i = 0; i < start; i++)
                sb.append(array[i]);
            for (i = start; i < end; i++)
                sb.append(LispCharacter.toUpperCase(array[i]));
            for (i = end; i < length; i++)
                sb.append(array[i]);
            return new SimpleString(sb);
        }
    };

    // ### %string-downcase
    private static final Primitive _STRING_DOWNCASE = new pf__string_downcase();
    private static final class pf__string_downcase extends Primitive {
        pf__string_downcase() {
            super("%string-downcase", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third) {
            LispObject s = first.STRING();
            final int length = s.length();
            int start = (int) Fixnum.getValue(second);
            if (start < 0 || start > length)
                return error(new TypeError("Invalid start position " + start + "."));
            int end;
            if (third == NIL)
                end = length;
            else
                end = (int) Fixnum.getValue(third);
            if (end < 0 || end > length)
                return error(new TypeError("Invalid end position " + start + "."));
            if (start > end)
                return error(new TypeError("Start (" + start + ") is greater than end (" + end + ")."));
            StringBuilder sb = new StringBuilder(length);
            char[] array = s.getStringChars();
            int i;
            for (i = 0; i < start; i++)
                sb.append(array[i]);
            for (i = start; i < end; i++)
                sb.append(LispCharacter.toLowerCase(array[i]));
            for (i = end; i < length; i++)
                sb.append(array[i]);
            return new SimpleString(sb);
        }
    };

    // ### %string-capitalize
    private static final Primitive _STRING_CAPITALIZE = new pf__string_capitalize();
    private static final class pf__string_capitalize extends Primitive {
        pf__string_capitalize() {
            super("%string-capitalize", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            LispObject s = first.STRING();
            final int length = s.length();
            int start = (int) Fixnum.getValue(second);
            if (start < 0 || start > length)
                return error(new TypeError("Invalid start position " + start + "."));
            int end;
            if (third == NIL)
                end = length;
            else
                end = (int) Fixnum.getValue(third);
            if (end < 0 || end > length)
                return error(new TypeError("Invalid end position " + start + "."));
            if (start > end)
                return error(new TypeError("Start (" + start + ") is greater than end (" + end + ")."));
            StringBuilder sb = new StringBuilder(length);
            char[] array = s.getStringChars();
            boolean lastCharWasAlphanumeric = false;
            int i;
            for (i = 0; i < start; i++)
                sb.append(array[i]);
            for (i = start; i < end; i++) {
                char c = array[i];
                if (Character.isLowerCase(c)) {
                    sb.append(lastCharWasAlphanumeric ? c : LispCharacter.toUpperCase(c));
                    lastCharWasAlphanumeric = true;
                } else if (Character.isUpperCase(c)) {
                    sb.append(lastCharWasAlphanumeric ? LispCharacter.toLowerCase(c) : c);
                    lastCharWasAlphanumeric = true;
                } else {
                    sb.append(c);
                    lastCharWasAlphanumeric = Character.isDigit(c);
                }
            }
            for (i = end; i < length; i++)
                sb.append(array[i]);
            return new SimpleString(sb);
        }
    };

    // ### %nstring-upcase
    private static final Primitive _NSTRING_UPCASE = new pf__nstring_upcase();
    private static final class pf__nstring_upcase extends Primitive {
        pf__nstring_upcase() {
            super("%nstring-upcase", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            final AbstractString string = checkString(first);
            final int length = string.length();
            int start = (int) Fixnum.getValue(second);
            if (start < 0 || start > length)
                return error(new TypeError("Invalid start position " + start + "."));
            int end;
            if (third == NIL)
                end = length;
            else
                end = (int) Fixnum.getValue(third);
            if (end < 0 || end > length)
                return error(new TypeError("Invalid end position " + start + "."));
            if (start > end)
                return error(new TypeError("Start (" + start + ") is greater than end (" + end + ")."));
            for (int i = start; i < end; i++)
                string.setCharAt(i, LispCharacter.toUpperCase(string.charAt(i)));
            return string;
        }
    };

    // ### %nstring-downcase
    private static final Primitive _NSTRING_DOWNCASE = new pf__nstring_downcase();
    private static final class pf__nstring_downcase extends Primitive {
        pf__nstring_downcase() {
            super("%nstring-downcase", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            final AbstractString string = checkString(first);
            final int length = string.length();
            int start = (int) Fixnum.getValue(second);
            if (start < 0 || start > length)
                return error(new TypeError("Invalid start position " + start + "."));
            int end;
            if (third == NIL)
                end = length;
            else
                end = (int) Fixnum.getValue(third);
            if (end < 0 || end > length)
                return error(new TypeError("Invalid end position " + start + "."));
            if (start > end)
                return error(new TypeError("Start (" + start + ") is greater than end (" + end + ")."));
            for (int i = start; i < end; i++)
                string.setCharAt(i, LispCharacter.toLowerCase(string.charAt(i)));
            return string;
        }
    };

    // ### %nstring-capitalize
    private static final Primitive _NSTRING_CAPITALIZE = new pf__nstring_capitalize();
    private static final class pf__nstring_capitalize extends Primitive {
        pf__nstring_capitalize() {
            super("%nstring-capitalize", PACKAGE_SYS, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            AbstractString string = checkString(first);
            final int length = string.length();
            int start = (int) Fixnum.getValue(second);
            if (start < 0 || start > length)
                return error(new TypeError("Invalid start position " + start + "."));
            int end;
            if (third == NIL)
                end = length;
            else
                end = (int) Fixnum.getValue(third);
            if (end < 0 || end > length)
                return error(new TypeError("Invalid end position " + start + "."));
            if (start > end)
                return error(new TypeError("Start (" + start + ") is greater than end (" + end + ")."));
            boolean lastCharWasAlphanumeric = false;
            for (int i = start; i < end; i++) {
                char c = string.charAt(i);
                if (Character.isLowerCase(c)) {
                    if (!lastCharWasAlphanumeric)
                        string.setCharAt(i, LispCharacter.toUpperCase(c));
                    lastCharWasAlphanumeric = true;
                } else if (Character.isUpperCase(c)) {
                    if (lastCharWasAlphanumeric)
                        string.setCharAt(i, LispCharacter.toLowerCase(c));
                    lastCharWasAlphanumeric = true;
                } else
                    lastCharWasAlphanumeric = Character.isDigit(c);
            }
            return string;
        }
    };

    // ### stringp
    public static final Primitive STRINGP = new pf_stringp();
    private static final class pf_stringp extends Primitive {
        pf_stringp() {
            super("stringp", "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.STRINGP();
        }
    };

    // ### simple-string-p
    public static final Primitive SIMPLE_STRING_P = new pf_simple_string_p();
    private static final class pf_simple_string_p extends Primitive {
        pf_simple_string_p() {
            super("simple-string-p", "object");
        }

        @Override
        public LispObject execute(LispObject arg) {
            return arg.SIMPLE_STRING_P();
        }
    };

    // ### %make-string
    // %make-string size initial-element element-type => string
    // Returns a simple string.
    private static final Primitive _MAKE_STRING = new pf__make_string();
    private static final class pf__make_string extends Primitive {
        pf__make_string() {
            super("%make-string", PACKAGE_SYS, false);
        }

        @Override
        public LispObject execute(LispObject size, LispObject initialElement,
                                  LispObject elementType)

        {
            final int n = Fixnum.getValue(size);
            if (n < 0 || n >= ARRAY_DIMENSION_MAX) {
                StringBuilder sb = new StringBuilder();
                sb.append("The size specified for this string (");
                sb.append(n);
                sb.append(')');
                if (n >= ARRAY_DIMENSION_MAX) {
                    sb.append(" is >= ARRAY-DIMENSION-LIMIT (");
                    sb.append(ARRAY_DIMENSION_MAX);
                    sb.append(").");
                } else
                    sb.append(" is negative.");
                return error(new LispError(sb.toString()));
            }
            // Ignore elementType.
            SimpleString string = new SimpleString(n);
            if (initialElement != NIL) {
                // Initial element was specified.
                char c = checkCharacter(initialElement).getValue();
                string.fill(c);
            }
            return string;
        }
    };

    // ### char
    private static final Primitive CHAR = new pf_char();
    private static final class pf_char extends Primitive {
        pf_char() {
            super(Symbol.CHAR, "string index");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.CHAR(Fixnum.getValue(second));
        }
    };

    // ### schar
    private static final Primitive SCHAR = new pf_schar();
    private static final class pf_schar extends Primitive {
        pf_schar() {
            super(Symbol.SCHAR, "string index");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            return first.SCHAR(Fixnum.getValue(second));
        }
    };

    // ### set-char
    private static final Primitive SET_CHAR = new pf_set_char();
    private static final class pf_set_char extends Primitive {
        pf_set_char() {
            super(Symbol.SET_CHAR, "string index character");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            checkString(first).setCharAt(Fixnum.getValue(second),
                                         LispCharacter.getValue(third));
            return third;
        }
    };

    // ### set-schar
    private static final Primitive SET_SCHAR = new pf_set_schar();
    private static final class pf_set_schar extends Primitive {
        pf_set_schar() {
            super(Symbol.SET_SCHAR, "string index character");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            if (first instanceof SimpleString) {
                ((SimpleString)first).setCharAt(Fixnum.getValue(second),
                                                LispCharacter.getValue(third));
                return third;
            }
            return type_error(first, Symbol.SIMPLE_STRING);
        }
    };

    // ### string-position
    private static final Primitive STRING_POSITION = new pf_string_position();
    private static final class pf_string_position extends Primitive {
        pf_string_position() {
            super("string-position", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second,
                                  LispObject third)

        {
            char c = LispCharacter.getValue(first);
            AbstractString string = checkString(second);
            int start = Fixnum.getValue(third);
            for (int i = start, limit = string.length(); i < limit; i++) {
                if (string.charAt(i) == c)
                    return number(i);
            }
            return NIL;
        }
    };

    // ### string-find
    private static final Primitive STRING_FIND = new pf_string_find();
    private static final class pf_string_find extends Primitive {
        pf_string_find() {
            super("string-find", PACKAGE_EXT, true, "char string");
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first instanceof LispCharacter) {
                final char c = ((LispCharacter)first).value;
                AbstractString string = Lisp.checkString(second);
                final int limit = string.length();
                for (int i = 0; i < limit; i++) {
                    if (string.charAt(i) == c)
                        return first;
                }
            }
            return NIL;
        }
    };

    // ### simple-string-search pattern string => position
    // Searches string for a substring that matches pattern.
    private static final Primitive SIMPLE_STRING_SEARCH = new pf_simple_string_search();
    private static final class pf_simple_string_search extends Primitive {
        pf_simple_string_search() {
            super("simple-string-search", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            // FIXME Don't call getStringValue() here! (Just look at the chars.)
            int index = second.getStringValue().indexOf(first.getStringValue());
            return index >= 0 ? Fixnum.getInstance(index) : NIL;
        }
    };

    // ### simple-string-fill string character => string
    private static final Primitive STRING_FILL = new pf_string_fill();
    private static final class pf_string_fill extends Primitive {
        pf_string_fill() {
            super("simple-string-fill", PACKAGE_EXT, true);
        }

        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (first instanceof AbstractString) {
                AbstractString s = (AbstractString) first;
                s.fill(LispCharacter.getValue(second));
                return first;
            }
            return type_error(first, Symbol.SIMPLE_STRING);
        }
    };

}
