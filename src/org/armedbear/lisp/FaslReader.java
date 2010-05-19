/*
 * FaslReader.java
 *
 * Copyright (C) 2005 Peter Graves
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

public final class FaslReader
{
    // ### fasl-read-string
    public static final ReaderMacroFunction FASL_READ_STRING =
        new ReaderMacroFunction("fasl-read-string", PACKAGE_SYS, false,
                                "stream character")
    {
        @Override
        public LispObject execute(Stream stream, char terminator)

        {
            return stream.readString(terminator, Stream.faslReadtable);
        }
    };

    // ### fasl-read-list
    public static final ReaderMacroFunction FASL_READ_LIST =
        new ReaderMacroFunction("fasl-read-list", PACKAGE_SYS, false,
                                "stream character")
    {
        @Override
        public LispObject execute(Stream stream, char ignored)

        {
            return stream.readList(false, Stream.faslReadtable);
        }
    };

    // ### fasl-read-quote
    public static final ReaderMacroFunction FASL_READ_QUOTE =
        new ReaderMacroFunction("fasl-read-quote", PACKAGE_SYS, false,
                                "stream character")
    {
        @Override
        public LispObject execute(Stream stream, char ignored)

        {
            return new Cons(Symbol.QUOTE,
                            new Cons(stream.read(true, NIL, true,
                                                 LispThread.currentThread(),
                                                 Stream.faslReadtable)));
        }
    };

    // ### fasl-read-dispatch-char
    public static final ReaderMacroFunction FASL_READ_DISPATCH_CHAR =
        new ReaderMacroFunction("fasl-read-dispatch-char", PACKAGE_SYS, false,
                                "stream character")
    {
        @Override
        public LispObject execute(Stream stream, char c)

        {
            return stream.readDispatchChar(c, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-left-paren
    public static final DispatchMacroFunction FASL_SHARP_LEFT_PAREN =
        new DispatchMacroFunction("fasl-sharp-left-paren", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
          return stream.readSharpLeftParen(c, n, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-star
    public static final DispatchMacroFunction FASL_SHARP_STAR =
        new DispatchMacroFunction("fasl-sharp-star", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char ignored, int n)

        {
          return stream.readSharpStar(ignored, n, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-dot
    public static final DispatchMacroFunction FASL_SHARP_DOT =
        new DispatchMacroFunction("fasl-sharp-dot", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
          return stream.readSharpDot(c, n, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-colon
    public static final DispatchMacroFunction FASL_SHARP_COLON =
        new DispatchMacroFunction("fasl-sharp-colon", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            LispThread thread = LispThread.currentThread();
            return stream.readSymbol(FaslReadtable.getInstance());
        }
    };

    // ### fasl-sharp-a
    public static final DispatchMacroFunction FASL_SHARP_A =
        new DispatchMacroFunction("fasl-sharp-a", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return stream.readArray(n, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-b
    public static final DispatchMacroFunction FASL_SHARP_B =
        new DispatchMacroFunction("fasl-sharp-b", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return stream.readRadix(2, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-c
    public static final DispatchMacroFunction FASL_SHARP_C =
        new DispatchMacroFunction("fasl-sharp-c", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return stream.readComplex(Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-o
    public static final DispatchMacroFunction FASL_SHARP_O =
        new DispatchMacroFunction("fasl-sharp-o", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return stream.readRadix(8, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-p
    public static final DispatchMacroFunction FASL_SHARP_P =
        new DispatchMacroFunction("fasl-sharp-p", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return stream.readPathname(Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-r
    public static final DispatchMacroFunction FASL_SHARP_R =
        new DispatchMacroFunction("fasl-sharp-r", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return stream.readRadix(n, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-s
    public static final DispatchMacroFunction FASL_SHARP_S =
        new DispatchMacroFunction("fasl-sharp-s", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return stream.readStructure(Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-x
    public static final DispatchMacroFunction FASL_SHARP_X =
        new DispatchMacroFunction("fasl-sharp-x", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return stream.readRadix(16, Stream.faslReadtable);
        }
    };

    // ### fasl-sharp-quote
    public static final DispatchMacroFunction FASL_SHARP_QUOTE =
        new DispatchMacroFunction("fasl-sharp-quote", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)

        {
            return new Cons(Symbol.FUNCTION,
                            new Cons(stream.read(true, NIL, true,
                                                 LispThread.currentThread(),
                                                 Stream.faslReadtable)));
        }
    };

    // ### fasl-sharp-backslash
    public static final DispatchMacroFunction FASL_SHARP_BACKSLASH =
        new DispatchMacroFunction("fasl-sharp-backslash", PACKAGE_SYS, false,
                                  "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)
        {
            return stream.readCharacterLiteral(FaslReadtable.getInstance(),
                                               LispThread.currentThread());
        }
    };

    // ### fasl-sharp-question-mark
    public static final DispatchMacroFunction FASL_SHARP_QUESTION_MARK =
        new DispatchMacroFunction("fasl-sharp-question-mark", PACKAGE_SYS,
                                  false, "stream sub-char numarg")
    {
        @Override
        public LispObject execute(Stream stream, char c, int n)
        {
            return Load.getUninternedSymbol(n);
        }
    };

}
