/*
 * SlimeOutputStream.java
 *
 * Copyright (C) 2004-2005 Andras Simon, Peter Graves
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

import java.io.StringWriter;

public final class SlimeOutputStream extends Stream
{
    private final StringWriter stringWriter;
    final Function f;

    SlimeOutputStream(Function f)
    {
        super(Symbol.SLIME_OUTPUT_STREAM);
        this.elementType = Symbol.CHARACTER;
        isInputStream = false;
        isOutputStream = true;
        isCharacterStream = true;
        isBinaryStream = false;
        eolStyle = EolStyle.LF;
        setWriter(stringWriter = new StringWriter());
        this.f = f;
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.SLIME_OUTPUT_STREAM;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.SLIME_OUTPUT_STREAM;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.SLIME_OUTPUT_STREAM)
            return T;
        if (type == Symbol.STRING_STREAM)
            return T;
        if (type == BuiltInClass.SLIME_OUTPUT_STREAM)
            return T;
        if (type == BuiltInClass.STRING_STREAM)
            return T;
        return super.typep(type);
    }

    @Override
    public void _writeChar(char c)
    {
        if (elementType == NIL)
            writeError();
        super._writeChar(c);
    }

    @Override
    public void _writeChars(char[] chars, int start, int end)

    {
        if (elementType == NIL)
            writeError();
        super._writeChars(chars, start, end);
    }

    @Override
    public void _writeString(String s)
    {
        if (elementType == NIL)
            writeError();
        super._writeString(s);
    }

    @Override
    public void _writeLine(String s)
    {
        if (elementType == NIL)
            writeError();
        super._writeLine(s);
    }

    private void writeError()
    {
        error(new TypeError("Attempt to write to a string output stream of element type NIL."));
    }

    @Override
    protected long _getFilePosition()
    {
        if (elementType == NIL)
            return 0;
        return stringWriter.toString().length();
    }

    @Override
    public void _finishOutput()
    {
        super._finishOutput ();
        if (stringWriter.getBuffer().length() > 0) {
            String s = stringWriter.toString();
            stringWriter.getBuffer().setLength(0);
            LispThread.currentThread().execute(f, new SimpleString(s));
        }
    }

    // ### %make-slime-output-stream
    // %make-slime-output-stream function => stream
    private static final Primitive MAKE_SLIME_OUTPUT_STREAM =
        new Primitive("make-slime-output-stream", PACKAGE_EXT, true, "function")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            final Function fun;
            if (arg instanceof Symbol)
                fun = (Function)arg.getSymbolFunction();
            else
                fun = (Function)arg;
            return new SlimeOutputStream(fun);
        }
    };
}
