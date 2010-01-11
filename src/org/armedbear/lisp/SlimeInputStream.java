/*
 * SlimeInputStream.java
 *
 * Copyright (C) 2004 Andras Simon, Peter Graves
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

public class SlimeInputStream extends Stream
{
    String s;
    int length;
    final Function f;
    final Stream ostream;

    public SlimeInputStream(Function f, Stream ostream)
    {
        super(Symbol.SLIME_INPUT_STREAM);
        elementType = Symbol.CHARACTER;
        isInputStream = true;
        isOutputStream = false;
        isCharacterStream = true;
        isBinaryStream = false;
        this.f = f;
        this.ostream = ostream;
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.SLIME_INPUT_STREAM;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.SLIME_INPUT_STREAM;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.SLIME_INPUT_STREAM)
            return T;
        if (type == Symbol.STRING_STREAM)
            return T;
        if (type == BuiltInClass.SLIME_INPUT_STREAM)
            return T;
        if (type == BuiltInClass.STRING_STREAM)
            return T;
        return super.typep(type);
    }

    @Override
    public LispObject close(LispObject abort)
    {
        setOpen(false);
        return T;
    }

    @Override
    public LispObject listen()
    {
        return offset < length ? T : NIL;
    }

    @Override
    protected int _readChar()
    {
        if (offset >= length) {
            ostream.finishOutput();
            s = LispThread.currentThread().execute(f).getStringValue();
            if (s.length() == 0)
                return -1;
            offset = 0;
            length = s.length();
        }
        int n = s.charAt(offset);
        ++offset;
        if (n == '\n')
            ++lineNumber;
        return n;
    }

    @Override
    protected void _unreadChar(int n)
    {
        if (offset > 0) {
            --offset;
            if (n == '\n')
                --lineNumber;
        }
    }

    @Override
    protected boolean _charReady()
    {
        return offset < length ? true : false;
    }


    @Override
    public void _clearInput()
    {
        super._clearInput();
        s = "";
        offset = 0;
        length = 0;
        lineNumber = 0;
    }


    @Override
    public String toString()
    {
        return unreadableString("SLIME-INPUT-STREAM");
    }

    // ### make-slime-input-stream
    // make-slime-input-stream function output-stream => slime-input-stream
    private static final Primitive MAKE_SLIME_INPUT_STREAM =
        new Primitive("make-slime-input-stream", PACKAGE_EXT, true,
                      "function output-stream")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            final Function fun;
            final Stream os;
            if (first instanceof Symbol)
                fun = (Function)first.getSymbolFunction();
            else
                fun = (Function)first;
            os = checkCharacterOutputStream(second);
            return new SlimeInputStream(fun, os);
        }
    };
}
