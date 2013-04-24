/*
 * SynonymStream.java
 *
 * Copyright (C) 2004 Peter Graves
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

public final class SynonymStream extends Stream
{
    final Symbol symbol;

    SynonymStream(Symbol symbol)
    {
        super(Symbol.SYNONYM_STREAM);
        this.symbol = symbol;
    }

    @Override
    public boolean isInputStream()
    {
        return checkStream(symbol.symbolValue()).isInputStream();
    }

    @Override
    public boolean isOutputStream()
    {
        return checkStream(symbol.symbolValue()).isOutputStream();
    }

    @Override
    public boolean isCharacterInputStream()
    {
        return checkStream(symbol.symbolValue()).isCharacterInputStream();
    }

    @Override
    public boolean isBinaryInputStream()
    {
        return checkStream(symbol.symbolValue()).isBinaryInputStream();
    }

    @Override
    public boolean isCharacterOutputStream()
    {
        return checkStream(symbol.symbolValue()).isCharacterOutputStream();
    }

    @Override
    public boolean isBinaryOutputStream()
    {
        return checkStream(symbol.symbolValue()).isBinaryOutputStream();
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.SYNONYM_STREAM;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.SYNONYM_STREAM;
    }

    @Override
    public LispObject typep(LispObject typeSpecifier)
    {
        if (typeSpecifier == Symbol.SYNONYM_STREAM)
            return T;
        if (typeSpecifier == BuiltInClass.SYNONYM_STREAM)
            return T;
        return super.typep(typeSpecifier);
    }

    @Override
    public LispObject getElementType()
    {
        return checkStream(symbol.symbolValue()).getElementType();
    }

    @Override
    public LispObject listen()
    {
        return checkStream(symbol.symbolValue()).listen();
    }

    @Override
    public LispObject fileLength()
    {
        return checkStream(symbol.symbolValue()).fileLength();
    }

    @Override
    public LispObject fileStringLength(LispObject arg)
    {
        return checkStream(symbol.symbolValue()).fileStringLength(arg);
    }

    @Override
    protected int _readChar() throws java.io.IOException
    {
        return checkStream(symbol.symbolValue())._readChar();
    }

    @Override
    protected void _unreadChar(int n) throws java.io.IOException
    {
        checkStream(symbol.symbolValue())._unreadChar(n);
    }

    @Override
    protected boolean _charReady() throws java.io.IOException
    {
        return checkStream(symbol.symbolValue())._charReady();
    }

    @Override
    public void _writeChar(char c)
    {
        checkStream(symbol.symbolValue())._writeChar(c);
    }

    @Override
    public void _writeChars(char[] chars, int start, int end)

    {
        checkStream(symbol.symbolValue())._writeChars(chars, start, end);
    }

    @Override
    public void _writeString(String s)
    {
        checkStream(symbol.symbolValue())._writeString(s);
    }

    @Override
    public void _writeLine(String s)
    {
        checkStream(symbol.symbolValue())._writeLine(s);
    }

    // Reads an 8-bit byte.
    @Override
    public int _readByte()
    {
        return checkStream(symbol.symbolValue())._readByte();
    }

    // Writes an 8-bit byte.
    @Override
    public void _writeByte(int n)
    {
        checkStream(symbol.symbolValue())._writeByte(n);
    }

    @Override
    public void _finishOutput()
    {
        checkStream(symbol.symbolValue())._finishOutput();
    }

    @Override
    public void _clearInput()
    {
        checkStream(symbol.symbolValue())._clearInput();
    }

    @Override
    protected long _getFilePosition()
    {
        return checkStream(symbol.symbolValue())._getFilePosition();
    }

    @Override
    protected boolean _setFilePosition(LispObject arg)
    {
        return checkStream(symbol.symbolValue())._setFilePosition(arg);
    }

    @Override
    public void _close()
    {
        checkStream(symbol.symbolValue())._close();
    }

    @Override
    public String printObject()
    {
        StringBuffer sb = new StringBuffer("SYNONYM-STREAM ");
        sb.append(symbol.printObject());
        return unreadableString(sb.toString());
    }

    // ### make-synonym-stream symbol => synonym-stream
    private static final Primitive MAKE_SYNONYM_STREAM =
        new Primitive("make-synonym-stream", "symbol")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            return new SynonymStream(checkSymbol(arg));
        }
    };

    // ### synonym-stream-symbol synonym-stream => symbol
    private static final Primitive SYNONYM_STREAM_STREAMS =
        new Primitive("synonym-stream-symbol", "synonym-stream")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof SynonymStream) 
                return ((SynonymStream)arg).symbol;
            return type_error(arg, Symbol.SYNONYM_STREAM);
        }
    };
}
