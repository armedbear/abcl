/*
 * EchoStream.java
 *
 * Copyright (C) 2004-2005 Peter Graves
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

public final class EchoStream extends Stream
{
    private final Stream in;
    private final Stream out;

    private int unreadChar = -1;

    public EchoStream(Stream in, Stream out)
    {
        super(Symbol.ECHO_STREAM);
        this.in = in;
        this.out = out;
    }

    public EchoStream(Stream in, Stream out, boolean interactive)
    {
        super(Symbol.ECHO_STREAM);
        this.in = in;
        this.out = out;
        setInteractive(interactive);
    }

    @Override
    public LispObject getElementType()
    {
        LispObject itype = in.getElementType();
        LispObject otype = out.getElementType();
        if (itype.equal(otype))
            return itype;
        return Symbol.NULL; // FIXME
    }

    public Stream getInputStream()
    {
        return in;
    }

    public Stream getOutputStream()
    {
        return out;
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.ECHO_STREAM;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.ECHO_STREAM;
    }

    @Override
    public LispObject typep(LispObject type)
    {
        if (type == Symbol.ECHO_STREAM)
            return T;
        if (type == BuiltInClass.ECHO_STREAM)
            return T;
        return super.typep(type);
    }

    @Override
    public boolean isInputStream()
    {
        return true;
    }

    @Override
    public boolean isOutputStream()
    {
        return true;
    }

    @Override
    public boolean isCharacterInputStream()
    {
        return in.isCharacterInputStream();
    }

    @Override
    public boolean isBinaryInputStream()
    {
        return in.isBinaryInputStream();
    }

    @Override
    public boolean isCharacterOutputStream()
    {
        return out.isCharacterOutputStream();
    }

    @Override
    public boolean isBinaryOutputStream()
    {
        return out.isBinaryOutputStream();
    }

    // Returns -1 at end of file.
    @Override
    protected int _readChar() throws java.io.IOException
    {
        int n = in._readChar();
        if (n >= 0) {
            // Not at end of file.
            if (unreadChar < 0)
                out._writeChar((char)n);
            else
                unreadChar = -1;
        }
        return n;
    }

    @Override
    protected void _unreadChar(int n) throws java.io.IOException
    {
        in._unreadChar(n);
        unreadChar = n;
    }

    @Override
    protected boolean _charReady() throws java.io.IOException
    {
        return in._charReady();
    }

    @Override
    public void _writeChar(char c)
    {
        out._writeChar(c);
    }

    @Override
    public void _writeChars(char[] chars, int start, int end)

    {
        out._writeChars(chars, start, end);
    }

    @Override
    public void _writeString(String s)
    {
        out._writeString(s);
    }

    @Override
    public void _writeLine(String s)
    {
        out._writeLine(s);
    }

    // Reads an 8-bit byte.
    @Override
    public int _readByte()
    {
        int n = in._readByte();
        if (n >= 0)
            out._writeByte(n);
        return n;
    }

    // Writes an 8-bit byte.
    @Override
    public void _writeByte(int n)
    {
        out._writeByte(n);
    }

    @Override
    public void _finishOutput()
    {
        out._finishOutput();
    }

    @Override
    public void _clearInput()
    {
        in._clearInput();
    }

    @Override
    public LispObject close(LispObject abort)
    {
        // "The effect of CLOSE on a constructed stream is to close the
        // argument stream only. There is no effect on the constituents of
        // composite streams."
        setOpen(false);
        return T;
    }

    @Override
    public LispObject listen()
    {
        return in.listen();
    }

    @Override
    public LispObject freshLine()
    {
        return out.freshLine();
    }

    @Override
    public String toString()
    {
        return unreadableString("ECHO-STREAM");
    }

    // ### make-echo-stream
    // input-stream output-stream => echo-stream
    private static final Primitive MAKE_ECHO_STREAM =
        new Primitive("make-echo-stream", "input-stream output-stream")
    {
        @Override
        public LispObject execute(LispObject first, LispObject second)

        {
            if (!(first instanceof Stream))
                return type_error(first, Symbol.STREAM);
            if (!(second instanceof Stream))
                return type_error(second, Symbol.STREAM);
            return new EchoStream((Stream) first, (Stream) second);
        }
    };

    // ### echo-stream-input-stream
    // echo-stream => input-stream
    private static final Primitive ECHO_STREAM_INPUT_STREAM =
        new Primitive("echo-stream-input-stream", "echo-stream")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof EchoStream)
                return ((EchoStream)arg).getInputStream();
            return type_error(arg, Symbol.ECHO_STREAM);
        }
    };

    // ### echo-stream-output-stream
    // echo-stream => output-stream
    private static final Primitive ECHO_STREAM_OUTPUT_STREAM =
        new Primitive("echo-stream-output-stream", "echo-stream")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof EchoStream)
                return ((EchoStream)arg).getOutputStream();
            return type_error(arg, Symbol.ECHO_STREAM);
        }
    };
}
