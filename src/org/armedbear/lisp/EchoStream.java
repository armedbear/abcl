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

public final class EchoStream extends Stream
{
    private final Stream in;
    private final Stream out;

    private int unreadChar = -1;

    public EchoStream(Stream in, Stream out)
    {
        this.in = in;
        this.out = out;
    }

    public EchoStream(Stream in, Stream out, boolean interactive)
    {
        this.in = in;
        this.out = out;
        setInteractive(interactive);
    }

    public LispObject getElementType() throws ConditionThrowable
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

    public LispObject typeOf()
    {
        return Symbol.ECHO_STREAM;
    }

    public LispObject classOf()
    {
        return BuiltInClass.ECHO_STREAM;
    }

    public LispObject typep(LispObject type) throws ConditionThrowable
    {
        if (type == Symbol.ECHO_STREAM)
            return T;
        if (type == BuiltInClass.ECHO_STREAM)
            return T;
        return super.typep(type);
    }

    public boolean isInputStream()
    {
        return true;
    }

    public boolean isOutputStream()
    {
        return true;
    }

    public boolean isCharacterInputStream() throws ConditionThrowable
    {
        return in.isCharacterInputStream();
    }

    public boolean isBinaryInputStream() throws ConditionThrowable
    {
        return in.isBinaryInputStream();
    }

    public boolean isCharacterOutputStream() throws ConditionThrowable
    {
        return out.isCharacterOutputStream();
    }

    public boolean isBinaryOutputStream() throws ConditionThrowable
    {
        return out.isBinaryOutputStream();
    }

    // Returns -1 at end of file.
    protected int _readChar() throws ConditionThrowable
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

    protected void _unreadChar(int n) throws ConditionThrowable
    {
        in._unreadChar(n);
        unreadChar = n;
    }

    protected boolean _charReady() throws ConditionThrowable
    {
        return in._charReady();
    }

    public void _writeChar(char c) throws ConditionThrowable
    {
        out._writeChar(c);
    }

    public void _writeChars(char[] chars, int start, int end)
        throws ConditionThrowable
    {
        out._writeChars(chars, start, end);
    }

    public void _writeString(String s) throws ConditionThrowable
    {
        out._writeString(s);
    }

    public void _writeLine(String s) throws ConditionThrowable
    {
        out._writeLine(s);
    }

    // Reads an 8-bit byte.
    public int _readByte() throws ConditionThrowable
    {
        int n = in._readByte();
        if (n >= 0)
            out._writeByte(n);
        return n;
    }

    // Writes an 8-bit byte.
    public void _writeByte(int n) throws ConditionThrowable
    {
        out._writeByte(n);
    }

    public void _finishOutput() throws ConditionThrowable
    {
        out._finishOutput();
    }

    public void _clearInput() throws ConditionThrowable
    {
        in._clearInput();
    }

    public LispObject close(LispObject abort) throws ConditionThrowable
    {
        // "The effect of CLOSE on a constructed stream is to close the
        // argument stream only. There is no effect on the constituents of
        // composite streams."
        setOpen(false);
        return T;
    }

    public LispObject listen() throws ConditionThrowable
    {
        return in.listen();
    }

    public LispObject freshLine() throws ConditionThrowable
    {
        return out.freshLine();
    }

    public String toString()
    {
        return unreadableString("ECHO-STREAM");
    }

    // ### make-echo-stream
    // input-stream output-stream => echo-stream
    private static final Primitive MAKE_ECHO_STREAM =
        new Primitive("make-echo-stream", "input-stream output-stream")
    {
        public LispObject execute(LispObject first, LispObject second)
            throws ConditionThrowable
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
        public LispObject execute(LispObject arg) throws ConditionThrowable
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
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof EchoStream)
                return ((EchoStream)arg).getOutputStream();
            return type_error(arg, Symbol.ECHO_STREAM);
        }
    };
}
