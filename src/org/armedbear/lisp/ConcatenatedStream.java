/*
 * ConcatenatedStream.java
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

public final class ConcatenatedStream extends Stream
{
    private LispObject streams;

    private ConcatenatedStream(LispObject streams) throws ConditionThrowable
    {
        this.streams = streams;
        isInputStream = true;
    }

    @Override
    public boolean isCharacterInputStream() throws ConditionThrowable
    {
        if (streams == NIL)
            return true;
        return ((Stream)streams.car()).isCharacterInputStream();
    }

    @Override
    public boolean isBinaryInputStream() throws ConditionThrowable
    {
        if (streams == NIL)
            return true;
        return ((Stream)streams.car()).isBinaryInputStream();
    }

    @Override
    public boolean isCharacterOutputStream() throws ConditionThrowable
    {
        return false;
    }

    @Override
    public boolean isBinaryOutputStream() throws ConditionThrowable
    {
        return false;
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.CONCATENATED_STREAM;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.CONCATENATED_STREAM;
    }

    @Override
    public LispObject typep(LispObject typeSpecifier) throws ConditionThrowable
    {
        if (typeSpecifier == Symbol.CONCATENATED_STREAM)
            return T;
        if (typeSpecifier == BuiltInClass.CONCATENATED_STREAM)
            return T;
        return super.typep(typeSpecifier);
    }

    @Override
    public LispObject getElementType() throws ConditionThrowable
    {
        if (streams == NIL)
            return NIL;
        return ((Stream)streams.car()).getElementType();
    }

    @Override
    public LispObject readCharNoHang(boolean eofError, LispObject eofValue)
        throws ConditionThrowable
    {
        if (streams == NIL) {
            if (eofError)
                return error(new EndOfFile(this));
            else
                return eofValue;
        }
	try 
	  {
	    return _charReady() ? readChar(eofError, eofValue) : NIL;
	  }
	catch (java.io.IOException e)
	  {
	    return error(new StreamError(this, e));
	  }
    }

    @Override
    public LispObject listen() throws ConditionThrowable
    {
        if (unreadChar >= 0)
            return T;
        if (streams == NIL)
            return NIL;
        LispObject obj = readCharNoHang(false, this);
        if (obj == this)
            return NIL;
        unreadChar = ((LispCharacter)obj).getValue();
        return T;
    }

    private int unreadChar = -1;

    // Returns -1 at end of file.
    @Override
    protected int _readChar() throws ConditionThrowable, java.io.IOException
    {
        int n;
        if (unreadChar >= 0) {
            n = unreadChar;
            unreadChar = -1;
            return n;
        }
        if (streams == NIL)
            return -1;
        Stream stream = (Stream) streams.car();
        n = stream._readChar();
        if (n >= 0)
            return n;
        streams = streams.cdr();
        return _readChar();
    }

    @Override
    protected void _unreadChar(int n) throws ConditionThrowable
    {
        if (unreadChar >= 0)
            error(new StreamError(this, "UNREAD-CHAR was invoked twice consecutively without an intervening call to READ-CHAR."));
        unreadChar = n;
    }

    @Override
    protected boolean _charReady() throws ConditionThrowable, java.io.IOException
    {
        if (unreadChar >= 0)
            return true;
        if (streams == NIL)
            return false;
        Stream stream = (Stream) streams.car();
        if (stream._charReady())
            return true;
        LispObject remainingStreams = streams.cdr();
        while (remainingStreams != NIL) {
            stream = (Stream) remainingStreams.car();
            if (stream._charReady())
                return true;
            remainingStreams = remainingStreams.cdr();
        }
        return false;
    }

    @Override
    public void _writeChar(char c) throws ConditionThrowable
    {
        outputStreamError();
    }

    @Override
    public void _writeChars(char[] chars, int start, int end)
        throws ConditionThrowable
    {
        outputStreamError();
    }

    @Override
    public void _writeString(String s) throws ConditionThrowable
    {
        outputStreamError();
    }

    @Override
    public void _writeLine(String s) throws ConditionThrowable
    {
        outputStreamError();
    }

    // Reads an 8-bit byte.
    @Override
    public int _readByte() throws ConditionThrowable
    {
        if (streams == NIL)
            return -1;
        Stream stream = (Stream) streams.car();
        int n = stream._readByte();
        if (n >= 0)
            return n;
        streams = streams.cdr();
        return _readByte();
    }

    // Writes an 8-bit byte.
    @Override
    public void _writeByte(int n) throws ConditionThrowable
    {
        outputStreamError();
    }

    @Override
    public void _finishOutput() throws ConditionThrowable
    {
        outputStreamError();
    }

    @Override
    public void _clearInput() throws ConditionThrowable
    {
        // FIXME
    }

    private void outputStreamError() throws ConditionThrowable
    {
        error(new StreamError(this,
                               String.valueOf(this) + " is not an output stream."));
    }

    // ### make-concatenated-stream &rest streams => concatenated-stream
    private static final Primitive MAKE_CONCATENATED_STREAM =
        new Primitive("make-concatenated-stream", "&rest streams")
    {
        @Override
        public LispObject execute(LispObject[] args) throws ConditionThrowable
        {
            LispObject streams = NIL;
            for (int i = 0; i < args.length; i++) {
                if (args[i] instanceof Stream) {
                    Stream stream = (Stream) args[i];
                    if (stream.isInputStream()) {
                        //                         streams[i] = (Stream) args[i];
                        streams = new Cons(stream, streams);
                        continue;
                    }
                }
                error(new TypeError(String.valueOf(args[i]) +
                                     " is not an input stream."));
            }
            return new ConcatenatedStream(streams.nreverse());
        }
    };

    // ### concatenated-stream-streams concatenated-stream => streams
    private static final Primitive CONCATENATED_STREAM_STREAMS =
        new Primitive("concatenated-stream-streams", "concatenated-stream")
    {
        @Override
        public LispObject execute(LispObject arg) throws ConditionThrowable
        {
            if (arg instanceof ConcatenatedStream) 
                return ((ConcatenatedStream)arg).streams;
            return error(new TypeError(arg, Symbol.CONCATENATED_STREAM));
        }
    };
}
