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

import static org.armedbear.lisp.Lisp.*;

public final class ConcatenatedStream extends Stream
{
    LispObject streams;

    ConcatenatedStream(LispObject streams)
    {
        super(Symbol.CONCATENATED_STREAM);
        this.streams = streams;
        isInputStream = true;
    }

    @Override
    public boolean isCharacterInputStream()
    {
        if (streams == NIL)
            return true;
        return ((Stream)streams.car()).isCharacterInputStream();
    }

    @Override
    public boolean isBinaryInputStream()
    {
        if (streams == NIL)
            return true;
        return ((Stream)streams.car()).isBinaryInputStream();
    }

    @Override
    public boolean isCharacterOutputStream()
    {
        return false;
    }

    @Override
    public boolean isBinaryOutputStream()
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
    public LispObject typep(LispObject typeSpecifier)
    {
        if (typeSpecifier == Symbol.CONCATENATED_STREAM)
            return T;
        if (typeSpecifier == BuiltInClass.CONCATENATED_STREAM)
            return T;
        return super.typep(typeSpecifier);
    }

    @Override
    public LispObject getElementType()
    {
        if (streams == NIL)
            return NIL;
        return ((Stream)streams.car()).getElementType();
    }

    @Override
    public LispObject readCharNoHang(boolean eofError, LispObject eofValue)

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
    public LispObject listen()
    {
        if (streams == NIL)
            return NIL;
        Stream stream = (Stream)streams.car();
        if (stream.listen() == NIL) {
            streams = streams.cdr();
            return listen();
        }
        return T;
    }

    // Returns -1 at end of file.
    @Override
    protected int _readChar() throws java.io.IOException
    {
        int n;
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
    protected void _unreadChar(int n) throws java.io.IOException
    {
      if (streams == NIL)
            error(new StreamError(this, "UNREAD-CHAR was invoked without a stream to unread into."));
      Stream stream = (Stream)streams.car();

      stream._unreadChar(n);
    }

    @Override
    protected boolean _charReady() throws java.io.IOException
    {
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
    public void _writeChar(char c)
    {
        outputStreamError();
    }

    @Override
    public void _writeChars(char[] chars, int start, int end)

    {
        outputStreamError();
    }

    @Override
    public void _writeString(String s)
    {
        outputStreamError();
    }

    @Override
    public void _writeLine(String s)
    {
        outputStreamError();
    }

    // Reads an 8-bit byte.
    @Override
    public int _readByte()
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
    public void _writeByte(int n)
    {
        outputStreamError();
    }

    @Override
    public void _finishOutput()
    {
        outputStreamError();
    }

    @Override
    public void _clearInput()
    {
        // FIXME
    }

    private void outputStreamError()
    {
        error(new StreamError(this,
                               String.valueOf(this) + " is not an output stream."));
    }

    // ### make-concatenated-stream &rest streams => concatenated-stream
    private static final Primitive MAKE_CONCATENATED_STREAM =
        new Primitive("make-concatenated-stream", "&rest streams")
    {
        @Override
        public LispObject execute(LispObject[] args)
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
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof ConcatenatedStream) 
                return ((ConcatenatedStream)arg).streams;
            return type_error(arg, Symbol.CONCATENATED_STREAM);
        }
    };
}
