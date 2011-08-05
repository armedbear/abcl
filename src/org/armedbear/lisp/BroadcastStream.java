/*
 * BroadcastStream.java
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

public final class BroadcastStream extends Stream
{
    final Stream[] streams;

    BroadcastStream(Stream[] streams)
    {
        super(Symbol.BROADCAST_STREAM);
        this.streams = streams;
        isOutputStream = true;
        if (streams.length == 0) {
            elementType = T;
            isBinaryStream = true;
            isCharacterStream = true;
        } else {
            elementType = streams[streams.length-1].getElementType();
            if (elementType == Symbol.CHARACTER || elementType == Symbol.BASE_CHAR)
                isCharacterStream = true;
            else
                isBinaryStream = true;
        }
    }

    public Stream[] getStreams()
    {
        return streams;
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.BROADCAST_STREAM;
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.BROADCAST_STREAM;
    }

    @Override
    public LispObject typep(LispObject typeSpecifier)
    {
        if (typeSpecifier == Symbol.BROADCAST_STREAM)
            return T;
        if (typeSpecifier == BuiltInClass.BROADCAST_STREAM)
            return T;
        return super.typep(typeSpecifier);
    }

    @Override
    public LispObject listen()
    {
        notSupported();
        // Not reached.
        return NIL;
    }

    @Override
    public LispObject fileLength()
    {
        if (streams.length > 0)
            return streams[streams.length - 1].fileLength();
        else
            return Fixnum.ZERO;
    }

    @Override
    public LispObject fileStringLength(LispObject arg)
    {
        if (streams.length > 0)
            return streams[streams.length - 1].fileStringLength(arg);
        else
            return Fixnum.ONE;
    }

    // Returns -1 at end of file.
    @Override
    protected int _readChar()
    {
        notSupported();
        // Not reached.
        return -1;
    }

    @Override
    protected void _unreadChar(int n)
    {
        notSupported();
    }

    @Override
    protected boolean _charReady()
    {
        notSupported();
        // Not reached.
        return false;
    }

    @Override
    public void _writeChar(char c)
    {
        for (int i = 0; i < streams.length; i++)
            streams[i]._writeChar(c);
    }

    @Override
    public void _writeChars(char[] chars, int start, int end)

    {
        for (int i = 0; i < streams.length; i++)
            streams[i]._writeChars(chars, start, end);
    }

    @Override
    public void _writeString(String s)
    {
        for (int i = 0; i < streams.length; i++)
            streams[i]._writeString(s);
    }

    @Override
    public void _writeLine(String s)
    {
        for (int i = 0; i < streams.length; i++)
            streams[i]._writeLine(s);
    }

    // Reads an 8-bit byte.
    @Override
    public int _readByte()
    {
        notSupported();
        // Not reached.
        return -1;
    }

    // Writes an 8-bit byte.
    @Override
    public void _writeByte(int n)
    {
        for (int i = 0; i < streams.length; i++)
            streams[i]._writeByte(n);
    }

    @Override
    public void _finishOutput()
    {
        for (int i = 0; i < streams.length; i++)
            streams[i]._finishOutput();
    }

    @Override
    public void _clearInput()
    {
        notSupported();
    }

    @Override
    protected long _getFilePosition()
    {
        if (streams.length == 0)
            return 0;
        else
            return streams[streams.length-1]._getFilePosition();
    }

    @Override
    protected boolean _setFilePosition(LispObject arg)
    {
        return false;
    }

    @Override
    public void _close()
    {
        setOpen(false);
    }

    private void notSupported()
    {
        error(new TypeError("Operation is not supported for streams of type BROADCAST-STREAM."));
    }

    @Override
    public String printObject()
    {
        return unreadableString("BROADCAST-STREAM");
    }

    // ### make-broadcast-stream &rest streams => broadcast-stream
    private static final Primitive MAKE_BROADCAST_STREAM =
        new Primitive("make-broadcast-stream", "&rest streams")
    {
        @Override
        public LispObject execute()
        {
            return new BroadcastStream(new Stream[0]);
        }
        @Override
        public LispObject execute(LispObject[] args)
        {
            Stream[] streams = new Stream[args.length];
            for (int i = 0; i < args.length; i++) {
                if (args[i] instanceof Stream) {
                    if (((Stream)args[i]).isOutputStream()) {
                        streams[i] = (Stream) args[i];
                        continue;
                    } else
                        return error(new TypeError(args[i], list(Symbol.SATISFIES,
                                                                   Symbol.OUTPUT_STREAM_P)));
                } else
                    return error(new TypeError(args[i], Symbol.STREAM));
            }
            // All is well.
            return new BroadcastStream(streams);
        }
    };

    // ### broadcast-stream-streams broadcast-stream => streams
    private static final Primitive BROADCAST_STREAM_STREAMS =
        new Primitive("broadcast-stream-streams", "broadcast-stream")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof BroadcastStream) {
                BroadcastStream stream = (BroadcastStream) arg;
                Stream[] streams = stream.streams;
                LispObject result = NIL;
                for (int i = streams.length; i-- > 0;)
                    result = new Cons(streams[i], result);
                return result;
            }
            return error(new TypeError(arg, Symbol.BROADCAST_STREAM));
        }
    };
}
