/*
 * ByteArrayOutputStream.java
 *
 * Copyright (C) 2009 Alessio Stalla
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

public final class ByteArrayOutputStream extends Stream
{
    private final java.io.ByteArrayOutputStream byteArrayOutputStream;

    public ByteArrayOutputStream()
    {
        this(UNSIGNED_BYTE_8); //Declared in Stream.java
    }

    ByteArrayOutputStream(LispObject elementType)
    {
        super(Symbol.SYSTEM_STREAM);
        this.elementType = elementType;
        initAsBinaryOutputStream(byteArrayOutputStream = new java.io.ByteArrayOutputStream(2048));
        // based on statistics of ABCL's own .cls files
        // as per 20100111, 2048 is the 70th percentile,
        // meaning that only 30% of all .cls files is bigger

        // However, *every* .cls file is bigger than 32 bytes;
        // we want to prevent buffer resizing
    }

    @Override
    public LispObject typeOf()
    {
        return Symbol.STREAM; //TODO
    }

    @Override
    public LispObject classOf()
    {
        return BuiltInClass.STREAM; //TODO
    }

    @Override
    public LispObject typep(LispObject type)
    {
        return super.typep(type); //TODO
    }

    @Override
    protected long _getFilePosition()
    {
        if (elementType == NIL)
            return 0;
        return byteArrayOutputStream.size();
    }

    public byte[] getByteArray()
    {
        if (elementType == NIL) {
            return new byte[0];
	} else {
	    return byteArrayOutputStream.toByteArray();
	}
    }

    @Override
    public String toString()
    {
        return unreadableString("BYTE-ARRAY-OUTPUT-STREAM");
    }

    // ### %make-byte-array-output-stream
    // %make-byte-array-output-stream &optional element-type => byte-array-output-stream
    private static final Primitive MAKE_BYTE_ARRAY_OUTPUT_STREAM =
        new Primitive("%make-byte-array-output-stream", PACKAGE_SYS, false,
                       "&optional element-type")
    {

        @Override
        public LispObject execute() {
            return new ByteArrayOutputStream();
        }

        @Override
        public LispObject execute(LispObject arg)
        {
            return new ByteArrayOutputStream(arg);
        }
    };

    // ### %get-output-stream-bytes
    // %get-output-stream-bytes byte-array-output-stream => java-byte-array
    private static final Primitive GET_OUTPUT_STREAM_STRING =
        new Primitive("%get-output-stream-bytes", PACKAGE_SYS, false,
                       "byte-array-output-stream")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof ByteArrayOutputStream) {
                return JavaObject.getInstance(((ByteArrayOutputStream)arg).getByteArray());
            }
            return error(new TypeError(this, Symbol.STREAM)); //TODO
        }
    };

    private static final Primitive GET_OUTPUT_STREAM_ARRAY =
        new Primitive("%get-output-stream-array", PACKAGE_SYS, false,
                      "byte-array-output-stream")
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof ByteArrayOutputStream)
                return new BasicVector_UnsignedByte8(((ByteArrayOutputStream)arg).getByteArray());

            return error(new TypeError(this, Symbol.STREAM)); // TODO
        }
    };

}
