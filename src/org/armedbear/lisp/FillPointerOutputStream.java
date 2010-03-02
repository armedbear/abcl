/*
 * FillPointerOutputStream.java
 *
 * Copyright (C) 2003-2005 Peter Graves
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

public final class FillPointerOutputStream extends Stream
{
    ComplexString string;

    FillPointerOutputStream(ComplexString string)
    {
        super(Symbol.SYSTEM_STREAM);
        elementType = Symbol.CHARACTER;
        isOutputStream = true;
        isInputStream = false;
        isCharacterStream = true;
        isBinaryStream = false;
        this.string = string;
        setWriter(new Writer());
    }

    // ### make-fill-pointer-output-stream string => string-stream
    private static final Primitive MAKE_FILL_POINTER_OUTPUT_STREAM =
        new Primitive("make-fill-pointer-output-stream", PACKAGE_SYS, true)
    {
        @Override
        public LispObject execute(LispObject arg)
        {
            if (arg instanceof ComplexString) {
                ComplexString string = (ComplexString) arg;
                if (string.getFillPointer() >= 0)
                    return new FillPointerOutputStream(string);
            }
            return type_error(arg, list(Symbol.AND, Symbol.STRING,
                                              list(Symbol.SATISFIES,
                                                    Symbol.ARRAY_HAS_FILL_POINTER_P)));
        }
    };

    class Writer extends java.io.Writer
    {
        @Override
        public void write(char cbuf[], int off, int len)
        {
            int fp = string.getFillPointer();
            if (fp >= 0) {
                final int limit = Math.min(cbuf.length, off + len);
                string.ensureCapacity(fp + limit);
                for (int i = off; i < limit; i++) {
                    string.setCharAt(fp, cbuf[i]);
                    ++fp;
                }
            }
            string.setFillPointer(fp);
        }

        @Override
        public void flush()
        {
        }

        @Override
        public void close()
        {
        }
    }
}
