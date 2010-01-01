/*
 * stream_external_format.java
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

// ### stream-external-format
public final class stream_external_format extends Primitive
{
    private stream_external_format()
    {
        super("stream-external-format", "stream");
    }

    @Override
    public LispObject execute(LispObject arg)
    {
        if (arg instanceof Stream)
            return ((Stream)arg).getExternalFormat();
        else
            return error(new TypeError(arg, Symbol.STREAM));
    }

    private static final Primitive STREAM_EXTERNAL_FORMAT =
        new stream_external_format();

    // DEFSETF-ed in 'setf.lisp'
    private static final Primitive SET_STREAM_EXTERNAL_FORMAT =
        new Primitive("%set-stream-external-format",
                      PACKAGE_SYS, false, "stream external-format") {
        @Override
        public LispObject execute(LispObject stream, LispObject format) {
            Stream s = checkStream(stream);
            s.setExternalFormat(format);

            return format;
        }
    };
}
