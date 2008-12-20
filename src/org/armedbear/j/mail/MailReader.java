/*
 * MailReader.java
 *
 * Copyright (C) 2002 Peter Graves
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
 */

package org.armedbear.j.mail;

import java.io.IOException;
import java.io.InputStream;

public final class MailReader
{
    private final InputStream inputStream;

    private byte[] buf = new byte[16384];
    private int count;
    private int pos;
    private char[] chars = new char[1024];
    private long offset;

    public MailReader(InputStream inputStream)
    {
        this.inputStream = inputStream;
    }

    public final long getOffset()
    {
        return offset;
    }

    public String readLine() throws IOException
    {
        int i = 0;
        while (true) {
            if (pos >= count) {
                fill();
                if (pos >= count) {
                    // End of stream.
                    if (i > 0)
                        return new String(chars, 0, i);
                    else
                        return null;
                }
            }
            byte b = buf[pos++];
            if (b == 10) {
                // End of line.
                ++offset;
                return new String(chars, 0, i);
            } else if (b == 13) {
                // Ignore.
                ++offset;
            } else {
                if (i == chars.length) {
                    // Need to grow char array.
                    char[] newChars = new char[chars.length * 2];
                    System.arraycopy(chars, 0, newChars, 0, chars.length);
                    chars = newChars;
                }
                ++offset;
                chars[i++] = (char) (b & 0xff);
            }
        }
    }

    public void close() throws IOException
    {
        inputStream.close();
    }

    private final void fill() throws IOException
    {
        pos = 0;
        count = inputStream.read(buf);
    }
}
