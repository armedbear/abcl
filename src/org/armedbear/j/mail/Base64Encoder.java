/*
 * Base64Encoder.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

import java.io.InputStream;
import java.io.IOException;
import org.armedbear.j.FastStringBuffer;

public final class Base64Encoder
{
    private static final int MAX_LINE_LENGTH = 72;
    private static final int BUFFER_SIZE = (MAX_LINE_LENGTH * 3) / 4;

    private InputStream inputStream;

    public Base64Encoder(InputStream inputStream)
    {
        this.inputStream = inputStream;
    }

    public String encodeLine() throws IOException
    {
        byte[] buffer = new byte[BUFFER_SIZE];
        int length = inputStream.read(buffer, 0, buffer.length);
        if (length <= 0)
            return null;
        if (length == buffer.length)
            return encode(buffer);
        // Partial line.
        byte[] newBuffer = new byte[length];
        System.arraycopy(buffer, 0, newBuffer, 0, length);
        return encode(newBuffer);
    }

    private static String encode(byte[] input)
    {
        FastStringBuffer encoded = new FastStringBuffer();
        for (int i = 0; i < input.length; i += 3)
            encoded.append(encodeThreeBytes(input, i));
        return encoded.toString();
    }

    private static char[] encodeThreeBytes(byte[] input, int offset)
    {
        int[] in = new int[3];
        char[] out = new char[4];
        int n;
        for (n = 0; n < 3; n++) {
            if (offset >= input.length)
                break;
            // Convert signed bytes to positive integers with the same bit
            // pattern.
            if (input[offset] < 0)
                in[n] = input[offset] + 256;
            else
                in[n] = input[offset];
            ++offset;
        }
        if (n > 0) {
            out[0] = map[in[0] >> 2];
            out[1] = map[((in[0] & 3) << 4) | (in[1] >> 4)];
            out[2] = map[((in[1] & 0x0f) << 2) | (in[2] >> 6)];
            out[3] = map[in[2] & 0x3f];
            // Replace characters in output array with pad characters if input
            // array contained fewer than three bytes.
            if (n < 3) {
                out[3] = '=';
                if (n < 2)
                    out[2] = '=';
            }
        }
        return out;
    }

    public static final char[] getBase64Chars()
    {
        return map;
    }

    private static final char[] map = {
        'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H',
        'I', 'J', 'K', 'L', 'M', 'N', 'O', 'P',
        'Q', 'R', 'S', 'T', 'U', 'V', 'W', 'X',
        'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f',
        'g', 'h', 'i', 'j', 'k', 'l', 'm', 'n',
        'o', 'p', 'q', 'r', 's', 't', 'u', 'v',
        'w', 'x', 'y', 'z', '0', '1', '2', '3',
        '4', '5', '6', '7', '8', '9', '+', '/'
    };
}
