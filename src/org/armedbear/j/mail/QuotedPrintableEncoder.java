/*
 * QuotedPrintableEncoder.java
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

import java.io.UnsupportedEncodingException;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;

public final class QuotedPrintableEncoder
{
    public static String encode(String input, String characterEncoding,
        String separator)
    {
        if (input.length() == 0)
            return input;
        byte[] bytes;
        try {
            bytes = input.getBytes(characterEncoding);
        }
        catch (UnsupportedEncodingException e) {
            Log.error(e);
            bytes = input.getBytes();
        }
        FastStringBuffer sb = new FastStringBuffer();
        int outputLength = 0;
        for (int i = 0; i < bytes.length; i++) {
            byte b = bytes[i];
            if ((b < ' ' && b != '\t') || b == 127 || b == '=') {
                sb.append(encode(b));
                outputLength += 3;
            } else if (outputLength == 0 && b == 'F') {
                if (i+4 < bytes.length &&
                    bytes[i+1] == 'r' &&
                    bytes[i+2] == 'o' &&
                    bytes[i+3] == 'm' &&
                    bytes[i+4] == ' ') {
                    // Need to encode the 'F'.
                    sb.append(encode(b));
                    outputLength += 3;
                } else {
                    // 'F' but not "From ". Don't encode it.
                    sb.append((char) (b < 0 ? b+256 : b));
                    ++outputLength;
                }
            } else if (outputLength == 0 && b == '.') {
                sb.append(encode(b));
                outputLength += 3;
            } else {
                sb.append((char) (b < 0 ? b+256 : b));
                ++outputLength;
            }
            if (outputLength >= 73) {
                // Soft line break.
                sb.append('=');
                sb.append(separator);
                outputLength = 0;
            }
        }
        // Check for whitespace at end of line.
        if (sb.length() > 0) {
            char c = sb.charAt(sb.length()-1);
            if (c == ' ' || c == '\t') {
                sb.append('=');
                sb.append(separator);
            }
        }
        return sb.toString();
    }

    private static String encode(byte b)
    {
        int n = b;
        if (n < 0)
            n += 256;
        FastStringBuffer sb = new FastStringBuffer('=');
        sb.append(Integer.toString(n, 16));
        return sb.toString().toUpperCase();
    }
}
