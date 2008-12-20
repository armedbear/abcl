/*
 * QuotedPrintableDecoder.java
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

import org.armedbear.j.ByteBuffer;

public final class QuotedPrintableDecoder
{
    public static byte[] decode(String encoded)
    {
        ByteBuffer bb = new ByteBuffer();
        int limit = encoded.length();
        int i = 0;
        while (i < limit) {
            char c = encoded.charAt(i);
            if (c == '=') {
                if (i+1 < limit && encoded.charAt(i+1) == '\n') {
                    // Soft line break.
                    i += 2;
                    continue;
                }
                if (i+2 < limit) {
                    char c1 = encoded.charAt(i+1);
                    char c2 = encoded.charAt(i+2);
                    if (c1 == '\r' && c2 == '\n') {
                        // Soft line break.
                        i += 3;
                        continue;
                    }
                    int hi = decodeHex(c1);
                    if (hi >= 0) {
                        int lo = decodeHex(c2);
                        if (lo >= 0) {
                            byte b = (byte) ((hi << 4) + lo);
                            bb.append(b);
                            i += 3;
                            continue;
                        }
                    }
                }
            }
            // Not encoded.
            bb.append((byte)c);
            ++i;
        }
        byte[] toBeReturned = new byte[bb.length()];
        System.arraycopy(bb.getBytes(), 0, toBeReturned, 0, bb.length());
        return toBeReturned;
    }

    private static int decodeHex(char c)
    {
        if (c >= '0' && c <= '9')
            return c - '0';
        else if (c >= 'A' && c <= 'F')
            return c - 'A' + 10;
        else if (c >= 'a' && c <= 'f')
            return c - 'a' + 10;
        // Error!
        return -1;
    }
}
