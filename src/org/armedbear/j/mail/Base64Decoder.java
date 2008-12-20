/*
 * Base64Decoder.java
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

import java.io.ByteArrayOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import org.armedbear.j.Debug;
import org.armedbear.j.FastStringReader;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;

public final class Base64Decoder
{
    private final static int BAD = 127;
    private final static int END = -1;

    public static boolean decode(String input, OutputStream outputStream)
        throws IOException
    {
        FastStringReader reader = new FastStringReader(input);
        String extra = null;
        String s;
        while ((s = reader.readLine()) != null) {
            s = s.trim();
            if (extra != null) {
                s = extra.concat(s);
                extra = null;
            }
            int length = s.length();
            if ((length % 4) != 0) {
                length = (length / 4) * 4;
                extra = s.substring(length);
                s = s.substring(0, length);
            }
            for (int i = 0; i < length; i += 4) {
                if (i+4 < length) {
                    // Full 4-character block.
                    if (!decode(s, i, outputStream))
                        return false;
                } else {
                    // Last block (may be short).
                    if (!decodeFinal(s, i, outputStream))
                        return false;
                }
            }
        }
        if (extra != null)
            return decodeFinal(extra, 0, outputStream);
        return true;
    }

    public static byte[] decode(String s)
    {
        try {
            ByteArrayOutputStream out = new ByteArrayOutputStream(80);
            int length = s.length();
            for (int i = 0; i < length; i += 4) {
                if (i+4 < length) {
                    // Full 4-character block.
                    if (!decode(s, i, out))
                        return null;
                } else {
                    // Last block (may be short).
                    if (!decodeFinal(s, i, out))
                        return null;
                }
            }
            return out.toByteArray();
        }
        catch (IOException e) {
            Log.error(e);
            return null;
        }
    }

    // Decodes a full 4-character block.
    private static boolean decode(String input, int offset, OutputStream out)
        throws IOException
    {
        // Check for illegal characters.
        byte byte1 = values[input.charAt(offset)];
        if (byte1 == BAD) {
            error(input, offset);
            return false;
        }
        byte byte2 = values[input.charAt(offset+1)];
        if (byte2 == BAD) {
            error(input, offset+1);
            return false;
        }
        byte byte3 = values[input.charAt(offset+2)];
        if (byte3 == BAD) {
            error(input, offset+2);
            return false;
        }
        byte byte4 = values[input.charAt(offset+3)];
        if (byte4 == BAD) {
            error(input, offset+3);
            return false;
        }
        int n = (byte1 << 18) + (byte2 << 12) + (byte3 << 6) + byte4;
        out.write((byte)(0xff & (n >> 16)));
        out.write((byte)(0xff & (n >> 8)));
        out.write((byte)(0xff & n));
        return true;
    }

    // At the end, the input string may have fewer than four characters left.
    // According to RFC2045: "Because it is used only for padding at the end
    // of the data, the occurrence of any '=' characters may be taken as
    // evidence that the end of the data has been reached (without truncation
    // in transit)."
    private static boolean decodeFinal(String input, int offset,
        OutputStream out) throws IOException
    {
        byte b0 = 0;
        byte b1 = 0;
        byte b2 = 0;
        byte b3 = 0;
        int numPaddingChars = 0;
        Debug.assertTrue(offset < input.length());
        b0 = values[input.charAt(offset)];
        if (b0 == END)
            return true; // OK, end of data.
        if (b0 == BAD) {
            error(input, offset);
            return false;
        }
        if (offset + 1 >= input.length()) {
            Log.error("Base64Decoder.decodeFinal premature end of input");
            error(input, offset + 1);
            return false;
        }
        b1 = values[input.charAt(offset + 1)];
        if (b1 == END || b1 == BAD) {
            // END is not legal here, since then we'd only have 6 bits of
            // output.
            error(input, offset + 1);
            return false;
        }
        if (offset + 2 < input.length())
            b2 = values[input.charAt(offset + 2)];
        else
            b2 = END;
        if (b2 == BAD) {
            error(input, offset + 2);
            return false;
        }
        if (b2 == END) {
            b2 = 0;
            numPaddingChars = 2;
        } else {
            // No END yet.
            if (offset + 3 < input.length())
                b3 = values[input.charAt(offset + 3)];
            else
                b3 = END;
            if (b3 == BAD) {
                error(input, offset + 3);
                return false;
            }
            if (b3 == END) {
                b3 = 0;
                numPaddingChars = 1;
            }
        }
        int n = (b0 << 18) | (b1 << 12) | (b2 << 6) | b3;
        out.write((byte)(0xff & (n >> 16)));
        if (numPaddingChars < 2) {
            out.write((byte)(0xff & (n >> 8)));
            if (numPaddingChars == 0)
                out.write((byte)(0xff & n));
        }
        return true;
    }

    private static void error(String input, int offset)
    {
        Log.error("Base64Decoder error at offset " + offset);
        Log.error("Base64Decoder input = |" + input + "|");
        Log.error("Base64Decoder          " + Utilities.spaces(offset) + '^');
    }

    private static final byte values[] =
    {
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, // 0x00-0x07
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, // 0x09-0xff
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, // 0x10-0x17
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, // 0x18-0x1f
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD, // 0x20-0x27   !"#$%&'
        BAD, BAD, BAD,  62, BAD, BAD, BAD,  63, // 0x28-0x2f  ()*+,-./
         52,  53,  54,  55,  56,  57,  58,  59, // 0x30-0x37  01234567
         60,  61, BAD, BAD, BAD, END, BAD, BAD, // 0x38-0x40  89:;<=>?
        BAD,   0,   1,   2,   3,   4,   5,   6, // 0x41-0x47  @ABCDEFG
          7,   8,   9,  10,  11,  12,  13,  14, // 0x48-0x4f  HIJKLMNO
         15,  16,  17,  18,  19,  20,  21,  22, // 0x50-0x57  PQRSTUVW
         23,  24,  25, BAD, BAD, BAD, BAD, BAD, // 0x58-0x5f  XYZ[\]^_
        BAD,  26,  27,  28,  29,  30,  31,  32, // 0x60-0x67  `abcdefg
         33,  34,  35,  36,  37,  38,  39,  40, // 0x68-0x6f  hijklmno
         41,  42,  43,  44,  45,  46,  47,  48, // 0x70-0x77  pqrstuvw
         49,  50,  51, BAD, BAD, BAD, BAD, BAD, // 0x78-0x7f  xyz{|}~
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD,
        BAD, BAD, BAD, BAD, BAD, BAD, BAD, BAD
    };
}
