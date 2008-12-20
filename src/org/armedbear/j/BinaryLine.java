/*
 * BinaryLine.java
 *
 * Copyright (C) 1999-2002 Peter Graves
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

package org.armedbear.j;

public final class BinaryLine extends AbstractLine implements Line
{
    private final int start;
    private final byte[] bytes;
    private final int count;

    public BinaryLine(int start, byte[] bytes, int count)
    {
         this.start = start;
         this.bytes = bytes;
         this.count = count;
    }

    public final int flags()
    {
        return 0;
    }

    public final void setFlags(int flags) {}

    public final String getText()
    {
        Debug.assertTrue(bytes != null);
        FastStringBuffer sb = new FastStringBuffer(256);
        String s = Long.toHexString(0x100000000L + start);
        s = s.substring(1, s.length());
        sb.append(s + "  ");
        int end = start + count;
        for (int i = start; i < end; i++) {
            s = Integer.toHexString(0x200 + bytes[i]);
            s = s.substring(1, s.length()) + " ";
            sb.append(s);
        }
        // Pad short lines.
        for (int i = count; i < 16; i++)
            sb.append("   ");
        for (int i = start; i < end; i++) {
            char c = (char) bytes[i];
            if (c < ' ' || c >= 0x7f)
                c = '.';
            sb.append(c);
        }
        return sb.toString();
    }

    public final void setText(String s) {}

    public final char charAt(int i)
    {
        return getText().charAt(i);
    }

    public final String substring(int beginIndex)
    {
        return getText().substring(beginIndex);
    }

    public final String substring(int beginIndex, int endIndex)
    {
        return getText().substring(beginIndex, endIndex);
    }

    public final String trim()
    {
        return getText().trim();
    }

    public final int length()
    {
        return getText().length();
    }

    public final byte[] getBytes(String encoding)
    {
        return bytes;
    }

    public final boolean isBlank()
    {
        return false;
    }
}
