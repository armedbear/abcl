/*
 * StringPosition.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

public final class StringPosition implements Constants
{
    private final String text;
    private final int length;

    private int offset;

    public StringPosition(String s)
    {
        text = s;
        length = s.length();
    }

    public StringPosition(String s, int offset)
    {
        this(s);
        this.offset = offset;
    }

    public final String getText()
    {
        return text;
    }

    public final int getOffset()
    {
        return offset;
    }

    public final void setOffset(int n)
    {
        Debug.assertTrue(n <= length);
        this.offset = n;
    }

    public final char getChar()
    {
        Debug.assertTrue(offset <= length);
        if (offset == length)
            return EOL;
        return text.charAt(offset);
    }

    public final boolean lookingAt(String s)
    {
        return s.regionMatches(0, text, offset, s.length());
    }

    public final boolean atEnd()
    {
        return offset >= length;
    }

    public final boolean charIsWhitespace()
    {
        return Character.isWhitespace(text.charAt(offset));
    }

    public final boolean next()
    {
        if (offset < length) {
            ++offset;
            return true;
        }
        return false;
    }

    public final void skip(int count)
    {
        offset += count;
    }
}
