/*
 * FastStringReader.java
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

package org.armedbear.j;

import java.io.Reader;

public final class FastStringReader extends Reader
{
    private final String s;
    private final int length;
    private int index;
    private int mark;

    public FastStringReader(String s)
    {
        this.s = s;
        length = s.length();
    }

    public final char readChar()
    {
        return index < length ? s.charAt(index++) : 0;
    }

    public final void unreadChar()
    {
        Debug.assertTrue(index > 0);
        if (index > 0)
            --index;
    }

    public int read()
    {
        return index < length ? s.charAt(index++) : -1;
    }

    public int read(char array[], int offset, int count)
    {
        if (offset < 0 || count < 0 || offset + count > array.length)
            throw new IndexOutOfBoundsException();
        if (count == 0)
            return 0;
        final int actual = Math.min(count, length - index);
        s.getChars(index, index + actual, array, offset);
        index += actual;
        return actual;
    }

    // Returns next word (delimited by whitespace) or quoted substring,
    // without enclosing quotes (if any).
    public String readToken()
    {
        skipWhitespace();
        if (index == length)
            return "";
        char quoteChar = 0;
        int begin = index;
        char c = s.charAt(index++);
        if (c == '"' || c == '\'') {
            quoteChar = c;
            ++begin;
        }
        while (index < length) {
            c = s.charAt(index);
            if (quoteChar != 0 && c == quoteChar) {
                // Reached end of pattern.
                int end = index;
                // Skip closing quote.
                ++index;
                return s.substring(begin, end);
            }
            if (quoteChar == 0 && Character.isWhitespace(c)) {
                // If not quoted, whitespace terminates string.
                return s.substring(begin, index);
            } else
                ++index;
        }
        return s.substring(begin, index);
    }

    public String readLine()
    {
        final int limit = length;
        if (index >= limit)
            return null;
        final int begin = index;
        do {
            switch (s.charAt(index)) {
                case '\n':
                    return s.substring(begin, index++);
                case '\r': {
                    final int end = index++;
                    // Skip following LF if any.
                    if (index < limit && s.charAt(index) == '\n')
                        ++index;
                    return s.substring(begin, end);
                }
                // Fall through...
            }
        } while (++index < limit);
        return s.substring(begin, index);
    }

    public long skip(long count)
    {
        final long actual = Math.min(count, length - index);
        index += actual;
        return actual;
    }

    public boolean ready()
    {
        return true;
    }

    public boolean markSupported()
    {
        return true;
    }

    public void mark(int readAheadLimit)
    {
        if (readAheadLimit < 0)
            throw new IllegalArgumentException("Read-ahead limit < 0");
        mark = index;
    }

    public void reset()
    {
        index = mark;
    }

    public void close() {}

    public final String remainder()
    {
        return s.substring(index);
    }

    public final void skipWhitespace()
    {
        while (index < length && Character.isWhitespace(s.charAt(index)))
            ++index;
    }
}
