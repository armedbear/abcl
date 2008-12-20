/*
 * DefaultSyntaxIterator.java
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

public class DefaultSyntaxIterator implements SyntaxIterator
{
    private Line line;
    private int offset;
    private char[] cachedChars;
    private Line cachedLine;

    protected DefaultSyntaxIterator(Position pos)
    {
        if (pos != null) {
            line = pos.getLine();
            offset = pos.getOffset();
        }
    }

    public final Position getPosition()
    {
        return new Position(line, offset);
    }

    public final Line getLine()
    {
        return line;
    }

    public final char nextChar()
    {
        int limit = line.length();
        if (offset < limit - 1) {
            if (cachedLine != line) {
                cachedChars = hideSyntacticWhitespace(line);
                cachedLine = line;
            }
            while (++offset < limit) {
                if (cachedChars[offset] > ' ')
                    return cachedChars[offset];
            }
        }
        while (true) {
            if (line.next() == null)
                return DONE;
            line = line.next();
            offset = 0;
            cachedChars = hideSyntacticWhitespace(line);
            cachedLine = line;
            limit = line.length();
            while (offset < limit) {
                if (cachedChars[offset] > ' ')
                    return cachedChars[offset];
                ++offset;
            }
        }
    }

    public final char prevChar()
    {
        if (offset > 0) {
            if (cachedLine != line) {
                cachedChars = hideSyntacticWhitespace(line);
                cachedLine = line;
            }
            while  (--offset >= 0) {
                if (cachedChars[offset] > ' ')
                    return cachedChars[offset];
            }
        }
        while (true) {
            if (line.previous() == null) {
                offset = 0;
                return DONE;
            }
            line = line.previous();
            cachedChars = hideSyntacticWhitespace(line);
            cachedLine = line;
            offset = line.length();
            while (--offset >= 0) {
                if (cachedChars[offset] > ' ')
                    return cachedChars[offset];
            }
        }
    }

    // Default implementation. Subclasses can override this method to examine
    // the line flags.
    public char[] hideSyntacticWhitespace(Line line)
    {
        return hideSyntacticWhitespace(line.getText());
    }

    public char[] hideSyntacticWhitespace(String s)
    {
        return s.toCharArray();
    }
}
