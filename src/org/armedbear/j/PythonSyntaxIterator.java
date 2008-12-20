/*  
 * PythonSyntaxIterator.java
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

public final class PythonSyntaxIterator extends DefaultSyntaxIterator
{
    private static final int STATE_NEUTRAL = 0;
    private static final int STATE_QUOTE   = 1;

    public PythonSyntaxIterator(Position pos)
    {
        super(pos);
    }

    // Returns char array with syntactic whitespace (quotes and comments)
    // replaced with actual space characters.
    public char[] hideSyntacticWhitespace(String s)
    {
        char[] chars = s.toCharArray();
        char quoteChar = 0;
        int state = STATE_NEUTRAL;
        final int length = chars.length;
        for (int i = 0; i < length; i++) {
            char c = chars[i];
            if (c == '\\' && i < length-1) {
                // Escape!
                chars[++i] = ' ';
            } else if (state == STATE_QUOTE) {
                chars[i] = ' ';
                if (c == quoteChar)
                    state = STATE_NEUTRAL;
            } else if (c == '"' || c == '\'') {
                quoteChar = c;
                state = STATE_QUOTE;
                chars[i] = ' ';
            }
        }
        // Handle comment part if any.
        int index = -1;
        for (int i = 0; i < length; i++) {
            if (chars[i] == '#') {
                if (i > 0) {
                    // Ignore '#' if escaped.
                    char c = chars[i-1];
                    if (c == '\\')
                        continue;
                }
                // Otherwise the rest of the line is a comment.
                index = i;
                break;
            }
        }
        if (index >= 0) {
            for (int i = index; i < length; i++)
                chars[i] = ' ';
        }
        return chars;
    }
}
