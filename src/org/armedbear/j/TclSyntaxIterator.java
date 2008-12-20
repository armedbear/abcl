/*
 * TclSyntaxIterator.java
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

// Supports movement through the syntactically important text of a buffer, i.e.
// skipping whitespace and comments.
public final class TclSyntaxIterator extends DefaultSyntaxIterator
{
    private static final int STATE_NEUTRAL     = 0;
    private static final int STATE_SINGLEQUOTE = 1;
    private static final int STATE_DOUBLEQUOTE = 2;

    public TclSyntaxIterator(Position pos)
    {
        super(pos);
    }

    public char[] hideSyntacticWhitespace(Line line)
    {
        return hideSyntacticWhitespace(line.getText());
    }

    // Returns char array with syntactic whitespace (quotes and comments)
    // replaced with actual space characters.
    public char[] hideSyntacticWhitespace(String s)
    {
        final char[] chars = s.toCharArray();
        int state = STATE_NEUTRAL;
        final int length = chars.length;
        for (int i = 0; i < length; i++) {
            char c = chars[i];
            if (c == '\\' && i < length-1) {
                // Escape character.
                chars[i++] = ' ';
                chars[i] = ' ';
                continue;
            }
            if (state == STATE_SINGLEQUOTE) {
                chars[i] = ' ';
                if (c == '\'')
                    state = STATE_NEUTRAL;
                continue;
            }
            if (state == STATE_DOUBLEQUOTE) {
                chars[i] = ' ';
                if (c == '"')
                    state = STATE_NEUTRAL;
                continue;
            }
            // Reaching here, STATE_NEUTRAL...
            if (c == '\'') {
                chars[i] = ' ';
                state = STATE_SINGLEQUOTE;
                continue;
            }
            if (c == '"') {
                chars[i] = ' ';
                state = STATE_DOUBLEQUOTE;
                continue;
            }
            if (c == '/') {
                if (i < length-1) {
                    if (chars[i+1] == '/') {
                        // "//" comment starting
                        for (int j = i; j < length; j++)
                            chars[j] = ' ';
                        return chars;
                    }
                }
            }
        }
        return chars;
    }
}
