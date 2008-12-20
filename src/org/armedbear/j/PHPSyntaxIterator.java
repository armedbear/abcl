/*
 * PHPSyntaxIterator.java
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
public final class PHPSyntaxIterator extends DefaultSyntaxIterator
    implements Constants
{
    public PHPSyntaxIterator(Position pos)
    {
        super(pos);
    }

    // Caller must make sure parseBuffer() has been called so flags will be
    // correct.
    public char[] hideSyntacticWhitespace(Line line)
    {
        int initialState = PHPFormatter.getState(line.flags());
        switch (initialState) {
            case STATE_NEUTRAL:
            case STATE_COMMENT:
            case STATE_QUOTE:
            case STATE_SINGLEQUOTE:
                break;
            default:
                // We don't care about any other states.
                initialState = STATE_NEUTRAL;
                break;
        }
        return hideSyntacticWhitespace(line.getText(), initialState);
    }

    public char[] hideSyntacticWhitespace(String s)
    {
        return hideSyntacticWhitespace(s, STATE_NEUTRAL);
    }

    // Returns char array with syntactic whitespace (quotes and comments)
    // replaced with actual space characters.
    private char[] hideSyntacticWhitespace(String s, int initialState)
    {
        final char[] chars = s.toCharArray();
        int state = initialState;
        final int length = chars.length;
        for (int i = 0; i < length; i++) {
            char c = chars[i];
            if (c == '\\' && i < length-1) {
                // Escape character.
                chars[i++] = ' ';
                chars[i] = ' ';
                continue;
            }
            if (state == STATE_QUOTE) {
                chars[i] = ' ';
                if (c == '"')
                    state = STATE_NEUTRAL;
                continue;
            }
            if (state == STATE_SINGLEQUOTE) {
                chars[i] = ' ';
                if (c == '\'')
                    state = STATE_NEUTRAL;
                continue;
            }
            if (state == STATE_COMMENT) {
                if (c == '*' && i < length-1 && chars[i+1] == '/') {
                    // /* */ comment ending
                    chars[i++] = ' ';
                    chars[i] = ' ';
                    state = STATE_NEUTRAL;
                } else
                    chars[i] = ' ';
                continue;
            }
            // Reaching here, STATE_NEUTRAL...
            if (c == '"') {
                chars[i] = ' ';
                state = STATE_QUOTE;
                continue;
            }
            if (c == '\'') {
                chars[i] = ' ';
                state = STATE_SINGLEQUOTE;
                continue;
            }
            if (c == '/') {
                if (i < length-1) {
                    if (chars[i+1] == '*') {
                        // /* */ comment starting
                        chars[i++] = ' ';
                        chars[i] = ' ';
                        state = STATE_COMMENT;
                        continue;
                    }
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
