/*
 * JavaSyntaxIterator.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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
public final class JavaSyntaxIterator extends DefaultSyntaxIterator
    implements Constants
{
    public JavaSyntaxIterator(Position pos)
    {
        super(pos);
    }

    // Caller must make sure parseBuffer() has been called so flags will be
    // correct.
    public char[] hideSyntacticWhitespace(Line line)
    {
        if (line.flags() == STATE_COMMENT)
            return hideSyntacticWhitespace(line.getText(), STATE_COMMENT);
        if (line.flags() == STATE_QUOTE)
            return hideSyntacticWhitespace(line.getText(), STATE_QUOTE);
        return hideSyntacticWhitespace(line.getText(), STATE_NEUTRAL);
    }

    public char[] hideSyntacticWhitespace(String s)
    {
        return hideSyntacticWhitespace(s, STATE_NEUTRAL);
    }

    // Replaces comments with space characters and double-quoted strings with
    // 'X' characters.
    private char[] hideSyntacticWhitespace(String s, int initialState)
    {
        final char[] chars = s.toCharArray();
        final int length = chars.length;
        if (length > 0 && chars[0] == '#') {
            // Preprocessor line.
            for (int i = length; i-- > 0;)
                chars[i] = ' ';
            return chars;
        }
        int state = initialState;
        for (int i = 0; i < length; i++) {
            char c = chars[i];
            if (c == '\\' && i < length-1) {
                // Escape character.
                chars[i++] = ' ';
                chars[i] = ' ';
                continue;
            }
            if (state == STATE_QUOTE) {
                chars[i] = 'X';
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
