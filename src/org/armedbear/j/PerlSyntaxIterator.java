/*
 * PerlSyntaxIterator.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;

// Supports movement through the syntactically important text of a buffer,
// i.e. skipping whitespace and comments.
public class PerlSyntaxIterator extends DefaultSyntaxIterator
{
    private static final int STATE_NEUTRAL = 0;
    private static final int STATE_QUOTE   = 1;
    private static final int STATE_REGEXP  = 2;
    private static final int STATE_SUBST   = 3;

    private static RE matchRE = new UncheckedRE("(=~|!~)[ \t]+m[^a-zA-Z0-9]");

    public PerlSyntaxIterator(Position pos)
    {
        super(pos);
    }

    // Returns char array with syntactic whitespace (quotes and comments)
    // replaced with actual space characters.
    public char[] hideSyntacticWhitespace(String s)
    {
        char[] chars = s.toCharArray();
        char quoteChar = 0;
        char delimiter = 0;
        int state = STATE_NEUTRAL;
        final int length = chars.length;
        for (int i = 0; i < length; i++) {
            char c = chars[i];
            if (c == '\\' && i < length-1) {
                // Escape!
                chars[i++] = ' ';
                chars[i] = ' ';
            } else if (state == STATE_QUOTE) {
                chars[i] = ' ';
                if (c == quoteChar)
                    state = STATE_NEUTRAL;
            } else if (state == STATE_REGEXP) {
                if (c == delimiter)
                    state = STATE_NEUTRAL;
                else
                    chars[i] = ' ';
            } else if (state == STATE_SUBST) {
                if (c == delimiter)
                    state = STATE_REGEXP;
                else
                    chars[i] = ' ';
            } else if (c == '"' || c == '\'' || c == '`') {
                quoteChar = c;
                state = STATE_QUOTE;
                chars[i] = ' ';
            } else if (c == '/') {
                if (PerlFormatter.isSubst(s, i)) {
                    state = STATE_SUBST;
                    delimiter = '/';
                } else if (PerlFormatter.isRegExp(s, i)) {
                    state = STATE_REGEXP;
                    delimiter = '/';
                }
            } else if (c == '=' || c == '!') {
                REMatch match = matchRE.getMatch(s.substring(i));
                if (match != null) {
                    final String m = match.toString();
                    final int len = m.length();
                    delimiter = m.charAt(len - 1);
                    if (delimiter == '{')
                        delimiter = '}';
                    state = STATE_REGEXP;
                    i += len - 1;
                }
            }
        }
        // Handle comment part if any.
        int index = -1;
        for (int i = 0; i < length; i++) {
            if (chars[i] == '#') {
                if (i > 0) {
                    // Ignore '#' if escaped or if preceding char is '$'.
                    char c = chars[i-1];
                    if (c == '\\' || c == '$')
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
