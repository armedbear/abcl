/*
 * Expression.java
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

public class Expression
{
    protected final String name;
    protected final int arity;

    public Expression(String name)
    {
        this(name, -1);
    }

    public Expression(String name, int arity)
    {
        this.name = name;
        this.arity = arity;
    }

    public final String getName()
    {
        return name;
    }

    public final int getArity()
    {
        return arity;
    }

    public boolean matches(LocalTag tag)
    {
        if (!name.equals(tag.getMethodName()))
            return false;
        if (arity >= 0) {
            int n = getArity(tag.getCanonicalSignature());
            if (n < 0 || n == arity)
                return true;
            else
                return false;
        }
        return true;
    }

    // BUG! This function is not aware of comments!
    public static int getArity(String s)
    {
        if (s == null)
            return -1;
        int start = -1;
        int parenCount = 0;
        int arity = 0;
        char quoteChar = '\0';
        boolean inQuote = false;
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            if (start < 0) {
                if (c == '(')
                    start = i+1;
                continue;
            }
            // Reaching here, we've seen the opening paren of the argument list.
            if (inQuote) {
                if (c == quoteChar)
                    inQuote = false;
                continue;
            }
            // Not in a quoted string.
            if (c == '"' || c == '\'') {
                inQuote = true;
                quoteChar = c;
                continue;
            }
            if (c == ',') {
                if (parenCount == 0) // Top level.
                    ++arity;
                continue;
            }
            if (c == '(') {
                ++parenCount;
                continue;
            }
            if (c == ')') {
                --parenCount;
                if (parenCount < 0) {
                    // Closing paren, done.
                    if (arity == 0) {
                        // We haven't seen a comma.
                        String enclosed = s.substring(start, i);
                        boolean isBlank = true;
                        for (int j = 0; j < enclosed.length(); j++) {
                            if (!Character.isWhitespace(enclosed.charAt(j))) {
                                isBlank = false;
                                break;
                            }
                        }
                        if (!isBlank)
                            arity = 1;
                    } else
                        ++arity;
                    return arity;
                }
                continue;
            }
        }
        return -1;
    }
}
