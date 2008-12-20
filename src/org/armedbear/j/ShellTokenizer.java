/*
 * ShellTokenizer.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

import java.util.Vector;

public final class ShellTokenizer
{
    private Vector v = new Vector();
    private int index;

    public ShellTokenizer(String s)
    {
        FastStringBuffer sb = new FastStringBuffer();
        char quoteChar = 0;
        int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            if (c == '\\') {
                sb.append(c);
                if (i < limit-1) {
                    sb.append(s.charAt(++i));
                    continue;
                }
            } else if (quoteChar != 0) {
                sb.append(c);
                if (c == quoteChar)
                    quoteChar = 0;
            } else if (c == '\'' || c == '"') {
                sb.append(c);
                quoteChar = c;
            } else if (c == ' ' || c == '\t' || c == '=') {
                if (sb.length() > 0) {
                    v.add(sb.toString());
                    sb.setLength(0);
                }
            } else
                sb.append(c);
        }
        if (sb.length() > 0)
            v.add(sb.toString());
    }

    public boolean hasMoreTokens()
    {
        return index < v.size();
    }

    public String nextToken()
    {
        String token = null;
        if (index < v.size())
            token = (String) v.get(index++);
        return token;
    }

    public String lastToken()
    {
        String token = null;
        if (v.size() > 0)
            token = (String) v.get(v.size()-1);
        return token;
    }
}
