/*
 * LispTag.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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

public final class LispTag extends LocalTag
{
    public LispTag(String name, Position pos)
    {
        super(name, pos);
    }

    public LispTag(String name, Position pos, int type)
    {
        super(name, pos, type);
    }

    public String getLongName()
    {
        String s = signature.trim();

        if (s.startsWith("("))
            s = s.substring(1).trim();

        // First word should be "defun" or "defvar" or ...
        int end = 0;
        final int limit = s.length();
        for (int i = 0; i < limit; i++) {
            char c = s.charAt(i);
            if (c == ' ' || c == '\t') {
                end = i;
                break;
            }
        }
        String d = s.substring(0, end);

        if (d.equals("defgeneric") || d.equals("defmethod"))
            return signature;

        s = s.substring(end).trim();
        end = s.length();
        FastStringBuffer sb = new FastStringBuffer('(');
        sb.append(d);
        sb.append(' ');
        for (int i = 0; i < end; i++) {
            char c = s.charAt(i);
            if (c == ' ' || c == '\t') {
                sb.append(s.substring(0, i));
                sb.append(" ...");
                return sb.toString();
            }
            if (c == ')') {
                sb.append(s.substring(0, i+1));
                return sb.toString();
            }
        }
        sb.append(s.substring(0, end));
        sb.append(" ...");
        return sb.toString();
    }
}
