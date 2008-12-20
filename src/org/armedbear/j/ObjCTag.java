/*
 * ObjCTag.java
 *
 * Copyright (C) 2003-2004 Peter Graves
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

public final class ObjCTag extends LocalTag
{
    public ObjCTag(String name, Position pos)
    {
        super(name, pos);
        canonicalSignature = parseCanonicalSignatureForMethod();
    }

    public String getMethodName()
    {
        int index = name.indexOf(':');
        return index >= 0 ? name.substring(0, index) : name;
    }

    public String getLongName()
    {
        return canonicalSignature;
    }

    public String toString()
    {
        return name;
    }

    public String getSidebarText()
    {
        return name;
    }

    private String parseCanonicalSignatureForMethod()
    {
        FastStringBuffer sb = new FastStringBuffer();
        Position pos = getPosition().copy();
        pos.setOffset(0);
        while (Character.isWhitespace(pos.getChar()))
            if (!pos.next())
                return null;
        char lastChar = 0;
        char c;
        while ((c = pos.getChar()) != '{') {
            if (c == '/') {
                if (!pos.next())
                    return null;
                skipComment(pos);
                if (pos.atEnd())
                    return null;
                continue;
            }
            if (c == '\n' || c == '\t')
                c = ' ';
            if (c != ' ' || lastChar != ' ') {
                sb.append(c);
                lastChar = c;
            }
            if (!pos.next())
                return null;
        }
        return sb.toString().trim();
    }

    // On entry, pos points at second char of "//" or "/*" (which might not
    // actually be '/' or '*').
    private static void skipComment(Position pos)
    {
        char c = pos.getChar();
        if (!pos.next())
            return;
        if (c == '/') {
            while ((c = pos.getChar()) != '\n')
                if (!pos.next())
                    return;
            pos.next();
        } else if (c == '*') {
            while (!pos.lookingAt("*/"))
                if (!pos.next())
                    return;
            pos.skip(2);
        }
    }
}
