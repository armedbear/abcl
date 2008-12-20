/*
 * CppTag.java
 *
 * Copyright (C) 2002-2006 Peter Graves
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

public final class CppTag extends LocalTag
{
    public CppTag(String name, Position pos, int type)
    {
        super(name, pos, type);
        if (type == TAG_EXPLICIT)
            canonicalSignature = pos.getLine().trim();
    }

    public String getMethodName()
    {
        int index = name.indexOf("::");
        if (index >= 0)
            return name.substring(index+2);
        else
            return name;
    }

    public String getLongName()
    {
        if (name.startsWith("class "))
            return name;
        String s = signature.trim();
        // Strip comment if any.
        int index = s.indexOf("//");
        if (index >= 0)
            s = s.substring(0, index).trim();
        index = s.indexOf(')');
        if (index >= 0)
            s = s.substring(0, index + 1);
        if (s.endsWith("("))
            s = s.substring(0, s.length() - 1);
        return s;
    }
}
