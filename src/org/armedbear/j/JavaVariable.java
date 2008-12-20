/*
 * JavaVariable.java
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

import java.util.StringTokenizer;

public final class JavaVariable
{
    // what
    public static final int FIELD     = 1;
    public static final int PARAMETER = 2;
    public static final int LOCAL     = 3;

    private final String type;
    private final String name;
    private final int what;

    public JavaVariable(String s, int what)
    {
        StringTokenizer st = new StringTokenizer(s);
        FastStringBuffer sb = new FastStringBuffer();
        while (st.hasMoreTokens()) {
            sb.append(st.nextToken());
            sb.append(' ');
        }
        s = sb.toString();
        int length = s.length();
        for (int i = length-1; i >= 0; i--) {
            if (" \t=;,".indexOf(s.charAt(i)) >= 0)
                --length;
            else
                break;
        }
        s = s.substring(0, length);
        int index = s.lastIndexOf(' ');
        if (index < 0)
            index = s.lastIndexOf('\t');
        if (index >= 0) {
            type = s.substring(0, index);
            name = s.substring(index+1).trim();
        } else {
            type = "";
            name = s;
        }
        this.what = what;
    }

    public final String getName()
    {
        return name;
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer(type);
        sb.append(' ');
        sb.append(name);
        sb.append(" (");
        switch (what) {
            case FIELD:
                sb.append("field");
                break;
            case PARAMETER:
                sb.append("parameter");
                break;
            case LOCAL:
                sb.append("local variable");
                break;
        }
        sb.append(")");
        return sb.toString();
    }
}
