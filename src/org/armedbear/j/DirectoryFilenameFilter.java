/*
 * DirectoryFilenameFilter.java
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

import java.util.regex.Matcher;
import java.util.regex.Pattern;
import java.util.regex.PatternSyntaxException;

public final class DirectoryFilenameFilter
{
    private Pattern pattern;
    private boolean ignoreCase;

    public DirectoryFilenameFilter(String s) throws PatternSyntaxException
    {
        ignoreCase = Platform.isPlatformWindows();
        if (ignoreCase)
            s = s.toLowerCase();
        FastStringBuffer sb = new FastStringBuffer();
        for (int i = 0; i < s.length(); i++) {
            char c = s.charAt(i);
            switch (c) {
                case '.':
                    sb.append("\\.");
                    break;
                case '*':
                    sb.append(".*");
                    break;
                case '?':
                    sb.append(".?");
                    break;
                default:
                    sb.append(c);
                    break;
            }
        }
        try {
            pattern = Pattern.compile(sb.toString());
        }
        catch (PatternSyntaxException e) {
            Log.debug(e);
            pattern = null;
        }
    }

    public boolean accepts(String name)
    {
        if (pattern == null)
            return false;
        if (ignoreCase)
            name = name.toLowerCase();
        Matcher matcher = pattern.matcher(name);
        return matcher.matches();
    }
}
