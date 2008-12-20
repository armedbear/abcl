/*
 * MailUtilities.java
 *
 * Copyright (C) 2000-2003 Peter Graves
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

package org.armedbear.j.mail;

import java.util.List;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Utilities;

public class MailUtilities
{
    public static String constructAddressHeader(String prefix, List list)
    {
        return constructAddressHeader(prefix, list, 8);
    }

    public static String constructAddressHeader(String prefix, List list,
        int indent)
    {
        FastStringBuffer sb = new FastStringBuffer(prefix);
        int length = prefix.length();
        if (list != null) {
            for (int i = 0; i < list.size(); i++) {
                MailAddress a = (MailAddress) list.get(i);
                String s = a.toEncodedString();
                if (i > 0 && length + s.length() > 74) {
                    // Won't fit on current line.
                    sb.append(',');
                    sb.append('\n');
                    sb.append(Utilities.spaces(indent)); // Continuation.
                    sb.append(s);
                    length = indent + s.length();
                } else {
                    if (i > 0) {
                        sb.append(", ");
                        length += 2;
                    }
                    sb.append(s);
                    length += s.length();
                }
            }
        }
        return sb.toString();
    }
}
