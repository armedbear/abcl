/*
 * ToOrCcMailboxFilter.java
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

package org.armedbear.j.mail;

import java.util.List;
import org.armedbear.j.FastStringReader;
import org.armedbear.j.Utilities;

public final class ToOrCcMailboxFilter extends MailboxFilter
{
    private final String pattern;
    private final boolean ignoreCase;

    public ToOrCcMailboxFilter(FastStringReader reader)
    {
        this.pattern = reader.readToken();
        ignoreCase = Utilities.isLowerCase(pattern);
    }

    public boolean accept(MailboxEntry entry)
    {
        MailAddress[] to = entry.getTo();
        if (to != null) {
            if (ignoreCase) {
                for (int i = to.length-1; i >= 0; i--) {
                    if (to[i].matchesIgnoreCase(pattern))
                        return true;
                }
            } else {
                for (int i = to.length-1; i >= 0; i--) {
                    if (to[i].matches(pattern))
                        return true;
                }
            }
        }
        MailAddress[] cc = entry.getCc();
        if (cc != null) {
            if (ignoreCase) {
                for (int i = cc.length-1; i >= 0; i--) {
                    if (cc[i].matchesIgnoreCase(pattern))
                        return true;
                }
            } else {
                for (int i = cc.length-1; i >= 0; i--) {
                    if (cc[i].matches(pattern))
                        return true;
                }
            }
        }
        return false;
    }
}
