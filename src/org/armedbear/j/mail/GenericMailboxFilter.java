/*
 * GenericMailboxFilter.java
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
import org.armedbear.j.Utilities;

public final class GenericMailboxFilter extends MailboxFilter
{
    private String pattern;
    private boolean ignoreCase;

    public GenericMailboxFilter(String pattern)
    {
        this.pattern = pattern;
        ignoreCase = Utilities.isLowerCase(pattern);
    }

    public boolean accept(MailboxEntry entry)
    {
        String subject = entry.getSubject();
        if (subject != null) {
            if (ignoreCase) {
                if (subject.toLowerCase().indexOf(pattern) >= 0)
                    return true; // Subject matches.
            } else {
                if (subject.indexOf(pattern) >= 0)
                    return true; // Subject matches.
            }
        }
        MailAddress[] from = entry.getFrom();
        if (from != null) {
            if (ignoreCase) {
                for (int i = from.length-1; i >= 0; i--) {
                    if (from[i].matchesIgnoreCase(pattern))
                        return true;
                }
            } else {
                for (int i = from.length-1; i >= 0; i--) {
                    if (from[i].matches(pattern))
                        return true;
                }
            }
        }
        return false;
    }
}
