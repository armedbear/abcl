/*
 * RelativeDateMailboxFilter.java
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

import java.util.Calendar;

public final class RelativeDateMailboxFilter extends MailboxFilter
{
    private boolean after;
    private int days;
    private int months;
    private int years;

    private RFC822Date begin;
    private RFC822Date end;

    private long initMillis;

    private RelativeDateMailboxFilter(int days, int months, int years, boolean after)
    {
        this.after = after;
        this.days = days;
        this.months = months;
        this.years = years;
    }

    public static MailboxFilter getMailboxFilter(String pattern)
    {
        // Shortest valid pattern is 3 chars (e.g. "<3d").
        if (pattern.length() < 3)
            return null;
        char c = pattern.charAt(0);
        boolean after;
        // First char must be either '<' or '>'.
        if (c == '<')
            after = true;
        else if (c == '>')
            after = false;
        else
            return null;
        String number = pattern.substring(1, pattern.length() - 1);
        int n;
        try {
            n = Integer.parseInt(number);
        }
        catch (NumberFormatException e) {
            return null;
        }
        if (n == 0)
            return null;
        c = pattern.charAt(pattern.length() - 1);
        switch (c) {
            case 'd':
                return new RelativeDateMailboxFilter(n, 0, 0, after);
            case 'w':
                return new RelativeDateMailboxFilter(n * 7, 0, 0, after);
            case 'm':
                return new RelativeDateMailboxFilter(0, n, 0, after);
            case 'y':
                return new RelativeDateMailboxFilter(0, 0, n, after);
            default:
                return null;
        }
    }

    private void init()
    {
        Calendar cal = Calendar.getInstance();
        if (days != 0) {
            cal.add(Calendar.DATE, - days);
            if (after)
                cal.add(Calendar.DATE, 1);
        } else if (months != 0) {
            cal.add(Calendar.MONTH, - months);
            if (after)
                cal.add(Calendar.DATE, 1);
        } else if (years != 0) {
            cal.add(Calendar.YEAR, - years);
            if (after)
                cal.add(Calendar.DATE, 1);
        }
        cal.set(Calendar.HOUR_OF_DAY, 0);
        cal.set(Calendar.MINUTE, 0);
        cal.set(Calendar.SECOND, 0);
        cal.set(Calendar.MILLISECOND, 0);
        if (after) {
            begin = new RFC822Date(cal.getTime());
            end = null;
        } else {
            begin = null;
            end = new RFC822Date(cal.getTime());
        }
        initMillis = System.currentTimeMillis();
    }

    public boolean accept(MailboxEntry entry)
    {
        if (initMillis == 0 || System.currentTimeMillis() - initMillis > 60000)
            init();
        RFC822Date date = entry.getDate();
        if (begin != null)
            if (date.before(begin))
                return false;
        if (end != null)
            if (date.after(end))
                return false;
        return true;
    }
}
