/*
 * DateSentMailboxFilter.java
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
import java.util.StringTokenizer;
import org.armedbear.j.Debug;
import org.armedbear.j.FastStringReader;
import org.armedbear.j.Log;

public final class DateSentMailboxFilter extends MailboxFilter
{
    private RFC822Date begin;
    private RFC822Date end;

    private DateSentMailboxFilter(RFC822Date date)
    {
        begin = end = date;
    }

    private DateSentMailboxFilter(RFC822Date begin, RFC822Date end)
    {
        this.begin = begin;
        this.end = end;
    }

    public static MailboxFilter getMailboxFilter(FastStringReader reader)
    {
        final String pattern = reader.readToken();
        if (pattern.length() > 0) {
            char c = pattern.charAt(0);
            if (c == '<' || c == '>')
                return RelativeDateMailboxFilter.getMailboxFilter(pattern);
            try {
                int index = pattern.indexOf('-');
                if (index < 0) {
                    RFC822Date date = parseDate(pattern);
                    return new DateSentMailboxFilter(date);
                } else {
                    RFC822Date begin = parseDate(pattern.substring(0, index));
                    RFC822Date end = parseDate(pattern.substring(index + 1));
                    return new DateSentMailboxFilter(begin, end);
                }
            }
            catch (InvalidDateException e) {}
        }
        return null;
    }

    public boolean accept(MailboxEntry entry)
    {
        RFC822Date date = entry.getDate();
        if (begin != null)
            if (date.before(begin))
                return false;
        if (end != null)
            if (date.after(end))
                return false;
        return true;
    }

    // Input must be in DD/MM/YY format.
    private static RFC822Date parseDate(String input) throws InvalidDateException
    {
        input = input.trim();
        if (input.length() == 0)
            return null; // Not an error (e.g. "~d -2/2/02").
        StringTokenizer st = new StringTokenizer(input, "/");
        int count = st.countTokens();
        if (count < 1 || count > 3)
            throw new InvalidDateException();
        Calendar cal = Calendar.getInstance();
        int day = cal.get(Calendar.DAY_OF_MONTH);
        int month = cal.get(Calendar.MONTH) + 1;
        int year = cal.get(Calendar.YEAR);
        try {
            day = Integer.parseInt(st.nextToken());
            if (count > 1) {
                month = Integer.parseInt(st.nextToken());
                if (count > 2)
                    year = Integer.parseInt(st.nextToken());
            }
        }
        catch (NumberFormatException e) {
            Log.error(e);
            throw new InvalidDateException();
        }
        if (year < 0)
            throw new InvalidDateException();
        if (year < 100) {
            year += 1900;
            if (year < 1971) // There was no email before 1971.
                year += 100;
        }
        if (month < 1 || month > 12)
            throw new InvalidDateException();
        if (day < 1)
            throw new InvalidDateException();
        switch (month) {
            case 1:
            case 3:
            case 5:
            case 7:
            case 8:
            case 10:
            case 12:
                if (day > 31)
                    throw new InvalidDateException();
                break;
            case 2:
                if (day > 29)
                    throw new InvalidDateException();
                if (day == 29 && (year % 4) != 0)
                    throw new InvalidDateException();
                break;
            case 4:
            case 6:
            case 9:
            case 11:
                if (day > 30)
                    throw new InvalidDateException();
                break;
            default:
                Debug.assertTrue(false);
        }
        cal.set(year, month - 1, day, 0, 0);
        return new RFC822Date(cal.getTime());
    }

    private static class InvalidDateException extends Exception
    {
    }
}
