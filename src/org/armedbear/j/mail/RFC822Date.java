/*
 * RFC822Date.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Date;
import java.util.Locale;
import java.util.StringTokenizer;
import java.util.TimeZone;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Utilities;

public final class RFC822Date implements Serializable
{
    private Date date;

    private RFC822Date()
    {
    }

    public RFC822Date(Date date)
    {
        this.date = date;
    }

    public Date getDate()
    {
        return date;
    }

    public long getTime()
    {
        if (date != null)
            return date.getTime();
        return 0;
    }

    public static RFC822Date parseDate(String input)
    {
        if (input == null || input.length() == 0)
            return new RFC822Date();
        final StringTokenizer st = new StringTokenizer(input, " ,");
        final ArrayList tokens = new ArrayList();
        while (st.hasMoreTokens())
            tokens.add(st.nextToken());
        final int tokenCount = tokens.size();
        int month = -1;
        int dayOfMonth = -1;
        int year = -1;
        int hour = -1;
        int minute = -1;
        int second = -1;
        for (int i = 0; i < tokenCount; i++) {
            String token = (String) tokens.get(i);
            switch (token.charAt(0)) {
                case 'J':
                    if (token.equals("Jan"))
                        month = 0;
                    else if (token.equals("Jun"))
                        month = 5;
                    else if (token.equals("Jul"))
                        month = 6;
                    break;
                case 'F':
                    if (token.equals("Feb"))
                        month = 1;
                    break;
                case 'M':
                    if (token.equals("Mar"))
                        month = 2;
                    else if (token.equals("May"))
                        month = 4;
                    break;
                case 'A':
                    if (token.equals("Apr"))
                        month = 3;
                    else if (token.equals("Aug"))
                        month = 7;
                    break;
                case 'S':
                    if (token.equals("Sep"))
                        month = 8;
                    break;
                case 'O':
                    if (token.equals("Oct"))
                        month = 9;
                    break;
                case 'N':
                    if (token.equals("Nov"))
                        month = 10;
                    break;
                case 'D':
                    if (token.equals("Dec"))
                        month = 11;
                    break;
            }
            if (month >= 0) {
                tokens.set(i, null);
                if (i > 0) {
                    String before = (String) tokens.get(i-1);
                    try {
                        dayOfMonth = Integer.parseInt(before);
                    }
                    catch (NumberFormatException e) {}
                    if (dayOfMonth >= 1 && dayOfMonth <= 31) {
                        tokens.set(i - 1, null);
                        break;
                    }
                }
                if (i < tokenCount - 1) {
                    String after = (String) tokens.get(i+1);
                    try {
                        dayOfMonth = Integer.parseInt(after);
                    }
                    catch (NumberFormatException e) {}
                    tokens.set(i + 1, null);
                }
                break;
            }
        }
        // Year.
        for (int i = 0; i < tokenCount; i++) {
            String token = (String) tokens.get(i);
            if (token == null)
                continue;
            try {
                year = Integer.parseInt(token);
            }
            catch (NumberFormatException e) {}
            if (year >= 1900 && year < 2100) {
                tokens.set(i, null);
                break;
            }
            year = -1;
        }
        if (year == -1) {
            // Be slightly more permissive.
            for (int i = 0; i < tokenCount; i++) {
                String token = (String) tokens.get(i);
                if (token == null)
                    continue;
                try {
                    year = Integer.parseInt(token);
                }
                catch (NumberFormatException e) {}
                if (year >= 0 && year < 200) {
                    year += 1900;
                    if (year < 1971) // There was no email before 1971.
                        year += 100;
                    tokens.set(i, null);
                    break;
                }
            }
        }
        // Time.
        for (int i = 0; i < tokenCount; i++) {
            String token = (String) tokens.get(i);
            if (token == null)
                continue;
            int index = token.indexOf(':');
            if (index < 0)
                continue;
            try {
                hour = Integer.parseInt(token.substring(0, index));
            }
            catch (NumberFormatException e) {
                continue;
            }
            if (hour > 24)
                continue;
            if (index + 1 >= token.length())
                continue;
            if (!Character.isDigit(token.charAt(index + 1)))
                continue;
            token = token.substring(index + 1);
            try {
                minute = Utilities.parseInt(token);
            }
            catch (NumberFormatException e) {
                continue;
            }
            index = token.indexOf(':');
            if (index >= 0) {
                token = token.substring(index + 1);
                try {
                    second = Utilities.parseInt(token);
                }
                catch (NumberFormatException e) {}
            }
            tokens.set(i, null);
            break;
        }
        // Time zone.
        TimeZone tz = null;
        for (int i = 0; i < tokenCount; i++) {
            String token = (String) tokens.get(i);
            if (token == null)
                continue;
            if (token.length() == 5) {
                boolean maybe = true;
                char c = token.charAt(0);
                if (c == '-' || c == '+') {
                    for (int j = 1; j <= 4; j++) {
                        if (!Character.isDigit(token.charAt(j))) {
                            maybe = false;
                            break;
                        }
                    }
                }
                if (maybe) {
                    tz = TimeZone.getTimeZone("GMT" + token);
                    break;
                }
            }
        }
        if (tz == null) {
            for (int i = 0; i < tokenCount; i++) {
                String token = (String) tokens.get(i);
                if (token == null)
                    continue;
                if (token.length() == 3) {
                    if (token.charAt(2) != 'T')
                        continue;
                    if (token.charAt(0) < 'A' || token.charAt(0) > 'Z')
                        continue;
                    if (token.charAt(1) < 'A' || token.charAt(1) > 'Z')
                        continue;
                    if (token.charAt(1) == 'D') {
                        // Java thinks PDT, EDT, etc. are the same as GMT.
                        // Pacific, Mountain, Central, Eastern.
                        if ("PMCE".indexOf(token.charAt(0)) >= 0) {
                            switch (token.charAt(0)) {
                                case 'P':
                                    token = "PST";
                                    break;
                                case 'M':
                                    token = "MST";
                                    break;
                                case 'C':
                                    token = "CST";
                                    break;
                                case 'E':
                                    token = "EST";
                                    break;
                            }
                        }
                    }
                    tz = TimeZone.getTimeZone(token);
                    if (tz != null)
                        break;
                }
            }
        }
        if (tz == null)
            tz = TimeZone.getTimeZone("GMT+0000");
        if (month < 0 || dayOfMonth < 0 || year < 0 || hour < 0 || minute < 0)
            return new RFC822Date();
        Calendar cal = Calendar.getInstance();
        cal.setTimeZone(tz);
        cal.set(year, month, dayOfMonth, hour, minute);
        if (second >= 0)
            cal.set(Calendar.SECOND, second);
        return new RFC822Date(cal.getTime());
    }

    private static final SimpleDateFormat toStringFormat =
        new SimpleDateFormat("EEE, dd MMM yyyy HH:mm:ss", Locale.US);

    public String toString()
    {
        if (date == null)
            return "null";
        return toStringFormat.format(date);
    }

    public static int compare(RFC822Date date1, RFC822Date date2)
    {
        if (date1.date == null)
            return -1;
        if (date2.date == null)
            return 1;
        if (date1.date.before(date2.date))
            return -1;
        if (date1.date.after(date2.date))
            return 1;
        return 0;
    }

    public final boolean equals(RFC822Date d)
    {
        if (d == null)
            return false;
        else if (date == null)
            return d.date == null;
        else
            return date.equals(d.date);
    }

    // Compares date only (i.e. ignores hours, minutes, seconds).
    public boolean before(RFC822Date d)
    {
        if (date == null) {
            if (d.date == null)
                return false;
            else
                return true;
        }
        if (d.date == null)
            return false;
        int thisYear = date.getYear();
        int otherYear = d.date.getYear();
        if (thisYear < otherYear)
            return true;
        if (thisYear > otherYear)
            return false;
        // Same year.
        int thisMonth = date.getMonth();
        int otherMonth = d.date.getMonth();
        if (thisMonth < otherMonth)
            return true;
        if (thisMonth > otherMonth)
            return false;
        // Same year and month.
        return date.getDate() < d.date.getDate();
    }

    // Compares date only (i.e. ignores hours, minutes, seconds).
    public boolean after(RFC822Date d)
    {
        if (date == null)
            return false;
        if (d.date == null)
            return true;
        int thisYear = date.getYear();
        int otherYear = d.date.getYear();
        if (thisYear > otherYear)
            return true;
        if (thisYear < otherYear)
            return false;
        // Same year.
        int thisMonth = date.getMonth();
        int otherMonth = d.date.getMonth();
        if (thisMonth > otherMonth)
            return true;
        if (thisMonth < otherMonth)
            return false;
        // Same year and month.
        return date.getDate() > d.date.getDate();
    }

    // Used only by getDateTimeString.
    private static final SimpleDateFormat df = new SimpleDateFormat("EEE, d MMM yyyy HH:mm:ss ", Locale.US);

    public static String getDateTimeString()
    {
        return getDateTimeString(Calendar.getInstance());
    }

    public static String getDateTimeString(Calendar calendar)
    {
        FastStringBuffer sb = new FastStringBuffer(48);
        sb.append(df.format(calendar.getTime()));
        int offset = calendar.get(Calendar.ZONE_OFFSET) + calendar.get(Calendar.DST_OFFSET);
        if (offset == 0) {
            sb.append("+0000"); // '+' by convention.
        } else {
            if (offset < 0) {
                sb.append('-');
                offset = - offset;
            }
            int hours = offset / (60 * 60 * 1000);
            if (hours >= 10) {
                sb.append(String.valueOf(hours));
            } else {
                sb.append('0');
                sb.append(String.valueOf(hours));
            }
            int minutes = offset % (60 * 60 * 1000);
            if (minutes >= 10) {
                sb.append(String.valueOf(minutes));
            } else {
                sb.append('0');
                sb.append(String.valueOf(minutes));
            }
        }
        return sb.toString();
    }

    public static final int getOffset(Calendar calendar)
    {
        return calendar.get(Calendar.ZONE_OFFSET) + calendar.get(Calendar.DST_OFFSET);
    }

//     public static void main(String[] args)
//     {
//         String input = "7 Nov 00 14:32:06 IST";
//         System.out.print(input + " ==> " );
//         System.out.println(parseDate(input));
//         input = "Sun, 12 Nov 00 14:58:09 EST";
//         System.out.print(input + " ==> " );
//         System.out.println(parseDate(input));
//         input = "Thu, 28 Dec 2000 09:47:08 -0800";
//         System.out.print(input + " ==> " );
//         System.out.println(parseDate(input));
//     }
}
