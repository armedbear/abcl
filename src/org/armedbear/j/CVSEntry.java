/*
 * CVSEntry.java
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

import java.util.Calendar;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.TimeZone;

public final class CVSEntry
{
    private String revision;
    private long checkoutTime;

    private CVSEntry(String revision, long checkoutTime)
    {
        this.revision = revision;
        this.checkoutTime = checkoutTime;
    }

    public String getRevision()
    {
        return revision;
    }

    public long getCheckoutTime()
    {
        return checkoutTime;
    }

    public static CVSEntry parseEntryForFile(File file)
    {
        final String text = getEntryText(file);
        if (text != null) {
            String revision = null;
            long checkoutTime = 0;
            StringTokenizer st = new StringTokenizer(text, "/");
            if (st.hasMoreTokens()) {
                // Ignore first token (filename).
                st.nextToken();
            }
            if (st.hasMoreTokens())
                revision = st.nextToken();
            String timeString = null;
            if (st.hasMoreTokens())
                timeString = st.nextToken();
            if (timeString == null || timeString.length() == 0 ||
                timeString.equals("dummy timestamp") ||
                timeString.equals("Result of merge"))
                return new CVSEntry(revision, 0);
            st = new StringTokenizer(timeString, " :");
            try {
                // Ignore first token (day of week).
                st.nextToken();
                String monthName = st.nextToken();
                String months =
                    "JanFebMarAprMayJunJulAugSepOctNovDec";
                // Month is zero-based.
                int month = months.indexOf(monthName) / 3;
                int dayOfMonth = Integer.parseInt(st.nextToken());
                int hour = Integer.parseInt(st.nextToken());
                int minute = Integer.parseInt(st.nextToken());
                int second = Integer.parseInt(st.nextToken());
                int year = Integer.parseInt(st.nextToken());
                Calendar cal = Calendar.getInstance();
                cal.setTimeZone(TimeZone.getTimeZone("GMT+0000"));
                cal.set(year, month, dayOfMonth, hour, minute);
                cal.set(Calendar.SECOND, second);
                cal.set(Calendar.MILLISECOND, 0);
                checkoutTime = cal.getTime().getTime();
            }
            catch (NoSuchElementException e) {}
            catch (NumberFormatException ex) {
                Log.error("parseEntryForFile NumberFormatException");
                Log.error("text = |" + text + "|");
            }
            if (revision != null && revision.length() > 0)
                return new CVSEntry(revision, checkoutTime);
        }
        return null;
    }

    private static String getEntryText(File file)
    {
        if (file == null)
            return null;
        if (file.isRemote())
            return null;
        File parentDir = file.getParentFile();
        if (parentDir == null)
            return null;
        File cvsDir = File.getInstance(parentDir, "CVS");
        if (cvsDir == null || !cvsDir.isDirectory())
            return null;
        File entriesFile = File.getInstance(cvsDir, "Entries");
        if (entriesFile == null || !entriesFile.isFile())
            return null;
        String lookFor = "/".concat(file.getName()).concat("/");
        SystemBuffer buf = new SystemBuffer(entriesFile);
        buf.load();
        for (Line line = buf.getFirstLine(); line != null; line = line.next()) {
            String entry = line.getText();
            if (entry.startsWith(lookFor))
                return entry;
        }
        return null;
    }
}
