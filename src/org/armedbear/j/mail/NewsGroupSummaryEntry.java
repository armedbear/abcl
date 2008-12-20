/*
 * NewsGroupSummaryEntry.java
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

import java.util.ArrayList;
import java.util.List;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;

public final class NewsGroupSummaryEntry extends MailboxEntry
{
    private String from;
    private int lineCount;

    private NewsGroupSummaryEntry()
    {
    }

    public static NewsGroupSummaryEntry parseOverviewEntry(String s)
    {
        NewsGroupSummaryEntry entry = new NewsGroupSummaryEntry();
        while (true) {
            int begin = 0;
            int end = s.indexOf('\t', begin);
            if (end < 0)
                return null;
            String token = s.substring(begin, end);
            try {
                entry.messageNumber = Integer.parseInt(token);
            }
            catch (NumberFormatException e) {
                Log.error(e);
                return null;
            }
            begin = end + 1;
            end = s.indexOf('\t', begin);
            if (end < 0)
                return null;
            entry.subject = s.substring(begin, end);
            begin = end + 1;
            end = s.indexOf('\t', begin);
            if (end < 0)
                break;
            entry.from = s.substring(begin, end);
            begin = end + 1;
            end = s.indexOf('\t', begin);
            if (end < 0)
                break;
            entry.date = RFC822Date.parseDate(s.substring(begin, end)); // Date.
            begin = end + 1;
            end = s.indexOf('\t', begin);
            if (end < 0)
                break;
            entry.messageId = s.substring(begin, end); // Message ID.
            begin = end + 1;
            end = s.indexOf('\t', begin);
            if (end < 0)
                break;
            String refs = s.substring(begin, end); // References.
            if (refs != null)
                entry.references = parseReferences(refs);
            begin = end + 1;
            end = s.indexOf('\t', begin);
            if (end < 0)
                break;
            if (end > begin) {
                token = s.substring(begin, end); // Byte count.
                try {
                    entry.size = Integer.parseInt(token);
                }
                catch (NumberFormatException e) {
                    Log.error(e);
                }
            }
            begin = end + 1;
            end = s.indexOf('\t', begin);
            if (end < 0)
                end = s.length(); // This might be the last token.
            if (end > begin) {
                token = s.substring(begin, end); // Line count.
                try {
                    entry.lineCount = Integer.parseInt(token);
                }
                catch (NumberFormatException e) {
                    Log.error(e);
                }
            }
            // Done with this entry.
            break;
        }
        return entry;
    }

    public final int getArticleNumber()
    {
        return messageNumber;
    }

    public String toString()
    {
        return toString(1);
    }

    public String toString(int depth)
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (SHOW_MESSAGE_NUMBERS) {
            sb.append(Utilities.rightJustify(getSequenceNumber(), 4));
            sb.append(' ');
        }
        sb.append("  "); // to
        sb.append(formatFlags());
        sb.append("  ");
        sb.append(formatDate());
        sb.append("  ");
        sb.append(formatFrom(20));
        sb.append("  ");
        sb.append(formatSize());
        sb.append(Utilities.spaces(depth+1));
        if (subject != null)
            sb.append(subject);
        return sb.toString();
    }

    protected String formatFrom(int fieldWidth)
    {
        String s = from;
        int index = s.indexOf('<');
        if (index > 0)
            s = s.substring(0, index).trim();
        else {
            index = s.indexOf('(');
            if (index > 0)
                s = s.substring(0, index).trim();
        }
        if (s.length() >= 2) {
            if (s.charAt(0) == '"' && s.charAt(s.length()-1) == '"') {
                s = s.substring(1, s.length()-1);
            }
        }
        if (fieldWidth > 0) {
            int length = s.length();
            if (length > fieldWidth)
                s = s.substring(0, fieldWidth);
            else if (length < fieldWidth)
                s = s.concat(Utilities.spaces(fieldWidth - length));
        }
        return s;
    }
}
