/*
 * MailboxEntry.java
 *
 * Copyright (C) 2000-2006 Peter Graves
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
import java.util.Locale;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;
import org.armedbear.j.Property;
import org.armedbear.j.Utilities;

public abstract class MailboxEntry implements Serializable
{
    // If DEBUG is true, formatDate() prints the time even if the entry is
    // more than six months old.
    private static final boolean DEBUG =
        Editor.preferences().getBooleanProperty("MailboxEntry.debug", false);

    protected static final boolean SHOW_MESSAGE_NUMBERS =
        Editor.preferences().getBooleanProperty(Property.SHOW_MESSAGE_NUMBERS);

    // Bit flags.
    public static final int SEEN     = 0x01;
    public static final int RECENT   = 0x02;
    public static final int ANSWERED = 0x04;
    public static final int DELETED  = 0x08;
    public static final int FLAGGED  = 0x10;
    public static final int TAGGED   = 0x20; // Not persistent.

    protected static final String STRING_DEFAULT = " ";
    protected static final String STRING_DELETED = "D";
    protected static final String STRING_REPLIED = "r";
    protected static final String STRING_NEW     = "N";
    protected static final String STRING_OLD     = "O";
    protected static final String STRING_FLAGGED = "!";

    protected int flags;
    protected int messageNumber;
    protected int size;
    protected String subject;
    protected RFC822Date date;
    protected MailAddress[] from;
    protected MailAddress[] replyTo;
    protected MailAddress[] to;
    protected MailAddress[] cc;
    protected String messageId;
    protected String inReplyTo;
    protected String[] references;

    // The message number displayed to the user.
    private transient int sequenceNumber;

    private transient boolean orphan;

    public final int getFlags()
    {
        return flags;
    }

    public final void setFlags(int flags)
    {
        this.flags = flags;
    }

    public final int getSize()
    {
        return size;
    }

    public final void setSize(int size)
    {
        this.size = size;
    }

    public final int getMessageNumber()
    {
        return messageNumber;
    }

    public final int getSequenceNumber()
    {
        return sequenceNumber;
    }

    public final void setSequenceNumber(int n)
    {
        sequenceNumber = n;
    }

    public final RFC822Date getDate()
    {
        return date;
    }

    public final MailAddress[] getFrom()
    {
        return from;
    }

    public final MailAddress[] getReplyTo()
    {
        return replyTo;
    }

    public final MailAddress[] getTo()
    {
        return to;
    }

    public final MailAddress[] getCc()
    {
        return cc;
    }

    public final String getSubject()
    {
        if (subject != null && subject.length() > 0)
            return subject;
        return "(no subject)";
    }

    public final String getBaseSubject()
    {
        if (subject == null)
            return null;
        final int length = subject.length();
        if (length == 0)
            return null;
        String s = subject.trim();
        while (true) {
            if (s.toLowerCase().startsWith("re:")) {
                s = s.substring(3).trim();
                continue;
            }
            if (s.length() > 0 && s.charAt(0) == '[') {
                int end = s.indexOf(']', 1);
                if (end >= 0) {
                    s = s.substring(end+1).trim();
                    continue;
                }
            }
            break;
        }
        while (s.toLowerCase().endsWith("(fwd)"))
            s = s.substring(0, s.length()-5).trim();

        // Some broken mailers (or MTAs) arbitrarily break the subject line
        // after 74 characters. If this happens to be in the middle of a word,
        // we'll end up with an extra LWSP char in the subject string when we
        // unfold the header, meaning we won't get an exact match with the
        // subject of the message being replied to, which is the whole point
        // here. So we strip out all LWSP chars before returning the base
        // subject.
        FastStringBuffer sb = new FastStringBuffer();
        for (int i = 0, limit = s.length(); i < limit; i++) {
            char c = s.charAt(i);
            if (c != ' ' && c != '\t')
                sb.append(c);
        }
        return sb.toString();
    }

    public final boolean subjectIsReply()
    {
        if (subject != null && subject.toLowerCase().startsWith("re:"))
            return true;
        return false;
    }

    public final String getMessageId()
    {
        return messageId;
    }

    public final String getInReplyTo()
    {
        return inReplyTo;
    }

    public final String[] getReferences()
    {
        return references;
    }

    public final void setOrphan(boolean b)
    {
        orphan = b;
    }

    public String getUidl()
    {
        return null;
    }

    public String formatSubject()
    {
        if (subject == null)
            return "";
        return subject;
    }

    public final boolean isDeleted()
    {
        return (flags & DELETED) == DELETED;
    }

    public final boolean isTagged()
    {
        return (flags & TAGGED) == TAGGED;
    }

    public final boolean isFlagged()
    {
        return (flags & FLAGGED) == FLAGGED;
    }

    public final boolean isAnswered()
    {
        return (flags & ANSWERED) == ANSWERED;
    }

    public final boolean isNew()
    {
        return (flags & (SEEN | DELETED | RECENT)) == RECENT;
    }

    public final boolean isRead()
    {
        return (flags & SEEN) != 0;
    }

    public final boolean isUnread()
    {
        return (flags & (SEEN | DELETED)) == 0;
    }

    public final void tag()
    {
        flags |= TAGGED;
    }

    public final void untag()
    {
        flags &= ~TAGGED;
    }

    public final void toggleTag()
    {
        if ((flags & TAGGED) != 0)
            flags &= ~TAGGED;
        else
            flags |= TAGGED;
    }

    public final void flag()
    {
        flags |= FLAGGED;
    }

    public final void unflag()
    {
        flags &= ~FLAGGED;
    }

    public final void toggleFlag()
    {
        if ((flags & FLAGGED) != 0)
            flags &= ~FLAGGED;
        else
            flags |= FLAGGED;
    }

    protected String formatSize()
    {
        if (size < 1000)
            return Utilities.rightJustify(size, 4);
        if (size < 10000) {
            int k = size / 1000;
            int remainder = size % 1000;
            int h = remainder / 100;
            if (remainder % 100 > 49) {
                ++h;
                if (h == 10) {
                    ++k;
                    h = 0;
                }
            }
            if (k < 10) {
                FastStringBuffer sb = new FastStringBuffer();
                sb.append(String.valueOf(k));
                sb.append('.');
                sb.append(String.valueOf(h));
                sb.append('K');
                return sb.toString();
            }
            // else fall through...
        }
        if (size < 1000000) {
            int k = size / 1000;
            if (size % 1000 > 499)
                ++k;
            if (k < 1000)
                return Utilities.rightJustify(k, 3) + "K";
            // else fall through...
        }
        int m = size / 1000000;
        if (size % 1000000 > 499999)
            ++m;
        return Utilities.rightJustify(m, 3) + "M";
    }

    protected char getToChar()
    {
        if (isFlagged())
            return '!';
        if (isFromMe())
            return 'F';
        char c = ' ';
        if (to != null) {
            for (int i = to.length-1; i >= 0; i--) {
                MailAddress a = to[i];
                if (a.matches(Mail.getUserMailAddress())) {
                    // Addressed to me.
                    if (to.length == 1)
                        c = '+';
                    else
                        return 'T';
                }
            }
        }
        if (c == '+') {
            // Addressed to me alone.
            if (cc != null && cc.length > 0) // Copied to others or to me.
                return 'T';
            return c;
        }
        if (cc != null) {
            for (int i = cc.length-1; i >= 0; i--) {
                MailAddress a = cc[i];
                if (a.matches(Mail.getUserMailAddress())) {
                    // Copied to me.
                    return 'C';
                }
            }
        }
        return ' ';
    }

    private boolean isFromMe()
    {
        if (from != null) {
            MailAddress a = from[0];
            if (a != null && a.matches(Mail.getUserMailAddress()))
                return true;
        }
        return false;
    }

    protected String formatFlags()
    {
        if (isAnswered())
            return STRING_REPLIED;
        else if ((flags & (SEEN | RECENT)) == RECENT) // Might be deleted.
            return STRING_NEW;
        else if ((flags & SEEN) == 0) // Might be deleted.
            return STRING_OLD;
        else
            return STRING_DEFAULT;
    }

    private static final SimpleDateFormat dateFormat1 =
        new SimpleDateFormat("MMM dd HH:mm", Locale.US);

    private static final SimpleDateFormat dateFormat2 =
        new SimpleDateFormat("MMM dd  yyyy", Locale.US);

    private static final String NULL_DATE = "null        ";

    private static final long SIX_MONTHS = (long) 6 * 30 * 24 * 60 * 60 * 1000;

    protected String formatDate()
    {
        if (date == null)
            return NULL_DATE;
        long millis = date.getTime();
        if (millis == 0)
            return NULL_DATE;
        if (DEBUG || System.currentTimeMillis() - millis < SIX_MONTHS)
            return dateFormat1.format(date.getDate());
        return dateFormat2.format(date.getDate());
    }

    protected String formatFrom(int fieldWidth)
    {
        String s = null;
        if (isFromMe() && to != null && to.length > 0) {
            MailAddress a = to[0];
            s = a.getPersonal();
            if (s == null || s.length() == 0)
                s = a.getAddress();
            if (s != null)
                s = "To: " + s;
        } else if (from != null && from.length > 0) {
            MailAddress a = from[0];
            s = a.getPersonal();
            if (s == null || s.length() == 0)
                s = a.getAddress();
        }
        if (s == null)
            s = "";
        final int length = s.length();
        if (length > fieldWidth)
            s = s.substring(0, fieldWidth);
        else if (length < fieldWidth)
            s = s + Utilities.spaces(fieldWidth - length);
        return s;
    }

    public String toString()
    {
        return toString(1);
    }

    public String toString(int depth)
    {
        FastStringBuffer sb = new FastStringBuffer(128);
        if (SHOW_MESSAGE_NUMBERS) {
            sb.append(Utilities.rightJustify(sequenceNumber, 5));
            sb.append(' ');
        }
        sb.append(getToChar());
        sb.append(' ');
        sb.append(formatFlags());
        sb.append("  ");
        sb.append(formatDate());
        sb.append("  ");
        sb.append(formatFrom(20));
        sb.append("  ");
        sb.append(formatSize());
        sb.append("  ");
        if (orphan)
            sb.append("- ");
        else if (depth > 1)
            sb.append(Utilities.spaces((depth-1)*2));
        sb.append(formatSubject());
        return sb.toString();
    }

    protected static String parseInReplyTo(String s)
    {
        if (s != null) {
            int begin = s.indexOf('<');
            if (begin >= 0) {
                int end = s.indexOf('>', begin+1);
                if (end > begin)
                    return s.substring(begin, end+1);
            }
        }
        return null;
    }

    protected static String[] parseReferences(String s)
    {
        ArrayList list = null;
        int begin = 0;
        while (true) {
            begin = s.indexOf('<', begin);
            if (begin < 0)
                break;
            int end = s.indexOf('>', begin+1);
            if (end < 0)
                break;
            if (list == null)
                list = new ArrayList();
            list.add(s.substring(begin, ++end));
            begin = end;
        }
        if (list == null)
            return null;
        String[] array = new String[list.size()];
        return (String[]) list.toArray(array);
    }
}
