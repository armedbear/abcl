/*
 * ImapMailboxEntry.java
 *
 * Copyright (C) 2000-2007 Peter Graves <peter@armedbear.org>
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

package org.armedbear.j.mail;

import java.io.Serializable;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Date;
import java.util.TimeZone;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;
import org.armedbear.j.StringPair;
import org.armedbear.j.Utilities;

/*package*/ final class ImapMailboxEntry extends MailboxEntry implements Serializable
{
  private transient ImapMailbox mailbox;

  private int uid;
  private RFC822Date arrival;

  private ImapMailboxEntry()
  {
  }

  // For testing only!
  /*package*/ ImapMailboxEntry(int uid)
  {
    this.uid = uid;
  }

  public final ImapMailbox getMailbox()
  {
    return mailbox;
  }

  public final void setMailbox(ImapMailbox mailbox)
  {
    this.mailbox = mailbox;
  }

  public final int getUid()
  {
    return uid;
  }

  public final RFC822Date getArrival()
  {
    return arrival;
  }

  private static final String INTERNALDATE_START = "INTERNALDATE ";
  private static final String RFC822_SIZE_START = "RFC822.SIZE ";
  private static final String ENVELOPE_START = "ENVELOPE (";
  private static final String REFERENCES_START = "BODY[HEADER.FIELDS (\"REFERENCES\")]";

  public static ImapMailboxEntry parseEntry(ImapMailbox mailbox, final String s)
  {
    ImapMailboxEntry entry = new ImapMailboxEntry();
    entry.mailbox = mailbox;
    entry.messageNumber = parseMessageNumber(s);
    if (entry.messageNumber < 1)
      {
        Log.error("can't parse message number");
        return null;
      }
    entry.uid = parseUid(s);
    if (entry.uid < 1)
      {
        Log.error("can't parse uid");
        return null;
      }
    entry.flags = parseFlags(s);
    int index = s.indexOf(INTERNALDATE_START);
    if (index < 0)
      {
        Log.error("can't find INTERNALDATE");
        return null;
      }
    StringPair p =
      parseQuoted(s.substring(index + INTERNALDATE_START.length()));
    if (p == null)
      {
        Log.error("can't parse INTERNALDATE");
        return null;
      }
    entry.arrival = entry.parseInternalDate(p.first.trim());
    String remaining = p.second;
    index = remaining.indexOf(RFC822_SIZE_START);
    if (index < 0)
      {
        Log.error("can't find RFC822.SIZE");
        return null;
      }
    remaining = remaining.substring(index + RFC822_SIZE_START.length());
    entry.size = -1;
    try
      {
        entry.size = Utilities.parseInt(remaining);
      }
    catch(NumberFormatException e)
      {
        Log.error("can't parse RFC822.SIZE");
        Log.error("|" + remaining + "|");
        Log.error(e);
        return null;
      }
    index = remaining.indexOf(ENVELOPE_START);
    if (index < 0)
      {
        Log.error("can't find ENVELOPE");
        return null;
      }
    remaining = remaining.substring(index + ENVELOPE_START.length());
    // Next field is date (quoted string).
    p = parseQuoted(remaining);
    if (p == null)
      {
        Log.error("can't parse date");
        return null;
      }
    entry.date = RFC822Date.parseDate(p.first);
    remaining = p.second;
    // Next field is subject (quoted string).
    p = parseQuoted(remaining);
    if (p == null)
      {
        Log.error("can't parse subject");
        return null;
      }
    entry.subject = p.first == null ? "" : RFC2047.decode(p.first);
    if (entry.subject.indexOf('\\') >= 0)
      {
        // strip out escape chars
        String temp = entry.subject;
        final int limit = temp.length();
        FastStringBuffer sb = new FastStringBuffer();
        boolean escaped = false;
        for (int i = 0; i < limit; i++)
          {
            char c = temp.charAt(i);
            if (escaped)
              {
                sb.append(c);
                escaped =false;
              }
            else
              {
                // not escaped
                if (c == '\\')
                  escaped = true;
                else
                  sb.append(c);
              }
          }
        entry.subject = sb.toString();
      }
    remaining = p.second;
    // Next field is "From" (parenthesized list).
    p = parseParenthesizedList(remaining);
    if (p == null)
      {
        Log.error("can't parse \"From\" list");
        return null;
      }
    if (p.first != null)
      entry.from = parseAddressList(p.first);
    remaining = p.second;
    do
      {
        // Sender
        p = parseParenthesizedList(remaining);
        if (p == null)
          {
            Log.error("can't parse \"Sender\" list");
            break;
          }
        remaining = p.second;
        // Reply-To
        p = parseParenthesizedList(remaining);
        if (p == null)
          {
            Log.error("can't parse \"Reply-To\" list");
            break;
          }
        if (p.first != null)
          entry.replyTo = parseAddressList(p.first);
        remaining = p.second;
        // To
        p = parseParenthesizedList(remaining);
        if (p == null)
          {
            Log.error("can't parse \"To\" list");
            break;
          }
        if (p.first != null)
          entry.to = parseAddressList(p.first);
        remaining = p.second;
        // Cc
        p = parseParenthesizedList(remaining);
        if (p == null)
          {
            Log.error("can't parse \"Cc\" list");
            break;
          }
        if (p.first != null)
          entry.cc = parseAddressList(p.first);
        remaining = p.second;
        // Bcc
        p = parseParenthesizedList(remaining);
        if (p == null)
          {
            Log.error("can't parse \"Bcc\" list");
            break;
          }
        remaining = p.second;
        // In-Reply-To (quoted string)
        p = parseQuoted(remaining);
        if (p == null)
          {
            Log.error("can't parse \"In-Reply-To\"");
            break;
          }
        entry.inReplyTo = parseInReplyTo(p.first);
        remaining = p.second;
        p = parseQuoted(remaining);
        if (p == null)
          {
            Log.error("can't parse \"Message-ID\"");
            break;
          }
        entry.messageId = p.first;
      }
    while (false);
    if (p == null)
      return null;
    remaining = p.second;
    index = remaining.indexOf(REFERENCES_START);
    if (index >= 0)
      {
        remaining = remaining.substring(index + REFERENCES_START.length());
        p = parseQuoted(remaining);
        if (p != null)
          {
            String refs = p.first.trim();
            if (refs.length() > 0)
              entry.references = parseReferences(refs);
          }
      }
    if (entry.subject != null)
      return entry;
    Log.error("skipping entry");
    return null;
  }

  private static int parseMessageNumber(String s)
  {
    final int length = s.length();
    if (length < 2)
      return 0; // Error.
    // String must start with "* ".
    if (s.charAt(0) != '*' || s.charAt(1) != ' ')
      return 0; // Error.
    FastStringBuffer sb = new FastStringBuffer();
    for (int i = 2; i < length; i++)
      {
        char c = s.charAt(i);
        if (c >= '0' && c <= '9')
          sb.append(c);
        else
          break;
      }
    try
      {
        return Integer.parseInt(sb.toString());
      }
    catch (NumberFormatException e)
      {
        Log.error(e);
        return 0;
      }
  }

  public static int parseUid(String s)
  {
    final int length = s.length();
    if (length < 2)
      return 0; // Error.
    // String must start with "* ".
    if (s.charAt(0) != '*' || s.charAt(1) != ' ')
      return 0; // Error.
    int index = s.indexOf("UID ");
    if (index < 0)
      return 0;
    FastStringBuffer sb = new FastStringBuffer();
    for (int i = index + 4; i < length; i++)
      {
        char c = s.charAt(i);
        if (c >= '0' && c <= '9')
          sb.append(c);
        else
          break;
      }
    try
      {
        return Integer.parseInt(sb.toString());
      }
    catch (NumberFormatException e)
      {
        Log.error(e);
        return 0;
      }
  }

  private static final String FLAGS_START = "FLAGS (";

  public static int parseFlags(String s)
  {
    int flags = 0;
    final int index = s.indexOf(FLAGS_START);
    if (index >= 0)
      {
        StringPair p = parseParenthesized(s.substring(index));
        if (p != null && p.first != null)
          {
            String flagsList = p.first.toLowerCase();
            if (flagsList.length() > 0)
              {
                if (flagsList.indexOf("seen") >= 0)
                  flags |= SEEN;
                if (flagsList.indexOf("answered") >= 0)
                  flags |= ANSWERED;
                if (flagsList.indexOf("recent") >= 0)
                  flags |= RECENT;
                if (flagsList.indexOf("deleted") >= 0)
                  flags |= DELETED;
                if (flagsList.indexOf("flagged") >= 0)
                  flags |= FLAGGED;
              }
          }
      }
    return flags;
  }

  private static StringPair parseQuoted(String s)
  {
    s = s.trim();
    final int slen = s.length();
    if (slen == 0)
      return null;
    String quoted = null;
    String remaining = null;
    if (s.charAt(0) == '{')
      {
        int end = s.indexOf('}', 1);
        if (end < 0)
          {
            Log.error("parseQuoted: bad literal");
            return null;
          }
        int length = 0;
        try
          {
            length = Integer.parseInt(s.substring(1, end));
          }
        catch (NumberFormatException e)
          {
            Log.error(e);
          }
        if (length == 0)
          {
            Log.error("parseQuoted: length of literal is zero");
            return null;
          }
        int begin = s.indexOf('\n', end + 1);
        if (begin < 0)
          {
            Log.error("parseQuoted: no LF after literal");
            return null;
          }
        ++begin; // Skip LF.
        end = begin + length;
        if (end > slen)
          {
            Log.error("parseQuoted end > slen");
            return null;
          }
        quoted = s.substring(begin, end);
        remaining = s.substring(end);
      }
    else if (s.startsWith("NIL"))
      {
        quoted = null;
        remaining = s.substring(3).trim();
      }
    else
      {
        final int begin = s.indexOf('"');
        if (begin < 0)
          return null;
        int end = begin + 1;
        while (end < slen)
          {
            char c = s.charAt(end);
            if (c == '\\')
              {
                if (end < slen - 1)
                  ++end;
                else
                  return null; // REVIEW
              }
            else if (c == '"')
              break;
            ++end;
          }
        if (end == slen)
          // reached end of string without closing quote
          return null;
        quoted = s.substring(begin + 1, end);
        remaining = s.substring(end + 1);
      }
    return new StringPair(quoted, remaining);
  }

  private static StringPair parseParenthesized(String s)
  {
    int begin = s.indexOf('(');
    if (begin < 0)
      return null;
    int end = -1;
    final int limit = s.length();
    boolean inQuote = false;
    char quoteChar = '\0';
    for (int i = begin + 1; i < limit; i++)
      {
        char c = s.charAt(i);
        if (inQuote)
          {
            if (c == quoteChar)
              inQuote = false;
          }
        else
          {
            // Not in quote.
            if (c == '"' || c == '\'')
              {
                inQuote = true;
                quoteChar = c;
              }
            else if (c == ')')
              {
                end = i;
                break;
              }
          }
      }
    if (end < 0)
      return null;
    String parenthesized = s.substring(begin + 1, end);
    String remaining = s.substring(end + 1);
    return new StringPair(parenthesized, remaining);
  }

  static private StringPair parseParenthesizedList(String s)
  {
    s = s.trim();
    if (s.startsWith("NIL"))
      return new StringPair(null, s.substring(3).trim());
    final int begin = s.indexOf("((");
    if (begin < 0)
      return null;
    int end = -1;
    final int slen = s.length();
    boolean in_quote = false;
    char quote_char = '\0';
    for (int i = begin + 2; i < slen; i++)
      {
        char c = s.charAt(i);
        if (in_quote)
          {
            if (c == quote_char)
              in_quote = false;
          }
        else
          {
            // not in quote
            if (c == '"' || c == '\'')
              {
                in_quote = true;
                quote_char = c;
              }
            else if (c == ')' && i < slen - 1 && s.charAt(i + 1) == ')')
              {
                end = i;
                break;
              }
          }
      }
    if (end < 0)
      return null;
    String list = s.substring(begin, end + 2);
    String remaining = s.substring(end + 2);
    return new StringPair(list, remaining);
  }

  private static MailAddress[] parseAddressList(String list)
  {
    if (list == null)
      return null;
    ArrayList addresses = new ArrayList();
    String remaining = list.substring(1, list.length() - 1);
    while (remaining.length() > 0)
      {
        StringPair p = parseParenthesized(remaining);
        if (p == null)
          {
            Log.error("parseAddressList error");
            Log.error("list = |" + list + "|");
            Log.error("remaining = |" + remaining + "|");
            return null;
          }
        String s = p.first; // The address.
        MailAddress address = parseAddress(s);
        if (address == null)
          {
            Log.error("**** parseAddress returned null");
            Log.error("s = |" + s + "|");
          }
        if (address != null)
          addresses.add(address);
        remaining = p.second;
      }
    if (addresses.size() == 0)
      return null;
    MailAddress[] array = new MailAddress[addresses.size()];
    return (MailAddress[]) addresses.toArray(array);
  }

  private static MailAddress parseAddress(String s)
  {
    StringPair p = parseQuoted(s);
    if (p == null) // Error.
      return null;
    String encodedPersonal = p.first;
    String remaining = p.second;
    p = parseQuoted(remaining);
    if (p == null) // Error.
      return null;
    String sourceRoute = p.first;
    remaining = p.second;
    p = parseQuoted(remaining);
    if (p == null) // Error.
      return null;
    String mailName = p.first;
    remaining = p.second;
    p = parseQuoted(remaining);
    if (p == null) // Error.
      return null;
    String domainName = p.first;
    remaining = p.second;
    if (remaining.length() > 0)
      Log.error("**** parseAddress: unexpected string remaining ****");
    return new MailAddress(encodedPersonal, mailName + '@' + domainName);
  }

  private static SimpleDateFormat internalDateFormat = new SimpleDateFormat("dd-MMM-yyyy HH:mm:ss");

  private static RFC822Date parseInternalDate(String internalDate)
  {
    Date date = null;
    int index = internalDate.indexOf(' ');
    if (index >= 0)
      {
        index = internalDate.indexOf(' ', index+1);
        if (index >= 0)
          {
            String dateString = internalDate.substring(0, index);
            String timeZone = internalDate.substring(index + 1);
            TimeZone tz = TimeZone.getTimeZone("GMT" + timeZone);
            if (tz != null)
              internalDateFormat.setTimeZone(tz);
            try
              {
                date = internalDateFormat.parse(dateString);
              }
            catch (Throwable t)
              {
                Log.error(t);
              }
          }
      }
    return new RFC822Date(date);
  }
}
