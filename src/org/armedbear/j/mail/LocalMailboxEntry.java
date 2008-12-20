/*
 * LocalMailboxEntry.java
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

import java.util.List;
import org.armedbear.j.Headers;
import org.armedbear.j.Log;

public final class LocalMailboxEntry extends MailboxEntry
{
    private long messageStart;
    private long nextMessageStart;
    private String uidl;

    public LocalMailboxEntry(int messageNumber, long messageStart, String s)
    {
        this.messageNumber = messageNumber;
        this.messageStart = messageStart;
        Headers headers = Headers.parse(s);
        subject = RFC2047.decode(headers.getValue(Headers.SUBJECT));
        date = RFC822Date.parseDate(headers.getValue(Headers.DATE));
        from = MailAddress.parseAddresses(RFC2047.decode(
            headers.getValue(Headers.FROM)));
        replyTo = MailAddress.parseAddresses(RFC2047.decode(
            headers.getValue(Headers.REPLY_TO)));
        to = MailAddress.parseAddresses(RFC2047.decode(
            headers.getValue(Headers.TO)));
        cc = MailAddress.parseAddresses(RFC2047.decode(
            headers.getValue(Headers.CC)));
        messageId = headers.getValue(Headers.MESSAGE_ID);
        inReplyTo = parseInReplyTo(headers.getValue(Headers.IN_REPLY_TO));
        String refs = headers.getValue(Headers.REFERENCES);
        if (refs != null)
            references = parseReferences(refs);
        uidl = headers.getValue(Headers.X_UIDL);
        String status = headers.getValue(Headers.X_J_STATUS);
        if (status != null) {
            try {
                flags = Integer.parseInt(status);
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
        }
    }

    public final long getMessageStart()
    {
        return messageStart;
    }

    public final void setMessageStart(long offset)
    {
        messageStart = offset;
    }

    public final long getNextMessageStart()
    {
        return nextMessageStart;
    }

    public final void setNextMessageStart(long offset)
    {
        nextMessageStart = offset;
    }

    public final String getUidl()
    {
        return uidl;
    }
}
