/*
 * MailboxURL.java
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

import java.net.MalformedURLException;
import org.armedbear.j.Log;

public abstract class MailboxURL
{
    private String baseName;
    private String limitPattern;

    protected String user;
    protected String host;
    protected int port;

    public final String getBaseName()
    {
        return baseName;
    }

    public final void setBaseName(String baseName)
    {
        this.baseName = baseName;
    }

    public final String getLimitPattern()
    {
        return limitPattern;
    }

    public final void setLimitPattern(String limitPattern)
    {
        this.limitPattern = limitPattern;
    }

    public final String getUser()
    {
        return user;
    }

    public final void setUser(String user)
    {
        this.user = user;
    }

    public final String getHost()
    {
        return host;
    }

    public final int getPort()
    {
        return port;
    }

    public static MailboxURL parse(String input)
    {
        if (input == null)
            return null;
        input = input.trim();
        if (input.length() == 0)
            return null;
        String baseName;
        String limitPattern = null;
        if (input.charAt(0) == '"') {
            int index = input.indexOf('"', 1);
            if (index < 0)
                return null;
            baseName = input.substring(1, index);
            if (index < input.length()-1)
                limitPattern = input.substring(index+1).trim();
        } else {
            int index = input.indexOf(' ');
            if (index >= 0) {
                baseName = input.substring(0, index);
                limitPattern = input.substring(index).trim();
            } else
                baseName = input;
        }
        if (baseName.length() == 0)
            return null;
        MailboxURL url = null;
        try {
            if (baseName.charAt(0) == '{') {
                // IMAP.
                url = new ImapURL(baseName);
            } else if (baseName.startsWith("pop://")) {
                // POP.
                url = new PopURL(baseName);
            } else {
                // Local.
                url = new LocalMailboxURL(baseName);
            }
        }
        catch (MalformedURLException e) {
            Log.error(e);
        }
        if (url != null) {
            url.setBaseName(baseName);
            url.setLimitPattern(limitPattern);
        }
        return url;
    }

    public abstract String getCanonicalName();
}
