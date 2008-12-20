/*
 * PopURL.java
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

import java.net.MalformedURLException;
import org.armedbear.j.FastStringBuffer;

public final class PopURL extends MailboxURL
{
    private static final int DEFAULT_PORT = 110;

    // RFC 2384 pop://user@host:port
    public PopURL(String s) throws MalformedURLException
    {
        if (!s.startsWith("pop://"))
            throw new MalformedURLException();
        s = s.substring(6);
        port = DEFAULT_PORT;
        // The user name may be enclosed in quotes.
        if (s.length() > 0 && s.charAt(0) == '"') {
            int index = s.indexOf('"', 1);
            if (index >= 0) {
                user = s.substring(1, index);
                s = s.substring(index + 1);
            } else
                throw new MalformedURLException();
            // We've got the user name.
            if (s.length() == 0) {
                // No host specified.
                host = "127.0.0.1";
                return;
            }
            if (s.charAt(0) != '@')
                throw new MalformedURLException();
            s = s.substring(1); // Skip '@'.
            index = s.indexOf(':');
            if (index >= 0) {
                try {
                    port = Integer.parseInt(s.substring(index + 1));
                }
                catch (Exception e) {
                    throw new MalformedURLException();
                }
                s = s.substring(0, index);
            }
            // What's left is the host name.
            host = s;
        } else {
            int index = s.indexOf(':');
            if (index >= 0) {
                try {
                    port = Integer.parseInt(s.substring(index + 1));
                }
                catch (Exception e) {
                    throw new MalformedURLException();
                }
                s = s.substring(0, index);
            }
            index = s.indexOf('@');
            if (index >= 0) {
                user = s.substring(0, index);
                host = s.substring(index + 1);
            } else {
                user = s;
                host = "127.0.0.1";
            }
        }
    }

    public boolean equals(Object object)
    {
        if (!(object instanceof PopURL))
            return false;
        PopURL url = (PopURL) object;
        if (host != url.host) {
            if (host == null)
                return false;
            if (!host.equals(url.host))
                return false;
        }
        if (user != url.user) {
            if (user == null)
                return false;
            if (!user.equals(url.user))
                return false;
        }
        if (port != url.port)
            return false;
        return true;
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer("pop://");
        if (user != null) {
            if (user.indexOf('@') >= 0) {
                sb.append('"');
                sb.append(user);
                sb.append('"');
            } else
                sb.append(user);
            sb.append('@');
        }
        sb.append(host);
        if (port != DEFAULT_PORT) {
            sb.append(':');
            sb.append(port);
        }
        return sb.toString();
    }

    public String getCanonicalName()
    {
        FastStringBuffer sb = new FastStringBuffer("pop://");
        String s = user != null ? user : System.getProperty("user.name");
        if (s.indexOf('@') >= 0) {
            sb.append('"');
            sb.append(s);
            sb.append('"');
        } else
            sb.append(s);
        sb.append('@');
        sb.append(host);
        sb.append(':');
        sb.append(port);
        return sb.toString();
    }
}
