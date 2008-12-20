/*
 * Netrc.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import java.io.InputStream;
import java.io.IOException;
import java.util.Vector;
import java.util.StringTokenizer;

public final class Netrc
{
    private static Vector logins;
    private static long lastModified;

    public static Login getLogin(String host)
    {
        if (host == null)
            return null;
        parseNetrc();
        if (logins == null)
            return null;
        final int limit = logins.size();
        for (int i = 0; i < limit; i++) {
            Login login = (Login) logins.get(i);
            if (host.equals(login.host))
                return login;
        }
        return null;
    }

    public static String getPassword(String host, String user)
    {
        if (host == null)
            return null;
        parseNetrc();
        if (logins == null)
            return null;
        final int limit = logins.size();
        for (int i = 0; i < limit; i++) {
            Login login = (Login) logins.get(i);
            if (host.equals(login.host)) {
                if (user == null || user.equals(login.user))
                    return login.password;
            }
        }
        return null;
    }

    // Returns Vector of Login objects.
    private static void parseNetrc()
    {
        File file = File.getInstance(Directories.getUserHomeDirectory(), ".netrc");
        if (!file.isFile() || !file.canRead()) {
            logins = null; // Nuke old cache, if any.
            return;
        }
        // File exists and is readable.
        if (logins != null) {
            // We have a cache.
            if (file.lastModified() == lastModified)
                return; // File hasn't changed.
            // Cache is stale.
            logins = null;
        }
        try {
            lastModified = file.lastModified();
            int length = (int) file.length();
            byte bytes[] = new byte[length];
            InputStream in = file.getInputStream();
            if (in.read(bytes) != length)
                return;
            in.close();
            String s = new String(bytes);
            StringTokenizer st = new StringTokenizer(s);
            String host = null;
            String user = null;
            String password = null;
            logins = new Vector();
            while (st.hasMoreTokens()) {
                String token = st.nextToken();
                if (token.equals("machine")) {
                    // Add current entry to vector.
                    if (host != null && user != null && password != null)
                        logins.add(new Login(host, user, password));
                    // Start new entry.
                    host = st.nextToken();
                    user = null;
                    password = null;
                } else if (token.equals("login")) {
                    user = st.nextToken();
                } else if (token.equals("password"))
                    password = st.nextToken();
            }
            // Add final entry to vector.
            if (host != null && user != null && password != null)
                logins.add(new Login(host, user, password));
        }
        catch (IOException e) {
            Log.error(e);
        }
        if (logins.size() == 0)
            logins = null;
    }
}
