/*
 * Cookie.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

import java.util.Vector;
import java.net.URL;

public final class Cookie
{
    private static Vector cookies;

    private String name;
    private String value;
    private String domain;
    private String path;
    private String expires;
    private boolean secure;

    private Cookie()
    {
    }

    public static void setCookie(URL url, String s)
    {
        Cookie cookie = new Cookie();
        String remaining = s.trim();
        while (remaining.length() > 0) {
            int index = remaining.indexOf('=');
            if (index < 0) {
                cookie.set(remaining, null);
                break;
            }
            String key = remaining.substring(0, index);
            remaining = remaining.substring(index + 1);
            index = remaining.indexOf(';');
            String value;
            if (index < 0) {
                value = remaining;
                cookie.set(key, value);
                break;
            } else {
                value = remaining.substring(0, index);
                cookie.set(key, value);
                remaining = remaining.substring(index + 1).trim();
            }
        }
        if (cookie.domain == null)
            cookie.domain = url.getHost();
        if (cookie.path == null) {
            // URL.getPath() is only available in Java 1.3!
            String file = url.getFile();
            int index = file.lastIndexOf('?');
            if (index >= 0)
                cookie.path = file.substring(0, index);
            else
                cookie.path = file;
        }
        if (cookie.isValid())
            addCookie(cookie);
    }

    // BUG! Cookies with more specific path mappings should be sent first.
    public static String getCookie(URL url)
    {
        if (cookies == null)
            return null;
        String host = url.getHost();
        // URL.getPath() is only available in Java 1.3!
        String file = url.getFile();
        int index = file.lastIndexOf('?');
        String path = index >= 0 ? file.substring(0, index) : file;
        FastStringBuffer sb = new FastStringBuffer(256);
        for (int i = cookies.size()-1; i >= 0; i--) {
            Cookie cookie = (Cookie) cookies.get(i);
            if (cookie.domain != null && host.endsWith(cookie.domain)) {
                if (cookie.path != null && path.startsWith(cookie.path)) {
                    if (sb.length() > 0)
                        sb.append("; ");
                    sb.append(cookie.name);
                    sb.append('=');
                    sb.append(cookie.value);
                }
            }
        }
        if (sb.length() == 0)
            return null;
        return sb.toString();
    }

    public static void deleteCookies()
    {
        cookies = null;
    }

    private static void addCookie(Cookie cookie)
    {
        if (cookies != null) {
            for (int i = cookies.size()-1; i >= 0; i--) {
                Cookie c = (Cookie) cookies.get(i);
                if (c.domain.equals(cookie.domain) &&
                    c.path.equals(cookie.path) &&
                    c.name.equals(cookie.name)) {
                    // BUG! Should delete cookie here if new cookie's
                    // expiration time is in the past.
                    c.value = cookie.value;
                    return;
                }
            }
        } else
            cookies = new Vector();
        cookies.add(cookie);
    }

    private void set(String key, String s)
    {
        if (key.equals("domain"))
            domain = s;
        else if (key.equals("path"))
            path = s;
        else if (key.equals("expires"))
            expires = s;
        else if (key.equals("secure"))
            secure = true;
        else {
            name = key;
            value = s;
        }
    }

    private boolean isValid()
    {
        if (name != null && name.length() > 0)
            if (value != null && value.length() > 0)
                return true;
        return false;
    }
}
