/*
 * HttpFile.java
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

package org.armedbear.j;

public final class HttpFile extends File
{
    private File cache;
    private String headers;
    private String contentType;

    private HttpFile()
    {
        isRemote = true;
    }

    private HttpFile(String hostName, String path, int protocol, int port)
    {
        this();
        this.hostName = hostName;
        this.canonicalPath = path;
        this.protocol = protocol;
        this.port = port;
    }

    public static HttpFile getHttpFile(String name)
    {
        HttpFile file = new HttpFile();
        if (name.startsWith(PREFIX_HTTP)) {
            name = name.substring(PREFIX_HTTP.length());
            file.protocol = PROTOCOL_HTTP;
        } else if (name.startsWith(PREFIX_HTTPS)) {
            name = name.substring(PREFIX_HTTPS.length());
            file.protocol = PROTOCOL_HTTPS;
        }
        int index = name.indexOf('/');
        if (index < 0) {
            file.hostName = name;
            file.canonicalPath = "/";
        } else {
            file.hostName = name.substring(0, index);
            file.canonicalPath = name.substring(index);
        }
        index = file.hostName.indexOf(':');
        if (index >= 0) {
            try {
                file.port = Integer.parseInt(file.hostName.substring(index + 1));
                file.hostName = file.hostName.substring(0, index);
            }
            catch (NumberFormatException e) {
                Log.error(e);
                return null;
            }
        }
        if (file.port == 0)
            file.port = file.protocol == PROTOCOL_HTTPS ? 443 : 80;
        return file;
    }

    public static HttpFile getHttpFile(HttpFile directory, String name)
    {
        if (name.startsWith("http://") || name.startsWith("https://")) {
            // Ignore directory.
            return getHttpFile(name);
        } else if (name.startsWith("//")) {
            switch (directory.protocol) {
                case PROTOCOL_HTTP:
                    return getHttpFile("http:" + name);
                case PROTOCOL_HTTPS:
                    return getHttpFile("https:" + name);
                default:
                    Debug.assertTrue(false);
                    return null;
            }
        } else if (name.startsWith("/")) {
            return new HttpFile(directory.hostName, name, directory.protocol, directory.port);
        } else {
            return new HttpFile(directory.hostName,
                canonicalize(appendNameToPath(directory.canonicalPath(), name, '/'), "/"),
                directory.protocol, directory.port);
        }
    }

    public final File getCache()
    {
        return cache;
    }

    public final void setCache(File cache)
    {
        this.cache = cache;
    }

    public final String getHeaders()
    {
        return headers;
    }

    public final void setHeaders(String s)
    {
        headers = s;
    }

    public final String getContentType()
    {
        return contentType;
    }

    public final void setContentType(String s)
    {
        contentType = s;
    }

    public final File getRoot()
    {
        return new HttpFile(hostName, "/", protocol, port);
    }

    public final String getSeparator()
    {
        return "/";
    }

    public final char getSeparatorChar()
    {
        return '/';
    }

    public File getParentFile()
    {
        if (canonicalPath() == null || canonicalPath.equals("/")) {
            // The file might really be "http://www.cnn.com/index.html", but
            // it might appear to be "http://www.cnn.com/".
            return new HttpFile(hostName, "/", protocol, port);
        }
        // Strip query (if any).
        int index = canonicalPath.indexOf('?');
        String stripped = index >= 0 ? canonicalPath.substring(0, index) : canonicalPath;
        index = stripped.lastIndexOf('/');
        if (index < 0) {
            // No parent.
            return null;
        } else if (index == 0) {
            // "/index.html"
            return new HttpFile(hostName, "/", protocol, port);
        } else {
            return new HttpFile(hostName, stripped.substring(0, index),
                protocol, port);
        }
    }

    public String netPath()
    {
        FastStringBuffer sb = new FastStringBuffer(256);
        if (protocol == PROTOCOL_HTTP) {
            sb.append(PREFIX_HTTP);
        } else if (protocol == PROTOCOL_HTTPS) {
            sb.append(PREFIX_HTTPS);
        } else {
            Debug.assertTrue(false);
            return null;
        }
        sb.append(hostName);
        int defaultPort = protocol == PROTOCOL_HTTPS ? 443 : 80;
        if (port != defaultPort) {
            sb.append(':');
            sb.append(port);
        }
        sb.append(canonicalPath);
        return sb.toString();
    }

    public String getName()
    {
        int index = canonicalPath.lastIndexOf('/');
        String name = index >= 0 ? canonicalPath.substring(index + 1) : canonicalPath;
        index = name.indexOf('?');
        if (index >= 0)
            return name.substring(0, index);
        else
            return name;
    }
}
