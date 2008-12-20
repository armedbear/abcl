/*
 * FtpFile.java
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

public final class FtpFile extends File
{
    private FtpFile()
    {
        isRemote = true;
        protocol = PROTOCOL_FTP;
        port = 21;
    }

    public FtpFile(String hostName, String path, String userName, String password, int port)
    {
        this();
        this.hostName = hostName;
        this.canonicalPath = path;
        this.userName = userName;
        this.password = password;
        this.port = port;
    }

    public static FtpFile getFtpFile(String name)
    {
        FtpFile file = new FtpFile();
        if (file.initRemote(name, PREFIX_FTP))
            return file;
        return null;
    }

    public static FtpFile getFtpFile(FtpFile directory, String name)
    {
        FtpFile file = new FtpFile();

        file.hostName = directory.hostName;
        file.userName = directory.userName;
        file.password = directory.password;
        file.port = directory.port;

        if (Utilities.isFilenameAbsolute(name))
            file.canonicalPath = canonicalize(name, "/");
        else
            file.canonicalPath = canonicalize(appendNameToPath(directory.canonicalPath(), name, '/'), "/");

        return file;
    }

    public static FtpFile getFtpFile(String host, String path)
    {
        if (host == null)
            return null;

        // Path can be null.

        FtpFile file = new FtpFile();

        file.hostName = host;
        file.canonicalPath = path;

        return file;
    }

    public File getRoot()
    {
        FtpFile file = new FtpFile();

        file.hostName = this.hostName;
        file.userName = this.userName;
        file.password = this.password;
        file.port = this.port;
        file.canonicalPath = "/";
        file.type = TYPE_DIRECTORY;

        return file;
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
        if (canonicalPath() == null || canonicalPath.equals("/"))
            return null; // No parent.

        int index = canonicalPath.lastIndexOf('/');

        if (index < 0)
            return null; // No parent.

        if (index == 0) // "/usr"
            return new FtpFile(hostName, "/", userName, password, port);

        return new FtpFile(hostName, canonicalPath.substring(0, index), userName, password, port);
    }

    public boolean isDirectory()
    {
        if (type == TYPE_UNKNOWN) {
            FtpSession session = FtpSession.getSession(this);
            if (session != null) {
                if (session.isDirectory(canonicalPath()))
                    type = TYPE_DIRECTORY;
                session.unlock();
            }
        }
        return type == TYPE_DIRECTORY;
    }

    public boolean isLink()
    {
        return type == TYPE_LINK;
    }

    public String getDirectoryListing()
    {
        return getDirectoryListing(false);
    }
    
    public String getDirectoryListing(boolean forceRefresh)
    {
        if (!forceRefresh) {
            String listing =
                DirectoryCache.getDirectoryCache().getListing(this);
            if (listing != null)
                return listing;
        }
        FtpSession session = FtpSession.getSession(this);
        if (session == null)
            return null;
        String listing = session.getDirectoryListing(canonicalPath());
        session.unlock();
        if (listing != null)
            DirectoryCache.getDirectoryCache().put(this, listing);
        return listing;
    }

    public String netPath()
    {
        FastStringBuffer sb = new FastStringBuffer(256);
        sb.append(PREFIX_FTP);
        sb.append(hostName);
        if (port != 21) {
            sb.append(':');
            sb.append(port);
        }
        if (canonicalPath != null)
            sb.append(canonicalPath);
        return sb.toString();
    }
}
