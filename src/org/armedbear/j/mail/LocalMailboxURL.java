/*
 * LocalMailboxURL.java
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
import org.armedbear.j.File;

public final class LocalMailboxURL extends MailboxURL
{
    private final File file;

    public LocalMailboxURL(String s) throws MalformedURLException
    {
        if (s.startsWith("mailbox:"))
            s = s.substring(8);
        file = File.getInstance(s);
        if (file == null)
            throw new MalformedURLException();
    }

    public LocalMailboxURL(File file)
    {
        this.file = file;
        setBaseName("mailbox:" + file.canonicalPath());
    }

    public final File getFile()
    {
        return file;
    }

    public boolean equals(Object object)
    {
        if (!(object instanceof LocalMailboxURL))
            return false;
        return file.equals(((LocalMailboxURL)object).getFile());
    }

    public String getCanonicalName()
    {
        return file.canonicalPath();
    }
}
