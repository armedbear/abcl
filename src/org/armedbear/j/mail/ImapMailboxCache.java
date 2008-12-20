/*
 * ImapMailboxCache.java
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

import java.io.BufferedInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.OutputStream;
import java.io.Serializable;
import java.util.ArrayList;
import java.util.Enumeration;
import java.util.List;
import java.util.Properties;
import org.armedbear.j.Debug;
import org.armedbear.j.Directories;
import org.armedbear.j.File;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;

public final class ImapMailboxCache implements Serializable
{
    private static Properties catalog;

    private transient ImapMailbox mailbox;
    private final String mailboxName;
    private final int uidValidity;
    private final ArrayList entries;

    // Force serialization to be dependent on ImapMailboxEntry.
    private final ImapMailboxEntry dummy = new ImapMailboxEntry(0);

    public ImapMailboxCache(ImapMailbox mailbox)
    {
        this.mailbox = mailbox;
        mailboxName = mailbox.getName();
        uidValidity = mailbox.getUidValidity();
        entries = new ArrayList(mailbox.getEntries());
    }

    private final void setMailbox(ImapMailbox mailbox)
    {
        this.mailbox = mailbox;
    }

    public final List getEntries()
    {
        return entries;
    }

    public void writeCache()
    {
        Runnable r = new Runnable() {
            public void run()
            {
                writeCacheInternal();
            }
        };
        new Thread(r).start();
    }

    private void writeCacheInternal()
    {
        try {
            Log.debug("ImapMailboxCache.writeCacheInternal " + entries.size() +
                " entries");
            Debug.assertTrue(uidValidity != 0);
            File temp = Utilities.getTempFile();
            ObjectOutputStream objectOut =
                new ObjectOutputStream(temp.getOutputStream());
            objectOut.writeObject(this);
            objectOut.flush();
            objectOut.close();
            Utilities.deleteRename(temp, getCacheFile(mailbox));
            Log.debug("ImapMailboxCache.writeCacheInternal completed");
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public static ImapMailboxCache readCache(ImapMailbox mb)
    {
        File file = ImapMailboxCache.getCacheFile(mb);
        if (file == null || !file.isFile())
            return null;
        ObjectInputStream in = null;
        try {
            in = new ObjectInputStream(new BufferedInputStream(
                file.getInputStream()));
            ImapMailboxCache cache = (ImapMailboxCache) in.readObject();
            cache.setMailbox(mb);
            return cache;
        }
        catch (Exception e) {
            Log.debug("ImapMailboxCache.readCache returning null");
            return null;
        }
        finally {
            if (in != null) {
                try {
                    in.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
        }
    }

    public boolean isValid()
    {
        if (entries == null)
            return false;
        if (entries.size() == 0)
            return false;
        if (mailboxName == null)
            return false;
        if (!mailboxName.equals(mailbox.getName()))
            return false;
        return uidValidity == mailbox.getSession().getUidValidity();
    }

    private static synchronized File getCacheFile(ImapMailbox mb)
    {
        File directory =
            File.getInstance(Directories.getMailDirectory(), "imap");
        if (!directory.isDirectory()) {
            directory.mkdirs();
            if (!directory.isDirectory()) {
                Log.error("can't create imap dir");
                return null;
            }
        }
        boolean modified = false;
        File catalogFile = File.getInstance(directory, "catalog");
        if (catalog == null) {
            catalog = new Properties();
            // Load the catalog.
            try {
                if (catalogFile.isFile()) {
                    InputStream in = catalogFile.getInputStream();
                    catalog.load(in);
                    in.close();
                }
            }
            catch (IOException e) {
                Log.error(e);
            }
            // Remove obsolete entries from catalog.
            Properties temp = new Properties();
            Enumeration keys = catalog.keys();
            while (keys.hasMoreElements()) {
                String key = (String) keys.nextElement();
                String value = (String) catalog.get(key);
                // Make sure key is canonical name.
                if (key.indexOf('@') < 0 || key.indexOf(':') < 0) {
                    Log.debug("removing obsolete entry " + key + '=' + value);
                    // Not canonical name. Delete corresponding file.
                    File file = File.getInstance(directory, value);
                    if (file != null && file.isFile())
                        file.delete();
                    modified = true;
                } else
                    temp.put(key, value);
            }
            if (modified)
                catalog = temp;
        }
        final String mailboxName = mb.getUrl().getCanonicalName();
        final String fileName = catalog.getProperty(mailboxName);
        File file;
        if (fileName != null)
            file = File.getInstance(directory, fileName);
        else {
            file = Utilities.getTempFile(directory);
            catalog.put(mailboxName, file.getName());
            modified = true;
        }
        if (modified) {
            try {
                OutputStream out = catalogFile.getOutputStream();
                catalog.save(out, null);
                out.flush();
                out.close();
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
        return file;
    }
}
