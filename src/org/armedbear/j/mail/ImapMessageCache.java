/*
 * ImapMessageCache.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.OutputStreamWriter;
import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.util.Enumeration;
import java.util.Iterator;
import java.util.List;
import java.util.Properties;
import org.armedbear.j.Directories;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Headers;
import org.armedbear.j.Log;
import org.armedbear.j.Utilities;

public final class ImapMessageCache
{
    private static Properties catalog;

    private final File cacheDirectory;
    private final int uidValidity;

    private ImapMessageCache(File cacheDirectory, int uidValidity)
    {
        this.cacheDirectory = cacheDirectory;
        this.uidValidity = uidValidity;
    }

    public int getUidValidity()
    {
        return uidValidity;
    }

    public static ImapMessageCache getMessageCache(ImapMailbox mb)
    {
        File cacheDirectory = getCacheDirectory(mb);
        if (cacheDirectory == null)
            return null;
        // Directory exists.
        final int uidValidity = mb.getSession().getUidValidity();
        File file = File.getInstance(cacheDirectory, "uidvalidity");
        boolean ok = false;
        if (file.isFile()) {
            try {
                BufferedReader reader =
                    new BufferedReader(new InputStreamReader(file.getInputStream()));
                String s = reader.readLine();
                reader.close();
                int n = Integer.parseInt(s);
                if (n == uidValidity)
                    ok = true;
                else
                    Log.warn("getMessageCache UIDVALIDITY has changed");
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
        if (!ok) {
            Log.debug("getMessageCache deleting old files");
            String[] files = cacheDirectory.list();
            for (int i = files.length-1; i >= 0; i--)
                File.getInstance(cacheDirectory, files[i]).delete();
            Log.debug("getMessageCache writing UIDVALIDITY " + uidValidity);
            try {
                BufferedWriter writer =
                    new BufferedWriter(new OutputStreamWriter(file.getOutputStream()));
                writer.write(String.valueOf(uidValidity));
                writer.flush();
                writer.close();
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
        return new ImapMessageCache(cacheDirectory, uidValidity);
    }

    public void store(int uid, String message, String encoding)
    {
        Log.debug("store encoding = |" + encoding + "|");
        if (encoding == null)
            encoding = "ISO-8859-1";
        if (!cacheDirectory.isDirectory()) {
            cacheDirectory.mkdirs();
            if (!cacheDirectory.isDirectory()) {
                Log.error("ImapMessageCache.store can't create cache directory");
                return;
            }
        }
        File file =
            File.getInstance(cacheDirectory, String.valueOf(uid));
        if (file == null)
            return;
        if (file.isFile()) {
            Log.debug("ImapMessageCache.store message is already cached");
            return;
        }
        try {
            BufferedWriter writer = null;
            writer =
                new BufferedWriter(new OutputStreamWriter(file.getOutputStream(),
                    encoding));
            writer.write(message);
            writer.flush();
            writer.close();
        }
        catch (UnsupportedEncodingException e) {
            Log.error(e);
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public String getMessageText(int uid)
    {
        try {
            File file =
                File.getInstance(cacheDirectory, String.valueOf(uid));
            if (file == null)
                return null;
            if (!file.isFile())
                return null;
            FastStringBuffer sb =
                new FastStringBuffer((int) (file.length() * 1.1));
            try {
                BufferedReader reader =
                    new BufferedReader(new InputStreamReader(file.getInputStream()));
                String s;
                while ((s = reader.readLine()) != null) {
                    if (s.length() == 0)
                        break;
                    sb.append(s);
                    sb.append("\r\n");
                }
                reader.close();
            }
            catch (Exception e) {
                Log.error(e);
            }
            Headers headers = Headers.parse(sb.toString());
            String charset = null;
            String contentType = headers.getValue(Headers.CONTENT_TYPE);
            if (contentType != null)
                charset = Utilities.getCharsetFromContentType(contentType);
            String encoding = Utilities.getEncodingFromCharset(charset);
            sb.setLength(0);
            try {
                BufferedReader reader =
                    new BufferedReader(new InputStreamReader(file.getInputStream(), encoding));
                String s;
                while ((s = reader.readLine()) != null) {
                    sb.append(s);
                    sb.append("\r\n");
                }
                reader.close();
            }
            catch (Exception e) {
                Log.error(e);
            }
            if (sb.length() > 0)
                return sb.toString();
            return null;
        }
        catch (OutOfMemoryError e) {
            Log.debug(e);
            return null;
        }
    }

    public void removeDeletedEntries(List mailboxEntries)
    {
        Log.debug("ImapMessageCache.removeDeletedEntries");
        long start = System.currentTimeMillis();
        Iterator it = mailboxEntries.iterator();
        while (it.hasNext()) {
            ImapMailboxEntry entry = (ImapMailboxEntry) it.next();
            if (entry.isDeleted()) {
                File file = File.getInstance(cacheDirectory,
                    String.valueOf(entry.getUid()));
                if (file != null && file.isFile()) {
                    Log.debug("deleting " + file.netPath());
                    file.delete();
                }
            }
        }
        long elapsed = System.currentTimeMillis() - start;
        Log.debug("ImapMessageCache.removeDeletedEntries " + elapsed + " ms");
    }

    private static synchronized File getCacheDirectory(ImapMailbox mb)
    {
        final File parentDirectory =
            File.getInstance(Directories.getMailDirectory(), "imap/cache");
        if (!parentDirectory.isDirectory()) {
            parentDirectory.mkdirs();
            if (!parentDirectory.isDirectory()) {
                Log.error("can't create IMAP cache directory");
                return null;
            }
        }
        File catalogFile = File.getInstance(parentDirectory, "catalog");
        boolean modified = false;
        if (catalog == null) {
            // Load the catalog.
            catalog = new Properties();
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
            // Update obsolete catalog entries.
            Properties temp = new Properties();
            Enumeration keys = catalog.keys();
            while (keys.hasMoreElements()) {
                String key = (String) keys.nextElement();
                String value = (String) catalog.get(key);
                if (key.indexOf('@') < 0 || key.indexOf(':') < 0) {
                    // Obsolete format (i.e. not canonical mailbox name).
                    try {
                        ImapURL url = new ImapURL(key);
                        temp.put(url.getCanonicalName(), value);
                    }
                    catch (MalformedURLException e) {
                        Log.error(e);
                        Log.debug("deleting cached messages for " + key);
                        File dir =
                            File.getInstance(parentDirectory, value);
                        if (dir != null && dir.isDirectory()) {
                            File[] files = dir.listFiles();
                            if (files != null) {
                                for (int i = files.length-1; i >= 0; i--) {
                                    Log.debug("deleting " + files[i]);
                                    files[i].delete();
                                }
                            }
                            Log.debug("removing directory " + dir);
                            dir.delete();
                        }
                    }
                    modified = true;
                } else
                    temp.put(key, value);
            }
            if (modified)
                catalog = temp;
        }
        File cacheDirectory = null;
        final String mailboxName = mb.getUrl().getCanonicalName();
        final String directoryName = catalog.getProperty(mailboxName);
        if (directoryName == null) {
            // Not found.
            cacheDirectory = Utilities.getTempFile(parentDirectory);
            if (cacheDirectory != null) {
                boolean directoryExists = cacheDirectory.isDirectory();
                if (!directoryExists) {
                    cacheDirectory.mkdirs();
                    directoryExists = cacheDirectory.isDirectory();
                }
                if (directoryExists) {
                    // Cache directory exists.
                    catalog.put(mailboxName, cacheDirectory.getName());
                    modified = true;
                } else
                    cacheDirectory = null;
            }
            if (cacheDirectory == null)
                Log.error("can't create IMAP cache directory");
        } else
            cacheDirectory = File.getInstance(parentDirectory, directoryName);
        if (modified) {
            Log.debug("saving modified catalog");
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
        return cacheDirectory;
    }
}
