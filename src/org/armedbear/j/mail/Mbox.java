/*
 * Mbox.java
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

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileWriter;
import java.io.IOException;
import java.io.StringReader;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.Iterator;
import java.util.List;
import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;
import org.armedbear.j.Mutex;
import org.armedbear.j.ProgressNotifier;

public final class Mbox
{
    private static ArrayList mboxList;

    private final Mutex mutex = new Mutex();
    private final File file;

    private long lastModified;
    private ArrayList entries;

    private Mbox(File file)
    {
        this.file = file;
        Debug.assertTrue(file != null);
    }

    public static synchronized Mbox getInstance(File file)
    {
        if (mboxList == null)
            mboxList = new ArrayList();
        else {
            for (int i = mboxList.size()-1; i >= 0; i--) {
                Mbox mbox = (Mbox) mboxList.get(i);
                if (mbox.getFile().equals(file))
                    return mbox;
            }
        }
        // Not found.
        Mbox mbox = new Mbox(file);
        mboxList.add(mbox);
        return mbox;
    }

    public static synchronized void cleanup()
    {
        Log.debug("Mbox.cleanup");
        if (mboxList == null || mboxList.size() == 0)
            return;
        Iterator iter = mboxList.iterator();
        while (iter.hasNext()) {
            Mbox mbox = (Mbox) iter.next();
            if (findMailbox(mbox) == null) {
                Log.debug("removing Mbox for " + mbox.getFile());
                iter.remove();
            }
        }
    }

    private static Mailbox findMailbox(Mbox mbox)
    {
        File file = mbox.getFile();
        BufferIterator iter = new BufferIterator();
        while (iter.hasNext()) {
            Buffer buf = iter.nextBuffer();
            if (buf instanceof LocalMailbox) {
                LocalMailbox mb = (LocalMailbox) buf;
                if (mb.getMailboxFile().equals(file))
                    return mb;
            }
        }
        return null;
    }

    public final File getFile()
    {
        return file;
    }

    // Return a copy.
    public synchronized final List getEntries(ProgressNotifier progressNotifier)
    {
        Log.debug("Mbox.getEntries");
        Debug.assertTrue(isLocked());
        if (entries != null) {
            if (file.lastModified() > lastModified) {
                Log.debug("mbox last modified later than entries last modified");
                entries = null;
            }
        }
        if (entries == null) {
            File summaryFile = getSummaryFile();
            if (summaryFile.isFile() && summaryFile.lastModified() > file.lastModified()) {
                Log.debug("using summary");
                MboxSummary summary = MboxSummary.read(summaryFile);
                if (summary != null) {
                    Log.debug("summary is valid");
                    if (summary.lastModified() == file.lastModified()) {
                        if (summary.length() == file.length()) {
                            entries = summary.getEntries();
                            lastModified = file.lastModified();
                        }
                    }
                }
            }
            if (entries == null) {
                Log.debug("entries == null, calling read...");
                read(progressNotifier);
            }
        }
        return new ArrayList(entries);
    }

    public synchronized boolean lock()
    {
        Log.debug("Mbox.lock " + file.canonicalPath());
        try {
            return mutex.attempt();
        }
        catch (InterruptedException e) {
            return false;
        }
    }

    public synchronized void unlock()
    {
        Log.debug("Mbox.unlock " + file.canonicalPath());
        mutex.release();
    }

    public synchronized boolean isLocked()
    {
        return mutex.isInUse();
    }

    private synchronized void read(ProgressNotifier progressNotifier)
    {
        Log.debug("entering Mbox.read");
        long start = System.currentTimeMillis();
        Debug.assertTrue(isLocked());
        entries = new ArrayList(1000);
        long messageStart = 0;
        MailReader reader = null;
        try {
            if (!file.isFile())
                return;
            reader = new MailReader(file.getInputStream());
            FastStringBuffer sb = new FastStringBuffer(1024);
            boolean complete = false;
            while (true) {
                long here = reader.getOffset();
                String text = reader.readLine();
                if (text == null) {
                    // End of file.
                    Log.debug("read - end of file");
                    if (entries.size() > 0) {
                        LocalMailboxEntry entry =
                            (LocalMailboxEntry) entries.get(entries.size()-1);
                        entry.setSize((int)(here - messageStart));
                        entry.setNextMessageStart(here);
                    }
                    complete = true;
                    break;
                }
                if (text.startsWith("From ")) {
                    if (entries.size() > 0) {
                        LocalMailboxEntry entry =
                            (LocalMailboxEntry) entries.get(entries.size()-1);
                        entry.setSize((int)(here - messageStart));
                        entry.setNextMessageStart(here);
                        messageStart = here;
                    }
                    if (progressNotifier != null && progressNotifier.cancelled()) {
                        Log.debug("Mbox.read cancelled!");
                        break;
                    }
                    sb.setLength(0);
                    while (true) {
                        text = reader.readLine();
                        if (text == null)
                            return; // Shouldn't happen.
                        if (text.length() == 0)
                            break; // End of header.
                        sb.append(text);
                        sb.append('\n');
                    }
                    LocalMailboxEntry entry =
                        new LocalMailboxEntry(entries.size()+1, here, sb.toString());
                    entries.add(entry);
                    if (progressNotifier != null) {
                        sb.setLength(0);
                        sb.append("Read ");
                        sb.append(entries.size());
                        sb.append(" message");
                        if (entries.size() > 1)
                            sb.append('s');
                        progressNotifier.progress(sb.toString());
                    }
                }
            }
            if (complete) {
                long elapsed = System.currentTimeMillis() - start;
                Log.debug("Mbox.read " + elapsed + " ms");
                // User did not cancel.
                writeSummary();
                Log.debug("Mbox.read - after writeSummary");
                lastModified = file.lastModified();
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        finally {
            if (reader != null) {
                try {
                    reader.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
            Log.debug("leaving Mbox.read");
        }
    }

    public synchronized boolean appendMessage(Message message, final int flags)
    {
        Log.debug("Mbox.appendMessage flags = " + flags);
        Debug.assertTrue(isLocked());
        try {
            BufferedReader reader =
                new BufferedReader(new StringReader(message.getRawText()));
            BufferedWriter writer =
                new BufferedWriter(new FileWriter(file.canonicalPath(), true));
            final long messageStart = file.length();
            writer.write("From - ");
            SimpleDateFormat dateFormatter =
                new SimpleDateFormat ("EEE MMM d HH:mm:ss yyyy");
            Calendar cal = Calendar.getInstance();
            String dateString = dateFormatter.format(cal.getTime());
            writer.write(dateString);
            writer.write('\n');
            // Headers.
            FastStringBuffer sb = new FastStringBuffer(2048);
            while (true) {
                String s = reader.readLine();
                if (s == null)
                    return false; // Error! (Reached end of stream before reaching end of headers.)
                if (s.length() == 0) {
                    // Reached end of headers.
                    // Add X-J-Status.
                    String status = "X-J-Status: " + flags + "\n";
                    writer.write(status);
                    sb.append(status);
                    writer.write('\n');
                    break;
                }
                // Skip X-UIDL.
                if (s.toUpperCase().startsWith("X-UIDL"))
                    continue;
                // Skip X-J-Status.
                if (s.startsWith("X-J-Status:"))
                    continue;
                writer.write(s);
                writer.write('\n');
                sb.append(s);
                sb.append('\n');
            }
            // Body.
            while (true) {
                String s = reader.readLine();
                if (s == null)
                    break;
                if (s.startsWith("From ")) {
                    // Mangle lines starting with "From " in body of message.
                    writer.write('>');
                }
                writer.write(s);
                writer.write('\n');
            }
            // Add a newline after the end of the message.
            writer.write('\n');
            writer.flush();
            writer.close();
            reader.close();
            if (entries != null) {
                final long nextMessageStart = file.length();
                LocalMailboxEntry entry =
                    new LocalMailboxEntry(entries.size()+1, messageStart,
                        sb.toString());
                entry.setNextMessageStart(nextMessageStart);
                entry.setSize((int)(nextMessageStart - messageStart));
                entries.add(entry);
            } else
                Log.debug("appendMessage entries == null");
            return true;
        }
        catch (IOException e) {
            Log.error(e);
            return false;
        }
    }

    public synchronized void updateViews()
    {
        if (entries == null)
            return;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf instanceof LocalMailbox) {
                LocalMailbox mb = (LocalMailbox) buf;
                if (mb.getMailboxFile().equals(file)) {
                    if (mb.lock()){
                        try {
                            mb.saveDisplayState();
                            mb.setEntries(new ArrayList(entries));
                            mb.refreshBuffer();
                            mb.updateDisplay();
                        }
                        finally {
                            mb.unlock();
                        }
                    }
                }
            }
        }
    }

    private final File getSummaryFile()
    {
        return File.getInstance(file.canonicalPath() + ".summary");
    }

    // Called only from read().
    private void writeSummary()
    {
        Debug.assertTrue(isLocked());
        File summaryFile = getSummaryFile();
        MboxSummary summary = new MboxSummary(file, entries);
        summary.write(summaryFile);
    }

    protected void finalize() throws Throwable
    {
        Log.debug("Mbox.finalize " + file);
        super.finalize();
    }
}
