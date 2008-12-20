/*
 * Drafts.java
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

import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStreamReader;
import java.util.ArrayList;
import java.util.Date;
import java.util.Iterator;
import java.util.List;
import javax.swing.SwingUtilities;
import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Headers;
import org.armedbear.j.Line;
import org.armedbear.j.Log;
import org.armedbear.j.View;

public final class Drafts extends Mailbox
{
    private final File directory;

    public Drafts(LocalMailboxURL url)
    {
        super();
        this.url = url;
        this.directory = url.getFile();
        supportsUndo = false;
        type = TYPE_MAILBOX;
        mode = MailboxMode.getMode();
        formatter = mode.getFormatter(this);
        readOnly = true;
        title = "drafts";
        setInitialized(true);
    }

    public File getDirectory()
    {
        return directory;
    }

    public String getName()
    {
        return directory.canonicalPath();
    }

    public int getMessageCount()
    {
        return 0;
    }

    public void getNewMessages()
    {
        reload();
    }

    public void createFolder()
    {
        notImplemented("Drafts.createFolder");
    }

    public void deleteFolder()
    {
        notImplemented("Drafts.deleteFolder");
    }

    public void saveToFolder()
    {
        notImplemented("Drafts.saveToFolder");
    }

    public void moveToFolder()
    {
        notImplemented("Drafts.moveToFolder");
    }

    public void delete()
    {
        final Editor editor = Editor.currentEditor();
        boolean advanceDot = false;
        List list = getTaggedEntries();
        if (list == null) {
            Line line = editor.getDotLine();
            if (!(line instanceof MailboxLine))
                return;
            advanceDot = true;
            list = new ArrayList();
            list.add(((MailboxLine)line).getMailboxEntry());
        }
        Iterator iter = list.iterator();
        while (iter.hasNext()) {
            DraftsEntry entry = (DraftsEntry) iter.next();
            if (entry == null)
                continue;
            File file = entry.getFile();
            if (file == null)
                continue;
            if (!file.isFile())
                continue;
            String name = file.getName();
            if (name.endsWith(".deleted"))
                continue; // Already deleted.
            File deleted =
                File.getInstance(directory, name.concat(".deleted"));
            if (deleted.isFile()) {
                Debug.bug();
                return;
            }
            Log.debug("delete renaming " + file.getName() + " to " +
                deleted.getName());
            if (file.renameTo(deleted)) {
                entry.setFile(deleted);
                entry.setFlags(entry.getFlags() | MailboxEntry.DELETED);
                updateEntry(entry);
            }
        }
        if (advanceDot)
            advanceDot(editor.getDotLine());
    }

    public void undelete()
    {
        final Editor editor = Editor.currentEditor();
        boolean advanceDot = false;
        List list = getTaggedEntries();
        if (list == null) {
            Line line = editor.getDotLine();
            if (!(line instanceof MailboxLine))
                return;
            advanceDot = true;
            list = new ArrayList();
            list.add(((MailboxLine)line).getMailboxEntry());
        }
        Iterator iter = list.iterator();
        while (iter.hasNext()) {
            DraftsEntry entry = (DraftsEntry) iter.next();
            if (entry == null)
                continue;
            File file = entry.getFile();
            if (file == null)
                continue;
            if (!file.isFile())
                continue;
            String name = file.getName();
            if (!name.endsWith(".deleted"))
                continue; // Not deleted.
            File undeleted = File.getInstance(directory,
                name.substring(0, name.length()-8));
            if (undeleted.isFile()) {
                Debug.bug();
                return;
            }
            Log.debug("undelete renaming " + file.getName() + " to " +
                undeleted.getName());
            if (file.renameTo(undeleted)) {
                entry.setFile(undeleted);
                entry.setFlags(entry.getFlags() & ~MailboxEntry.DELETED);
                updateEntry(entry);
            }
        }
        if (advanceDot)
            advanceDot(editor.getDotLine());
    }

    public void markRead()
    {
        notImplemented("Drafts.markRead");
    }

    public void markUnread()
    {
        notImplemented("Drafts.markUnread");
    }

    public void flag()
    {
        notImplemented("Drafts.flag");
    }

    public void setAnsweredFlag(MailboxEntry entry)
    {
        notImplemented("Drafts.setAnsweredFlag");
    }

    public void expunge()
    {
        if (lock()) {
            setBusy(true);
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this)
                    ed.saveView();
            }
            new Thread(expungeRunnable).start();
        }
    }

    private Runnable expungeRunnable = new Runnable() {
        public void run()
        {
            try {
                if (expungeInternal())
                    loadInternal();
            }
            finally {
                setBusy(false);
                unlock();
                updateDisplay();
            }
        }
    };

    // Returns true if at least one message is expunged.
    private boolean expungeInternal()
    {
        boolean result = false;
        entries = new ArrayList();
        String[] names = directory.list();
        if (names != null) {
            for (int i = 0; i < names.length; i++) {
                final String name = names[i];
                if (name.indexOf(".deleted") >= 0) {
                    File file = File.getInstance(directory, name);
                    Log.debug("deleting " + file);
                    file.delete();
                    result = true;
                }
            }
        }
        return result;
    }

    public int load()
    {
        if (lock()) {
            setBusy(true);
            setLoaded(true);
            new Thread(loadRunnable).start();
            return LOAD_PENDING;
        } else
            return LOAD_FAILED;
    }

    public void reload()
    {
        if (lock()) {
            setBusy(true);
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this)
                    ed.saveView();
            }
            new Thread(reloadRunnable).start();
        }
    }

    private Runnable loadRunnable = new Runnable() {
        public void run()
        {
            try {
                loadInternal();
            }
            finally {
                unlock();
                setBusy(false);
                Runnable completionRunnable = new Runnable() {
                    public void run()
                    {
                        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                            Editor ed = it.nextEditor();
                            View view = new View();
                            view.setDotEntry(getInitialEntry());
                            ed.setView(Drafts.this, view);
                            if (ed.getBuffer() == Drafts.this) {
                                ed.bufferActivated(true);
                                ed.updateDisplay();
                            }
                        }
                    }
                };
                SwingUtilities.invokeLater(completionRunnable);
            }
        }
    };

    private Runnable reloadRunnable = new Runnable() {
        public void run()
        {
            try {
                loadInternal();
                updateDisplay();
            }
            finally {
                unlock();
                setBusy(false);
            }
        }
    };

    private void loadInternal()
    {
        entries = new ArrayList();
        String[] names = directory.list();
        if (names != null) {
            for (int i = 0; i < names.length; i++) {
                final String name = names[i];
                DraftsEntry entry =
                    DraftsEntry.parseEntry(directory, name);
                if (entry != null) {
                    if (name.endsWith(".deleted"))
                        entry.setFlags(entry.getFlags() | MailboxEntry.DELETED);
                    entries.add(entry);
                }
            }
        }
        refreshBuffer();
    }

    public MailboxEntry getInitialEntry()
    {
        Line line = getFirstLine();
        if (line == null)
            return null;
        while (true) {
            if (line.next() == null)
                break; // Reached last line.
            line = line.next();
        }
        return ((MailboxLine)line).getMailboxEntry();
    }

    public void readMessage(Line line)
    {
        readMessage(line, false);
    }

    public void readMessageOtherWindow(Line line)
    {
        readMessage(line, true);
    }

    // useOtherWindow is ignored. Since it's a mail composition buffer, it's
    // opened full-height.
    private void readMessage(Line line, boolean useOtherWindow)
    {
        DraftsEntry entry = (DraftsEntry) ((MailboxLine)line).getMailboxEntry();
        if (entry == null)
            return;
        File file = entry.getFile();
        if (file == null)
            return;
        Buffer buf = null;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer b = it.nextBuffer();
            if (b instanceof SendMail) {
                if (file.equals(b.getFile())) {
                    buf = b;
                    break;
                }
            }
        }
        if (buf == null)
            buf = new SendMail(file);
        Editor editor = Editor.currentEditor();
        editor.makeNext(buf);
        editor.switchToBuffer(buf);
    }

    public File getCurrentDirectory()
    {
        return directory;
    }

    public String toString()
    {
        return "drafts";
    }

    private static class DraftsEntry extends MailboxEntry
    {
        private File file;

        DraftsEntry(File file)
        {
            this.file = file;
        }

        public final File getFile()
        {
            return file;
        }

        public final void setFile(File file)
        {
            this.file = file;
        }

        public static DraftsEntry parseEntry(File directory, String fileName)
        {
            File file = File.getInstance(directory, fileName);
            if (file != null && file.isFile()) {
                FastStringBuffer sb = new FastStringBuffer();
                try {
                    BufferedReader reader =
                        new BufferedReader(new InputStreamReader(file.getInputStream()));
                    while (true) {
                        String s = reader.readLine();
                        if (s == null)
                            break;
                        if (s.length() == 0)
                            break;
                        char c = s.charAt(0);
                        if (c == '-')
                            break; // Header separator line.
                        if (c != ' ' && c != '\t' && s.indexOf(':') < 0)
                            break; // Not a header line.
                        sb.append(s);
                        sb.append("\r\n");
                    }
                    reader.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
                Headers headers = Headers.parse(sb.toString());
                if (headers == null)
                    return null;
                DraftsEntry entry = new DraftsEntry(file);
                entry.subject =
                    RFC2047.decode(headers.getValue(Headers.SUBJECT));
                String dateString = headers.getValue(Headers.DATE);
                if (dateString != null)
                    entry.date = RFC822Date.parseDate(dateString);
                else
                    entry.date = new RFC822Date(new Date(file.lastModified()));
                entry.size = (int) file.length();
                String fromString = RFC2047.decode(headers.getValue(Headers.FROM));
                if (fromString != null)
                    entry.from = MailAddress.parseAddresses(fromString);
                else {
                    MailAddress ma = Mail.getUserMailAddress();
                    if (ma != null)
                        entry.from = new MailAddress[]{ma};
                }
                entry.replyTo = MailAddress.parseAddresses(RFC2047.decode(
                    headers.getValue(Headers.REPLY_TO)));
                entry.to = MailAddress.parseAddresses(RFC2047.decode(
                    headers.getValue(Headers.TO)));
                entry.cc = MailAddress.parseAddresses(RFC2047.decode(
                    headers.getValue(Headers.CC)));
                entry.messageId = headers.getValue(Headers.MESSAGE_ID);
                entry.inReplyTo =
                    parseInReplyTo(headers.getValue(Headers.IN_REPLY_TO));
                String refs = headers.getValue(Headers.REFERENCES);
                if (refs != null)
                    entry.references = parseReferences(refs);
                entry.flags = SEEN;
                return entry;
            }
            return null;
        }
    }
}
