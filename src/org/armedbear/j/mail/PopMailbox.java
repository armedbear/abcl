/*
 * PopMailbox.java
 *
 * Copyright (C) 2000-2003 Peter Graves
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
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.RandomAccessFile;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.HashSet;
import java.util.Iterator;
import java.util.List;
import java.util.Locale;
import java.util.Properties;
import javax.swing.SwingUtilities;
import org.armedbear.j.BackgroundProcess;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.Directories;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Log;
import org.armedbear.j.PasswordDialog;
import org.armedbear.j.Property;
import org.armedbear.j.StatusBarProgressNotifier;
import org.armedbear.j.Utilities;
import org.armedbear.j.View;

public final class PopMailbox extends LocalMailbox
{
    private final PopSession session;

    private File localStore;
    private StatusBarProgressNotifier progressNotifier;
    private boolean cancelled;
    private Thread backgroundThread;

    public PopMailbox(PopURL url, PopSession session)
    {
        super(url);
        this.session = session;
        if (url.getUser() == null)
            url.setUser(session.getUser());
        setMailboxFile(getLocalStore());
        setInitialized(true);
    }

    public String getFileNameForDisplay()
    {
        FastStringBuffer sb = new FastStringBuffer(64);
        sb.append(url.toString());
        String limitPattern = getLimitPattern();
        if (limitPattern != null) {
            sb.append(' ');
            sb.append(limitPattern);
        }
        return sb.toString();
    }

    public final String getName()
    {
        return url.toString();
    }

    public synchronized int load()
    {
        if (isLoaded())
            return LOAD_COMPLETED;
        if (lock()) {
            Debug.assertTrue(backgroundThread == null);
            backgroundThread = new Thread(loadProcess);
            backgroundThread.start();
            setLoaded(true);
            return LOAD_PENDING;
        }
        // Not loaded, lock() failed. Shouldn't happen.
        Debug.bug("PopMailbox.load can't lock mailbox");
        return LOAD_FAILED;
    }

    private BackgroundProcess loadProcess = new BackgroundProcess() {
        public void run()
        {
            // Mailbox is already locked at this point.
            boolean abort = false;
            try {
                setBusy(true);
                cancelled = false;
                progressNotifier = new StatusBarProgressNotifier(PopMailbox.this);
                progressNotifier.progressStart();
                setBackgroundProcess(this);
                readMailboxFile(progressNotifier); // Error handling?
                if (cancelled) {
                    abort = true;
                    return;
                }
                readExpungedUidlsList();
                clearRecent();
                if (cancelled || getBooleanProperty(Property.OFFLINE))
                    return;
                if (retrieveNewMessages()) {
                    addEntriesToAddressBook(entries);
                    setLastCheckMillis(System.currentTimeMillis());
                } else if (!cancelled) {
                    // Error!
                    error(session.getErrorText(), url.toString());
                }
            }
            finally {
                if (abort) {
                    Runnable r = new Runnable() {
                        public void run()
                        {
                            kill();
                            for (EditorIterator it = new EditorIterator(); it.hasNext();)
                                it.nextEditor().updateDisplay();
                        }
                    };
                    SwingUtilities.invokeLater(r);
                } else {
                    refreshBuffer();
                    Runnable r = new Runnable() {
                        public void run()
                        {
                            setBusy(false);
                            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                                Editor ed = it.nextEditor();
                                View view = new View();
                                view.setDotEntry(getInitialEntry());
                                ed.setView(PopMailbox.this, view);
                                if (ed.getBuffer() == PopMailbox.this) {
                                    ed.bufferActivated(true);
                                    ed.updateDisplay();
                                }
                            }
                        }
                    };
                    SwingUtilities.invokeLater(r);
                }
                setBackgroundProcess(null);
                backgroundThread = null;
                if (progressNotifier != null) {
                    progressNotifier.setText(cancelled ? "Cancelled" : "");
                    progressNotifier.progressStop();
                    progressNotifier = null;
                }
                unlock();
            }
        }

        public void cancel()
        {
            Log.debug("loadProcess.cancel");
            cancelled = true;
            progressNotifier.cancel();
            progressNotifier.setText("Cancelled, cleaning up...");
            if (backgroundThread != null && backgroundThread.isAlive())
                backgroundThread.interrupt();
            session.disconnect();
        }
    };

    public void getNewMessages()
    {
        if (session.getPassword() == null) {
            String password =
                PasswordDialog.showPasswordDialog(Editor.currentEditor(),
                    "Password:", "Password");
            if (password == null || password.length() == 0)
                return;
            session.setPassword(password);
        }
        if (lock())
            getNewMessages(true);
        else
            Editor.currentEditor().status("Mailbox is locked");
    }

    public void getNewMessages(boolean userInitiated)
    {
        Debug.assertTrue(isLocked());
        // This method can get called in the background so we can't put up a
        // dialog.
        if (session.getPassword() == null)
            return;
        Log.debug("PopMailbox.getNewMessages " + userInitiated);
        setBusy(true);
        Log.debug("PopMailbox.getNewMessages back from setBusy(true)");
        if (userInitiated)
            saveDisplayState();
        Debug.assertTrue(backgroundThread == null);
        backgroundThread = new Thread(new GetNewMessagesProcess(userInitiated));
        backgroundThread.start();
    }

    private class GetNewMessagesProcess implements BackgroundProcess
    {
        private boolean userInitiated;

        // If this constructor is private, we run into jikes 1.15 bug #2256.
        /*private*/ GetNewMessagesProcess(boolean userInitiated)
        {
            this.userInitiated = userInitiated;
        }

        public void run()
        {
            try {
                boolean changed = false;
                if (userInitiated)
                    changed = clearRecent();
                cancelled = false;
                progressNotifier = new StatusBarProgressNotifier(PopMailbox.this);
                progressNotifier.progressStart();
                setBackgroundProcess(this);
                int oldSize = entries.size();
                boolean ok = retrieveNewMessages();
                if (changed || entries.size() != oldSize) {
                    refreshBuffer();
                    addEntriesToAddressBook(entries);
                    updateDisplay();
                }
                newMessagesStatus();
                if (!ok) {
                    if (userInitiated && !cancelled)
                        error(session.getErrorText(), url.toString());
                }
            }
            finally {
                setBusy(false);
                if (progressNotifier != null) {
                    progressNotifier.setText(cancelled ? "Cancelled" : "");
                    progressNotifier.progressStop();
                    progressNotifier = null;
                }
                setBackgroundProcess(null);
                backgroundThread = null;
                setLastCheckMillis(System.currentTimeMillis());
                unlock();
                Editor.updateDisplayLater(PopMailbox.this);
            }
        }

        public void cancel()
        {
            Log.debug("GetNewMessagesProcess.cancel");
            cancelled = true;
            progressNotifier.cancel();
            progressNotifier.setText("Cancelled, cleaning up...");
            if (backgroundThread != null && backgroundThread.isAlive()) {
                Log.debug("interrupting background thread...");
                backgroundThread.interrupt();
            }
            session.disconnect();
        }
    }

    private boolean retrieveNewMessages()
    {
        Log.debug("PopMailbox.retrieveNewMessages");
        if (!connect())
            return false;
        File outputFile = null;
        long start = System.currentTimeMillis();
        try {
            if (Thread.currentThread().isInterrupted() || cancelled)
                return true;
            int count = stat();
            if (count < 0)
                return false; // Error.
            if (count == 0)
                return true; // No messages on the server.
            if (Thread.currentThread().isInterrupted() || cancelled)
                return true;
            List serverMessageList = getServerMessageList(count);
            if (serverMessageList == null)
                return false; // Error.
            if (Thread.currentThread().isInterrupted() || cancelled)
                return true;
            // Removed uidls from the expunged uidls list if the corresponding
            // message no longer exists on the server.
            pruneExpungedUidlsList(serverMessageList);
            if (Thread.currentThread().isInterrupted() || cancelled)
                return true;
            // Remove messages from the server message list if we already have
            // them or if they've been expunged locally.
            List messagesToBeRetrieved =
                getMessagesToBeRetrieved(serverMessageList);
            if (Thread.currentThread().isInterrupted() || cancelled)
                return true;
            // Is there anything left?
            if (messagesToBeRetrieved.size() == 0) {
                // If we're not keeping messages on the server, delete the old
                // messages.
                if (!getBooleanProperty(Property.POP_KEEP_MESSAGES_ON_SERVER))
                    deleteMessagesOnServer(serverMessageList);
                Log.debug("no new messages");
                return true;
            }
            if (getLocalStore() == null)
                return false; // Error.
            if (localStore.isFile() && localStore.length() > 0) {
                outputFile = Utilities.getTempFile(localStore.getParentFile());
                Log.debug("calling copyFile");
                long copyStart = System.currentTimeMillis();
                if (!Utilities.copyFile(localStore, outputFile))
                    return false;
                Log.debug("back from copyFile " + (System.currentTimeMillis() - copyStart) + " ms");
            } else
                outputFile = localStore;
            MailboxFileWriter writer =
                MailboxFileWriter.getInstance(outputFile, true); // Append.
            if (writer == null)
                return false; // Error.
            boolean ok = retrieveMessages(messagesToBeRetrieved, writer);
            try {
                writer.flush();
                writer.close();
            }
            catch (IOException e) {
                Log.error(e);
                return false;
            }
            if (!ok) {
                Log.debug("not ok...");
                // retrieveMessages() was interrupted. Truncate output file to
                // proper length.
                if (entries.size() == 0)
                    return false;
                LocalMailboxEntry entry =
                    (LocalMailboxEntry) entries.get(entries.size() - 1);
                long offset = entry.getNextMessageStart(); // Truncate to here.
                try {
                    RandomAccessFile raf =
                        outputFile.getRandomAccessFile("rw");
                    Log.debug("before raf.length() = " + raf.length());
                    Log.debug("truncating to " + offset);
                    raf.setLength(offset);
                    Log.debug("after raf.length() = " + raf.length());
                    raf.close();
                }
                catch (IOException e) {
                    Log.error(e);
                    return false;
                }
            }
            if (outputFile != localStore)
                if (!Utilities.deleteRename(outputFile, localStore))
                    return false;
            if (getBooleanProperty(Property.POP_KEEP_MESSAGES_ON_SERVER) == false)
                deleteMessagesOnServer(serverMessageList);
            outputFile = null;
        }
        finally {
            Log.debug("retrieveNewMessages calling logout() ...");
            session.logout();
            if (outputFile != null && outputFile.isFile())
                outputFile.delete();
        }
        Log.debug("retrieveNewMessages " + (System.currentTimeMillis() - start) + " ms");
        return true; // Success!
    }

    private boolean connect()
    {
        if (progressNotifier != null)
            progressNotifier.setText("Connecting to " + session.getHost());
        if (session.connect()) {
            if (progressNotifier != null)
                progressNotifier.setText("Connected to " + session.getHost());
            return true;
        }
        return false;
    }

    // Returns number of messages on server or -1 if there is an error.
    private int stat()
    {
        int count = -1;
        session.write("stat");
        String response = session.readLine();
        if (response.startsWith("+OK")) {
            try {
                count = Utilities.parseInt(response.substring(3).trim());
            }
            catch (NumberFormatException e) {
                Log.error(e);
            }
        } else {
            Log.error("stat failed");
            Log.error(response);
        }
        return count;
    }

    private List getServerMessageList(int count)
    {
        long start = System.currentTimeMillis();
        session.write("uidl");
        String response = session.readLine();
        if (!response.startsWith("+OK")) {
            Log.error("getServerMessageList uidl failed");
            Log.error(response);
            return null;
        }
        List list = new ArrayList(count);
        while (true) {
            String s = session.readLine();
            if (s == null)
                return null; // Shouldn't happen.
            if (s.equals("."))
                break;
            int index = s.indexOf(' ');
            if (index >= 0) {
                int messageNumber = Integer.parseInt(s.substring(0, index)); // BUG! Error handling!
                String uidl = s.substring(index + 1);
                if (messageNumber >= 1)
                    list.add(new MessageListEntry(messageNumber, uidl));
            }
        }
        Log.debug("getServerMessageList " + (System.currentTimeMillis() - start) + " ms");
        Log.debug("getServerMessageList count = " + count + " list size = " + list.size());
        return list;
    }

    private List getMessagesToBeRetrieved(List serverMessageList)
    {
        long start = System.currentTimeMillis();
        HashSet hashSet = null;
        if (entries != null) {
            int size = entries.size();
            hashSet = new HashSet(size);
            for (int i = 0; i < size; i++) {
                LocalMailboxEntry mailboxEntry =
                    (LocalMailboxEntry) entries.get(i);
                hashSet.add(mailboxEntry.getUidl());
            }
        }
        List toBeReturned = new ArrayList();
        int size = serverMessageList.size();
        for (int i = 0; i < size; i++) {
            MessageListEntry messageListEntry =
                (MessageListEntry) serverMessageList.get(i);
            String uidl = messageListEntry.uidl;
            if (isExpunged(uidl))
                continue;
            if (hashSet != null && hashSet.contains(uidl))
                continue;
            toBeReturned.add(messageListEntry);
        }
        Log.debug("getMessagesToBeRetrieved " + (System.currentTimeMillis() - start) + " ms");
        return toBeReturned;
    }

    private boolean retrieveMessages(List messageList, MailboxFileWriter writer)
    {
        Log.debug("entering retrieveMessages");
        long start = System.currentTimeMillis();
        for (int i = 0; i < messageList.size(); i++) {
            String text = "Retrieving message " + (i + 1)  + " of " + messageList.size();
            if (i == 0)
                progressNotifier.setText(text); // Make sure the user sees this.
            else
                progressNotifier.progress(text);
            MessageListEntry entry = (MessageListEntry) messageList.get(i);
            if (!retrieveMessage(entry.messageNumber, entry.uidl, writer)) {
                Log.error("retrieveMessages error retrieving message " + entry.messageNumber);
                return false;
            }
        }
        progressNotifier.setText(cancelled ? "Cancelled, cleaning up" : "");
        Log.debug("leaving retrieveMessages " +(System.currentTimeMillis() - start) + " ms");
        return true;
    }

    // Returns true if no error.
    private boolean retrieveMessage(int i, String uidl, MailboxFileWriter writer)
    {
        String command = "list " + i;
        session.write(command);
        int size = 0;
        String response = session.readLine();
        String expected = "+OK " + i + " ";
        if (response != null && response.startsWith(expected)) {
            try {
                size = Integer.parseInt(response.substring(expected.length()));
            }
            catch (NumberFormatException e) {
                Log.error(e);
                return false; // Error!
            }
        } else {
            Log.error("retrieveMessage command failed: " + command);
            Log.error("response = " + response);
        }
        command = "retr " + i;
        session.write(command);
        response = session.readLine();
        if (response == null || !response.startsWith("+OK")) {
            Log.error("retrieveMessage command failed: " + command);
            Log.error(response);
            return false; // Error!
        }
        try {
            final long messageStart = writer.getOffset();
            writer.write("From - ");
            writer.write(getDateTimeStamp());
            writer.write('\n');
            // Headers.
            FastStringBuffer sb = new FastStringBuffer(2048);
            while (true) {
                String s = session.readLine();
                if (s == null)
                    return false; // Error! (Reached end of stream before reaching end of headers.)
                if (s.length() == 0) {
                    // Reached end of headers.
                    // Add X-J-Status.
                    String status = "X-J-Status: 0\n";
                    writer.write(status);
                    sb.append(status);
                    // Add X-UIDL.
                    if (uidl != null) {
                        writer.write("X-UIDL: ");
                        writer.write(uidl);
                        writer.write('\n');
                        sb.append("X-UIDL: ");
                        sb.append(uidl);
                        sb.append('\n');
                    }
                    writer.write('\n');
                    break;
                }
                if (s.toUpperCase().startsWith("X-UIDL"))
                    continue;
                writer.write(s);
                writer.write('\n');
                sb.append(s);
                sb.append('\n');
            }
            // Body.
            boolean echo = session.getEcho();
            session.setEcho(false);
            while (true) {
                String s = session.readLine();
                if (s == null)
                    return false; // Error! (Reached end of stream before reaching end of message.)
                if (s.equals("."))
                    break; // End of message.
                if (s.length() > 1 && s.charAt(0) == '.' && s.charAt(1) == '.') {
                    // Remove dot-stuffing.
                    s = s.substring(1);
                } else if (s.startsWith("From ")) {
                    // Mangle lines starting with "From " in body of message.
                    writer.write('>');
                }
                writer.write(s);
                writer.write('\n');
            }
            session.setEcho(echo);
            // Add a newline after the end of the message.
            writer.write('\n');
            LocalMailboxEntry entry = new LocalMailboxEntry(entries.size()+1,
                messageStart, sb.toString());
            entry.setNextMessageStart(writer.getOffset());
            entry.setSize(size);
            entry.setFlags(MailboxEntry.RECENT);
            entries.add(entry);
            setDirty(true);
            return true; // No error.
        }
        catch (IOException e) {
            Log.error(e);
            return false; // Error!
        }
    }

    private boolean deleteMessagesOnServer(List serverMessageList)
    {
        Log.debug("deleteMessagesOnServer need to delete " + serverMessageList.size() + " messages");
        for (int i = 0; i < serverMessageList.size(); i++) {
            MessageListEntry messageListEntry =
                (MessageListEntry) serverMessageList.get(i);
            session.write("dele " + messageListEntry.messageNumber);
            String response = session.readLine();
            if (response == null || !response.startsWith("+OK")) {
                Log.error("deleteMessagesOnServer dele failed response = " + response);
                session.write("rset");
                session.readLine();
                return false; // Error!
            }
        }
        Log.debug("deleteMessagesOnServer success!");
        return true;
    }

    public void expunge()
    {
        if (lock()) {
            setBusy(true);
            saveDisplayState();
            Debug.assertTrue(backgroundThread == null);
            backgroundThread = new Thread(expungeProcess);
            backgroundThread.start();
        }
    }

    private Runnable expungeProcess = new BackgroundProcess() {
        public void run()
        {
            try {
                setBackgroundProcess(this);
                progressNotifier = new StatusBarProgressNotifier(PopMailbox.this);
                progressNotifier.progressStart();
                cancelled = false;
                expungeInternal();
            }
            finally {
                setBackgroundProcess(null);
                backgroundThread = null;
                setBusy(false);
                if (progressNotifier != null) {
                    if (cancelled)
                        progressNotifier.setText("Cancelled");
                    progressNotifier.progressStop();
                    progressNotifier = null;
                }
                unlock();
                updateDisplay();
            }
        }

        public void cancel()
        {
            Log.debug("expungeProcess.cancel");
            cancelled = true;
            if (backgroundThread != null && backgroundThread.isAlive())
                backgroundThread.interrupt();
            session.disconnect();
        }
    };

    private void expungeInternal()
    {
        if (entries == null)
            return; // No error.
        if (getBooleanProperty(Property.POP_EXPUNGE_DELETED_MESSAGES_ON_SERVER) == false ||
            getBooleanProperty(Property.POP_KEEP_MESSAGES_ON_SERVER) == false) {
            // This is the "local expunge only" case.
            Log.debug("expungeInternal \"local expunge only\" case");
            // First add all deleted entries to expunged list.
            for (int i = entries.size() - 1; i >= 0; i--) {
                MailboxEntry entry = (MailboxEntry) entries.get(i);
                if (entry.isDeleted())
                    addToExpungedUidlsList(entry.getUidl());
            }
            writeExpungedUidlsList(); // BUG! Error handling?
            rewriteMailbox(true); // BUG! Error handling!
            refreshBuffer();
            return;
        }
        // Reaching here, we want to expunge the deleted messages on the server too.
        Log.debug("expungeInternal \"expunge through\" case");
        if (!connect()) {
            Log.error("expungeInternal can't connect");
            return;
        }
        try {
            int count = stat();
            if (count < 0)
                return; // Error.
            if (count > 0) {
                List serverMessageList = getServerMessageList(count);
                for (int i = 0; i < entries.size(); i++) {
                    MailboxEntry entry = (MailboxEntry) entries.get(i);
                    if (entry.isDeleted()) {
                        String uidl = entry.getUidl();
                        if (!expungeUidl(uidl, serverMessageList))
                            return; // Error!
                        if (cancelled) {
                            progressNotifier.setText("Cancelled");
                            return;
                        }
                    }
                }
                if (expungedUidlsList != null) {
                    // Expunge on the server messages that were previously expunged locally.
                    Iterator it = expungedUidlsList.iterator();
                    while (it.hasNext()) {
                        String uidl = (String) it.next();
                        if (!expungeUidl(uidl, serverMessageList))
                            return; // Error!
                        if (cancelled) {
                            progressNotifier.setText("Cancelled");
                            return;
                        }
                    }
                }
            }
            progressNotifier.progress("Logging out...");
            if (!session.logout())
                return; // Error!
            // All deletions have been completed on the server.
            progressNotifier.progress("Saving mailbox...");
            rewriteMailbox(true); // BUG! Error handling!
            progressNotifier.setText("Saving mailbox...done");
            refreshBuffer();
        }
        finally {
            session.disconnect();
        }
    }

    private boolean expungeUidl(String uidl, List serverMessageList)
    {
        if (uidl != null) {
            for (int j = serverMessageList.size() - 1; j >= 0; j--) {
                MessageListEntry messageListEntry = (MessageListEntry) serverMessageList.get(j);
                if (uidl.equals(messageListEntry.uidl)) {
                    if (progressNotifier != null) {
                        FastStringBuffer sb = new FastStringBuffer("Deleting message ");
                        sb.append(messageListEntry.messageNumber);
                        sb.append(" on server");
                        progressNotifier.progress(sb.toString());
                    }
                    session.write("dele " + messageListEntry.messageNumber);
                    String response = session.readLine();
                    if (response != null && response.startsWith("+OK"))
                        return true;
                    // Error!
                    Log.error("expungeUidl dele failed response = " + response);
                    session.write("rset");
                    session.readLine();
                    return false;
                }
            }
        }
        // Didn't find uidl.
        return true;
    }

    private HashSet expungedUidlsList;

    private final boolean isExpunged(String uidl)
    {
        if (expungedUidlsList == null)
            return false;
        return expungedUidlsList.contains(uidl);
    }

    private final void addToExpungedUidlsList(String uidl)
    {
        if (expungedUidlsList == null)
            expungedUidlsList = new HashSet();
        expungedUidlsList.add(uidl);
    }

    // Prune our list of expunged uidls, removing entries that no longer exist
    // on the server.
    private void pruneExpungedUidlsList(List serverMessageList)
    {
        Log.debug("pruneExpungedUidlsList");
        if (expungedUidlsList == null)
            return; // Nothing to do.
        boolean changed = false;
        long start = System.currentTimeMillis();
        int size = serverMessageList.size();
        HashSet serverUidls = new HashSet(size);
        for (int i = 0; i < size; i++) {
            MessageListEntry entry = (MessageListEntry) serverMessageList.get(i);
            serverUidls.add(entry.uidl);
        }
        Iterator it = expungedUidlsList.iterator();
        while (it.hasNext()) {
            String uidl = (String) it.next();
            if (!serverUidls.contains(uidl)) {
                Log.warn("removing uidl " + uidl + " (no longer exists on server)");
                it.remove();
                changed = true;
            }
        }
        if (changed)
            writeExpungedUidlsList();
        Log.debug("pruneExpungedUidlsList " + (System.currentTimeMillis() - start) + " ms");
    }

    private void readExpungedUidlsList()
    {
        File mailboxFile = getMailboxFile();
        if (mailboxFile == null) {
            Debug.bug("readExpungedUidlsList mailboxFile is null");
            return;
        }
        String filename = mailboxFile.canonicalPath() + ".expunged";
        try {
            BufferedReader reader = new BufferedReader(new FileReader(filename));
            String s;
            while ((s = reader.readLine()) != null) {
                if (expungedUidlsList == null)
                    expungedUidlsList = new HashSet();
                expungedUidlsList.add(s);
            }
            reader.close();
        }
        catch (FileNotFoundException e) {
            // Might happen.
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private void writeExpungedUidlsList()
    {
        File mailboxFile = getMailboxFile();
        if (mailboxFile == null) {
            Debug.bug("writeExpungedUidls mailboxFile is null");
            return;
        }
        String filename = mailboxFile.canonicalPath() + ".expunged";
        if (expungedUidlsList == null || expungedUidlsList.size() == 0) {
            File file = File.getInstance(filename);
            if (file.isFile())
                file.delete();
            return;
        }
        try {
            BufferedWriter writer = new BufferedWriter(new FileWriter(filename));
            Iterator it = expungedUidlsList.iterator();
            while (it.hasNext()) {
                writer.write((String) it.next());
                writer.newLine();
            }
            writer.flush();
            writer.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private static final SimpleDateFormat df =
        new SimpleDateFormat("EEE MMM dd HH:mm:ss yyyy", Locale.US);

    private final String getDateTimeStamp()
    {
        return df.format(Calendar.getInstance().getTime());
    }

    private File getLocalStore()
    {
        if (localStore != null)
            return localStore;
        File popDir = File.getInstance(Directories.getMailDirectory(), "pop");
        if (!popDir.isDirectory()) {
            popDir.mkdirs();
            if (!popDir.isDirectory()) {
                Log.error("can't make directory " + popDir.canonicalPath());
                return null;
            }
        }
        File catalogFile = File.getInstance(popDir, "catalog");
        Properties catalog = new Properties();
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
        String mailboxName = getUrl().toString();
        if (mailboxName.startsWith("pop://"))
            mailboxName = mailboxName.substring(6);
        String filename = catalog.getProperty(mailboxName);
        if (filename != null) // Found existing store.
            return localStore = File.getInstance(popDir, filename);
        File file = Utilities.getTempFile(popDir);
        catalog.put(mailboxName, file.getName());
        try {
            OutputStream out = catalogFile.getOutputStream();
            catalog.save(out, null);
            out.flush();
            out.close();
            return localStore = file;
        }
        catch (IOException e) {
            Log.error(e);
            return null;
        }
    }

    public void dispose()
    {
        Log.debug("PopMailbox.dispose");
        Runnable disposeRunnable = new Runnable() {
            public void run()
            {
                try {
                    Log.debug("disposeRunnable.run() calling acquire()...");
                    acquire(); // Blocks, may throw InterruptedException.
                    Log.debug("disposeRunnable.run() back from acquire()");
                    if (dirty) {
                        final Object pending = new Object();
                        Editor.getPendingOperations().add(pending);
                        Log.debug("disposeRunnable.run() calling rewriteMailbox()...");
                        rewriteMailbox(false);
                        Log.debug("disposeRunnable.run() back from rewriteMailbox()");
                        Editor.getPendingOperations().remove(pending);
                    }
                    Debug.assertTrue(session != null);
                    Log.debug("disposeRunnable.run() calling session.logout()...");
                    session.logout();
                    release();
                    Log.debug("disposeRunnable.run() back from release()");
                }
                catch (InterruptedException e) {
                    Log.error(e);
                }
            }
        };
        new Thread(disposeRunnable).start();
        MailboxProperties.saveProperties(this);
    }

    protected void finalize() throws Throwable
    {
        Log.debug("PopMailbox.finalize");
        super.finalize();
    }

    public String toString()
    {
        int newMessageCount = getNewMessageCount();
        if (newMessageCount > 0) {
            FastStringBuffer sb = new FastStringBuffer(url.toString());
            sb.append(" (");
            sb.append(newMessageCount);
            sb.append(" new)");
            return sb.toString();
        } else
            return url.toString();
    }

    public String getTitle()
    {
        return toString();
    }
}

class MessageListEntry
{
    int messageNumber;
    String uidl;

    MessageListEntry(int messageNumber, String uidl)
    {
        this.messageNumber = messageNumber;
        this.uidl = uidl;
    }
}
