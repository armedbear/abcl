/*
 * ImapMailbox.java
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

import java.io.UnsupportedEncodingException;
import java.net.MalformedURLException;
import java.util.ArrayList;
import java.util.Collections;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import javax.swing.SwingUtilities;
import org.armedbear.j.BackgroundProcess;
import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Frame;
import org.armedbear.j.Headers;
import org.armedbear.j.InputDialog;
import org.armedbear.j.Line;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.ProgressNotifier;
import org.armedbear.j.Property;
import org.armedbear.j.Sidebar;
import org.armedbear.j.StatusBar;
import org.armedbear.j.StatusBarProgressNotifier;
import org.armedbear.j.Utilities;
import org.armedbear.j.View;

public final class ImapMailbox extends Mailbox
{
    private static final int DEFAULT_PORT = 143;

    private final ImapSession session;
    private final String folderName;

    private int messageCount = -1;
    private int uidValidity;
    private int uidLast;
    private ImapMailboxCache mailboxCache;
    private boolean cancelled;
    private Thread backgroundThread;

    public ImapMailbox(ImapURL url, ImapSession session)
    {
        super(url);
        this.session = session;
        session.setMailbox(this);
        String tunnel = getStringProperty(Property.TUNNEL);
        if (tunnel != null) {
            Log.debug("tunnel = |" + tunnel + "|");
            session.setTunnel(tunnel);
        }
        folderName = session.getFolderName();
        Debug.assertTrue(folderName.equals(url.getFolderName()));
        supportsUndo = false;
        type = TYPE_MAILBOX;
        mode = MailboxMode.getMode();
        formatter = mode.getFormatter(this);
        readOnly = true;
        setInitialized(true);
        Log.debug("ImapMailbox constructor ".concat(url.getCanonicalName()));
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

    public String getName()
    {
        FastStringBuffer sb = new FastStringBuffer();
        sb.append('{');
        sb.append(session.getUser());
        sb.append('@');
        sb.append(session.getHost());
        sb.append(':');
        sb.append(session.getPort());
        sb.append('}');
        sb.append(folderName);
        return sb.toString();
    }

    public final ImapSession getSession()
    {
        return session;
    }

    public final String getFolderName()
    {
        return folderName;
    }

    public final int getUidValidity()
    {
        return uidValidity;
    }

    public final int getMessageCount()
    {
        return messageCount;
    }

    public synchronized long getLastErrorMillis()
    {
        return session.getLastErrorMillis();
    }

    public void setAlertText(final String s)
    {
        Log.debug("alert = " + s);
        Runnable r = new Runnable() {
            public void run()
            {
                Editor.currentEditor().status(s);
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void setStatusText(final String s)
    {
        Log.debug("status = " + s);
        Runnable r = new Runnable() {
            public void run()
            {
                Editor.currentEditor().status(s);
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public void messageExpunged(int messageNumber)
    {
        // Invalidate message count.
        messageCount = -1;
    }

    public void expunge()
    {
        Runnable expungeRunnable = new Runnable() {
            public void run()
            {
                try {
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        if (session.isReadOnly()) {
                            Log.debug("expunge - read-only - reselecting...");
                            session.reselect(folderName);
                            if (session.isReadOnly()) {
                                Log.error("expunge - mailbox is read-only");
                                readOnlyError();
                                return;
                            }
                        }
                        if (messageCache != null)
                            messageCache.removeDeletedEntries(Collections.unmodifiableList(entries));
                        if (session.close()) {
                            Log.debug("expunge back from close(), calling reselect()");
                            if (session.reselect(folderName)) {
                                Log.debug("expunge back from reselect()");
                                getAllMessageHeaders();
                                refreshBuffer();
                                setBusy(false);
                                for (int i = 0; i < Editor.getFrameCount(); i++) {
                                    Frame frame = Editor.getFrame(i);
                                    if (frame != null) {
                                        StatusBar statusBar = frame.getStatusBar();
                                        if (statusBar != null)
                                            statusBar.setText(null);
                                    }
                                }
                                updateDisplay();
                                return;
                            }
                        }
                        // Error!
                        error("Operation failed", "Expunge");
                    }
                }
                finally {
                    setBusy(false);
                    unlock();
                }
            }
        };
        if (lock()) {
            setBusy(true);
            saveDisplayState();
            new Thread(expungeRunnable).start();
        }
    }

    public synchronized int load()
    {
        if (isLoaded()) {
            return LOAD_COMPLETED;
        } else if (lock()) {
            Debug.assertTrue(backgroundThread == null);
            backgroundThread = new Thread(loadProcess);
            backgroundThread.start();
            setLoaded(true);
            return LOAD_PENDING;
        } else {
            // Not loaded, lock() failed. Shouldn't happen.
            Debug.bug("ImapMailbox.load can't lock mailbox");
            return LOAD_FAILED;
        }
    }

    private BackgroundProcess loadProcess = new BackgroundProcess() {
        public void run()
        {
            Runnable completionRunnable = null;
            try {
                cancelled = false;
                setBusy(true);
                setBackgroundProcess(this);
                if (getAllMessageHeaders()) {
                    refreshBuffer();
                    completionRunnable = new Runnable() {
                        public void run()
                        {
                            setBusy(false);
                            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                                Editor ed = it.nextEditor();
                                View view = new View();
                                view.setDotEntry(getInitialEntry());
                                ed.setView(ImapMailbox.this, view);
                                if (ed.getBuffer() == ImapMailbox.this) {
                                    ed.bufferActivated(true);
                                    ed.updateDisplay();
                                }
                            }
                        }
                    };
                } else {
                    // Error or user cancelled.
                    completionRunnable = new Runnable() {
                        public void run()
                        {
                            if (Editor.getBufferList().contains(ImapMailbox.this))
                                kill();
                            for (EditorIterator it = new EditorIterator(); it.hasNext();)
                                it.nextEditor().updateDisplay();
                        }
                    };
                }
            }
            finally {
                setBackgroundProcess(null);
                backgroundThread = null;
                unlock();
                if (completionRunnable != null)
                    SwingUtilities.invokeLater(completionRunnable);
            }
        }

        public void cancel()
        {
            abort();
        }
    };

    private void abort()
    {
        Log.debug("ImapMailbox.abort");
        cancelled = true;
        if (backgroundThread != null && backgroundThread.isAlive())
            backgroundThread.interrupt();
        session.disconnect();
    }

    public final void getNewMessages()
    {
        if (lock())
            getNewMessages(true);
        else
            Editor.currentEditor().status("Mailbox is locked");
    }

    public void getNewMessages(boolean interactive)
    {
        Debug.assertTrue(isLocked());
        setBusy(true);
        if (interactive)
            saveDisplayState();
        Debug.assertTrue(backgroundThread == null);
        backgroundThread = new Thread(new GetNewMessagesProcess(interactive));
        backgroundThread.start();
    }

    private class GetNewMessagesProcess implements BackgroundProcess
    {
        private final boolean interactive;

        public GetNewMessagesProcess(boolean interactive)
        {
            this.interactive = interactive;
        }

        public void run()
        {
            cancelled = false;
            setBackgroundProcess(this);
            boolean changed = false;
            if (interactive)
                changed = clearRecent();
            try {
                // verifyConnected() sends a NOOP command to the server, which
                // gives the server a chance to report changes to the mailbox.
                if (!session.verifyConnected()) {
                    Log.error("GetNewMessagesProcess.run can't connect");
                    return;
                }
                if (cancelled) {
                    Log.debug("cancelled, disconnecting...");
                    session.disconnect();
                    return;
                }
                if (!session.verifySelected(folderName)) {
                    Log.error("GetNewMessagesProcess.run can't select " +
                        folderName);
                    return;
                }
                if (cancelled) {
                    Log.debug("cancelled, disconnecting...");
                    session.disconnect();
                    return;
                }
                if (session.isReadOnly()) {
                    Log.warn("GetNewMessagesProcess.run " + folderName +
                        " is read-only, returning...");
                    return;
                }
                if (uidValidity != session.getUidValidity()) {
                    getAllMessageHeaders();
                    changed = true;
                } else if (interactive) {
                    changed = getNewMessageHeaders() || changed;
                } else {
                    // Not interactive.
                    if (session.getMessageCount() != messageCount) {
                        Log.debug("session.getMessageCount() = " +
                            session.getMessageCount() +
                            " mailbox message count = " +
                            messageCount);
                        changed = getNewMessageHeaders();
                    }
                }
                if (cancelled) {
                    Log.debug("cancelled, disconnecting...");
                    session.disconnect();
                }
            }
            finally {
                setBackgroundProcess(null);
                backgroundThread = null;
                if (changed) {
                    refreshBuffer();
                    setBusy(false);
                    updateDisplay();
                    newMessagesStatus();
                } else {
                    setBusy(false);
                    for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                        Editor ed = it.nextEditor();
                        if (ed != null && ed.getBuffer() == ImapMailbox.this)
                            ed.setDefaultCursor();
                    }
                }
                setLastCheckMillis(System.currentTimeMillis());
                unlock();
            }
        }

        public void cancel()
        {
            abort();
        }
    }

    public void createFolder()
    {
        final Editor editor = Editor.currentEditor();
        final String input =
            InputDialog.showInputDialog(editor, "Folder:", "Create Folder");
        if (input == null || input.length() == 0)
            return;
        final String name = extractFolderName(input);
        if (name == null)
            return;
        Runnable createRunnable = new Runnable() {
            public void run()
            {
                boolean succeeded = false;
                try {
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        session.setEcho(true);
                        session.writeTagged("create " + name);
                        succeeded = session.getResponse() == ImapSession.OK;
                        session.setEcho(false);
                    }
                }
                finally {
                    unlock();
                    setBusy(false);
                    Editor.updateDisplayLater(ImapMailbox.this);
                    if (succeeded)
                        success("Folder created");
                    else
                        error("Unable to create folder", "Error");
                }
            }
        };
        // Even though we're not changing this mailbox per se, we need to lock
        // it here so we have exclusive use of the session.
        if (lock()) {
            setBusy(true);
            new Thread(createRunnable).start();
        }
    }

    public void deleteFolder()
    {
        final Editor editor = Editor.currentEditor();
        final String input =
            InputDialog.showInputDialog(editor, "Folder:", "Delete Folder");
        if (input == null || input.length() == 0)
            return;
        final String name = extractFolderName(input);
        if (name == null)
            return;
        String message = "Delete folder \"" + name + "\" on " + session.getHost() + "?";
        if (!editor.confirm("Delete Folder", message))
            return;
        Runnable deleteFolderRunnable = new Runnable() {
            public void run()
            {
                boolean succeeded = false;
                try {
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        session.setEcho(true);
                        session.writeTagged("delete " + name);
                        succeeded = session.getResponse() == ImapSession.OK;
                        session.setEcho(false);
                    }
                }
                finally {
                    unlock();
                    setBusy(false);
                    Editor.updateDisplayLater(ImapMailbox.this);
                    if (succeeded)
                        success("Folder deleted");
                    else
                        error("Unable to delete folder", "Error");
                }
            }
        };
        // Even though we're not changing this mailbox per se, we need to lock
        // it here so we have exclusive use of the session.
        if (lock()) {
            setBusy(true);
            new Thread(deleteFolderRunnable).start();
        }
    }

    public void saveToFolder()
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
        final List toBeCopied = list;
        final int count = toBeCopied.size();
        String title;
        if (count > 1)
            title = "Save Tagged Messages to Folder";
        else
            title = "Save Message to Folder";
        FastStringBuffer sb = new FastStringBuffer("Save ");
        sb.append(count);
        sb.append(" message");
        if (count > 1)
            sb.append('s');
        sb.append(" to:");
        final String destination = ChooseFolderDialog.chooseFolder(editor,
            sb.toString(), title);
        if (destination == null)
            return;
        final Line dotLine = advanceDot ? editor.getDotLine() : null;
        Runnable saveRunnable = new Runnable() {
            public void run()
            {
                boolean succeeded = false;
                try {
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        if (destination.startsWith("mailbox:")) {
                            // Destination is local.
                            succeeded = saveLocal(toBeCopied, destination, false);
                        } else {
                            session.setEcho(true);
                            final String messageSet = getMessageSet(toBeCopied);
                            FastStringBuffer sbuf = new FastStringBuffer("uid copy ");
                            sbuf.append(messageSet);
                            sbuf.append(' ');
                            sbuf.append(destination);
                            if (session.writeTagged(sbuf.toString()))
                                if (session.getResponse() == ImapSession.OK)
                                    succeeded = true;
                            session.setEcho(false);
                        }
                    }
                }
                finally {
                    if (succeeded && dotLine != null)
                        advanceDot(dotLine);
                    setBusy(false);
                    unlock();
                    editor.updateDisplayLater();
                    if (succeeded) {
                        FastStringBuffer sbuf = new FastStringBuffer("Saved ");
                        sbuf.append(toBeCopied.size());
                        sbuf.append(" message");
                        if (toBeCopied.size() != 1)
                            sbuf.append('s');
                        success(sbuf.toString());
                    } else
                        error("Save failed", "Error");
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(saveRunnable).start();
        }
    }

    public void moveToFolder()
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
        final List toBeMoved = list;
        final int count = toBeMoved.size();
        String title;
        if (count > 1)
            title = "Move Tagged Messages to Folder";
        else
            title = "Move Message to Folder";
        FastStringBuffer sb = new FastStringBuffer("Move ");
        sb.append(count);
        sb.append(" message");
        if (count > 1)
            sb.append('s');
        sb.append(" to:");
        final String destination = ChooseFolderDialog.chooseFolder(editor,
            sb.toString(), title);
        if (destination == null)
            return;
        final Line dotLine = advanceDot ? editor.getDotLine() : null;
        Runnable moveRunnable = new Runnable() {
            public void run()
            {
                boolean succeeded = false;
                String errorText = "Move failed";
                try {
                    succeeded = moveToFolder(toBeMoved, destination);
                }
                catch (MailException e) {
                    errorText = e.getMessage();
                }
                finally {
                    // Update message count in sidebar buffer list. We should
                    // do this even if there was an error, since some messages
                    // may have been moved successfully.
                    countMessages();
                    Sidebar.repaintBufferListInAllFrames();
                    if (succeeded && dotLine != null)
                        advanceDot(dotLine);
                    setBusy(false);
                    unlock();
                    editor.updateDisplayLater();
                    if (succeeded) {
                        FastStringBuffer sbuf = new FastStringBuffer("Moved ");
                        sbuf.append(toBeMoved.size());
                        sbuf.append(" message");
                        if (toBeMoved.size() != 1)
                            sbuf.append('s');
                        success(sbuf.toString());
                    } else
                        error(errorText, "Error");
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(moveRunnable).start();
        }
    }

    private boolean moveToFolder(List toBeMoved, String destination) throws MailException
    {
        if (destination == null)
            return false;
        if (!destination.startsWith("mailbox:")) {
            // Not local. Extract folder name from URL.
            destination = extractFolderName(destination);
            if (destination == null)
                return false;
        }
        boolean succeeded = false;
        if (session.verifyConnected() && session.verifySelected(folderName)) {
            if (session.isReadOnly()) {
                Log.debug("moveToFolder " + folderName + " is read-only - reselecting...");
                session.reselect(folderName);
                if (session.isReadOnly())
                    throw new MailException("Mailbox " + folderName + " is read-only");
            }
            if (destination.startsWith("mailbox:")) {
                // Destination is local.
                succeeded = saveLocal(toBeMoved, destination, true);
            } else {
                // Destination is an IMAP folder.
                session.setEcho(true);
                final String messageSet = getMessageSet(toBeMoved);
                FastStringBuffer sb = new FastStringBuffer("uid copy ");
                sb.append(messageSet);
                sb.append(' ');
                sb.append(destination);
                session.writeTagged(sb.toString());
                if (session.getResponse() == ImapSession.OK) {
                    sb.setLength(0);
                    sb.append("uid store ");
                    sb.append(messageSet);
                    sb.append(" +flags.silent (\\deleted)");
                    session.writeTagged(sb.toString());
                    if (session.getResponse() == ImapSession.OK) {
                        succeeded = true;
                        for (int i = 0; i < toBeMoved.size(); i++) {
                            ImapMailboxEntry entry = (ImapMailboxEntry) toBeMoved.get(i);
                            entry.setFlags(entry.getFlags() | MailboxEntry.DELETED);
                            updateEntry(entry);
                        }
                    }
                }
                session.setEcho(false);
            }
        }
        return succeeded;
    }

    private boolean saveLocal(List toBeSaved, String destination, boolean delete)
    {
        File file = File.getInstance(destination.substring(8));
        if (file == null)
            return false;
        if (!file.isLocal())
            return false;
        Mbox mbox = Mbox.getInstance(file);
        if (!mbox.lock())
            return false;
        boolean error = false;
        for (int i = 0; i < toBeSaved.size(); i++) {
            ImapMailboxEntry entry = (ImapMailboxEntry) toBeSaved.get(i);
            Message message = getMessage(entry, null);
            if (message != null &&
                mbox.appendMessage(message, entry.getFlags() & ~MailboxEntry.TAGGED)) {
                if (delete) {
                    session.writeTagged("uid store " + entry.getUid() + " +flags.silent (\\deleted)");
                    if (session.getResponse() == ImapSession.OK) {
                        entry.setFlags(entry.getFlags() | MailboxEntry.DELETED);
                        updateEntry(entry);
                    } else {
                        error = true;
                        break;
                    }
                }
            } else {
                error = true;
                break;
            }
        }
        mbox.unlock();
        mbox.updateViews();
        return !error;
    }

    public String extractFolderName(String input)
    {
        if (input.startsWith("{")) {
            try {
                ImapURL targetUrl = new ImapURL(input);
                if (!url.getHost().equals(targetUrl.getHost())) {
                    // We don't support operations involving folders on other
                    // servers.
                    return null;
                }
                String name = targetUrl.getFolderName();
                Log.debug("folder name = |" + name + "|");
                return name;
            }
            catch (MalformedURLException e) {
                Log.error(e);
                return null;
            }
        } else {
            // Assume input is already a relative path.
            return input;
        }
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
        final List toBeDeleted = list;
        final Line dotLine = advanceDot ? editor.getDotLine() : null;
        Runnable deleteRunnable = new Runnable() {
            public void run()
            {
                boolean succeeded = false;
                String errorText = "Delete failed";
                try {
                    succeeded = delete(toBeDeleted);
                }
                catch (MailException e) {
                    errorText = e.getMessage();
                }
                finally {
                    if (succeeded) {
                        // Update message count in sidebar buffer list.
                        countMessages();
                        Sidebar.repaintBufferListInAllFrames();
                        if (dotLine != null)
                            advanceDot(dotLine);
                    }
                    setBusy(false);
                    unlock();
                    editor.updateDisplayLater();
                    if (!succeeded)
                        error(errorText, "Error");
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(deleteRunnable).start();
        }
    }

    private boolean delete(List toBeDeleted) throws MailException
    {
        boolean succeeded = false;
        if (session.verifyConnected() && session.verifySelected(folderName)) {
            if (session.isReadOnly()) {
                Log.debug("delete " + folderName + " is read-only - reselecting...");
                session.reselect(folderName);
                if (session.isReadOnly())
                    throw new MailException("Mailbox " + folderName + " is read-only");
            }
            session.setEcho(true);
            session.uidStore(getMessageSet(toBeDeleted), "+flags.silent (\\deleted)");
            if (session.getResponse() == ImapSession.OK) {
                succeeded = true;
                for (int i = 0; i < toBeDeleted.size(); i++) {
                    ImapMailboxEntry entry = (ImapMailboxEntry) toBeDeleted.get(i);
                    entry.setFlags(entry.getFlags() | MailboxEntry.DELETED);
                    updateEntry(entry);
                }
            }
            session.setEcho(false);
        }
        return succeeded;
    }

    public void undelete()
    {
        storeFlagsInternal(ACTION_UNDELETE);
    }

    public void markRead()
    {
        storeFlagsInternal(ACTION_MARK_READ);
    }

    public void markUnread()
    {
        storeFlagsInternal(ACTION_MARK_UNREAD);
    }

    private static final int ACTION_UNDELETE    = 0;
    private static final int ACTION_MARK_UNREAD = 1;
    private static final int ACTION_MARK_READ   = 2;

    private void storeFlagsInternal(final int action)
    {
        final Editor editor = Editor.currentEditor();
        boolean advanceDot = false;
        List list = getTaggedEntries();
        if (list == null) {
            Line line = editor.getDotLine();
            if (!(line instanceof MailboxLine))
                return;
            if (action == ACTION_UNDELETE)
                advanceDot = getBooleanProperty(Property.UNDELETE_ADVANCE_DOT);
            else
                advanceDot = true;
            list = new ArrayList();
            list.add(((MailboxLine)line).getMailboxEntry());
        }
        final List entriesToBeProcessed = list;
        final Line dotLine = advanceDot ? editor.getDotLine() : null;
        Runnable storeFlagsRunnable = new Runnable() {
            public void run()
            {
                try {
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        if (session.isReadOnly()) {
                            Log.debug("storeFlagsInternal - read-only - reselecting...");
                            session.reselect(folderName);
                            if (session.isReadOnly()) {
                                readOnlyError();
                                return;
                            }
                        }
                        session.setEcho(true);
                        final String messageSet = getMessageSet(entriesToBeProcessed);
                        switch (action) {
                            case ACTION_UNDELETE:
                                session.uidStore(messageSet, "-flags.silent (\\deleted)");
                                break;
                            case ACTION_MARK_READ:
                                session.uidStore(messageSet, "+flags.silent (\\seen)");
                                break;
                            case ACTION_MARK_UNREAD:
                                session.uidStore(messageSet, "-flags.silent (\\seen)");
                                break;
                            default:
                                Debug.assertTrue(false);
                                break;
                        }
                        if (session.getResponse() == ImapSession.OK) {
                            for (int i = 0; i < entriesToBeProcessed.size(); i++) {
                                ImapMailboxEntry entry = (ImapMailboxEntry) entriesToBeProcessed.get(i);
                                switch (action) {
                                    case ACTION_UNDELETE:
                                        entry.setFlags(entry.getFlags() & ~MailboxEntry.DELETED);
                                        break;
                                    case ACTION_MARK_READ:
                                        entry.setFlags(entry.getFlags() | MailboxEntry.SEEN);
                                        break;
                                    case ACTION_MARK_UNREAD:
                                        entry.setFlags(entry.getFlags() & ~MailboxEntry.SEEN);
                                        break;
                                    default:
                                        Debug.assertTrue(false);
                                        break;
                                }
                                updateEntry(entry);
                            }
                            if (dotLine != null)
                                advanceDot(dotLine);
                        }
                        session.setEcho(false);
                    }
                    countMessages();
                }
                finally {
                    setBusy(false);
                    unlock();
                    editor.updateDisplayLater();
                    // Update message count in sidebar buffer list.
                    Sidebar.repaintBufferListInAllFrames();
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(storeFlagsRunnable).start();
        }
    }

    public void flag()
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
        final List entriesToBeSet = new ArrayList();
        final List entriesToBeCleared = new ArrayList();
        for (int i = 0; i < list.size(); i++) {
            MailboxEntry entry = (MailboxEntry) list.get(i);
            if (entry.isFlagged())
                entriesToBeCleared.add(entry);
            else
                entriesToBeSet.add(entry);
        }
        final Line dotLine = advanceDot ? editor.getDotLine() : null;
        Runnable flagRunnable = new Runnable() {
            public void run()
            {
                try {
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        if (session.isReadOnly()) {
                            Log.debug("storeFlagsInternal - read-only - reselecting...");
                            session.reselect(folderName);
                            if (session.isReadOnly()) {
                                readOnlyError();
                                return;
                            }
                        }
                        boolean error = false;
                        session.setEcho(true);
                        if (entriesToBeSet.size() > 0) {
                            session.uidStore(getMessageSet(entriesToBeSet), "+flags.silent (\\flagged)");
                            if (session.getResponse() == ImapSession.OK) {
                                for (int i = 0; i < entriesToBeSet.size(); i++) {
                                    MailboxEntry entry = (MailboxEntry) entriesToBeSet.get(i);
                                    entry.flag();
                                    updateEntry(entry);
                                }
                            } else
                                error = true;
                        }
                        if (!error && entriesToBeCleared.size() > 0) {
                            session.uidStore(getMessageSet(entriesToBeCleared), "-flags.silent (\\flagged)");
                            if (session.getResponse() == ImapSession.OK) {
                                for (int i = 0; i < entriesToBeCleared.size(); i++) {
                                    MailboxEntry entry = (MailboxEntry) entriesToBeCleared.get(i);
                                    entry.unflag();
                                    updateEntry(entry);
                                }
                            } else
                                error = true;
                        }
                        session.setEcho(false);
                        if (!error && dotLine != null)
                            advanceDot(dotLine);
                    }
                }
                finally {
                    setBusy(false);
                    unlock();
                    editor.updateDisplayLater();
                    // Update message count in sidebar buffer list.
                    Sidebar.repaintBufferListInAllFrames();
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(flagRunnable).start();
        }
    }

    public void setAnsweredFlag(final MailboxEntry entry)
    {
        Runnable setAnsweredFlagRunnable = new Runnable() {
            public void run()
            {
                try {
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        session.setEcho(true);
                        session.uidStore(((ImapMailboxEntry)entry).getUid(), "+flags (\\answered)");
                        if (session.getResponse() == ImapSession.OK) {
                            entry.setFlags(entry.getFlags() | MailboxEntry.ANSWERED);
                            updateEntry(entry);
                        }
                        session.setEcho(false);
                    }
                }
                finally {
                    setBusy(false);
                    unlock();
                    Editor.updateDisplayLater(ImapMailbox.this);
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(setAnsweredFlagRunnable).start();
        }
    }

    private List retrieveMessageHeaders(int uidBegin, int uidEnd,
        boolean recent)
    {
        Log.debug("retrieveMessageHeaders " + folderName + " " + uidBegin +
            " " + uidEnd);
        long start = System.currentTimeMillis();
        uidValidity = session.getUidValidity();
        messageCount = session.getMessageCount();
        ArrayList list = new ArrayList();
        FastStringBuffer sbCommand = new FastStringBuffer("uid fetch ");
        sbCommand.append(uidBegin);
        sbCommand.append(':');
        if (uidEnd < 0)
            sbCommand.append('*');
        else
            sbCommand.append(uidEnd);
        sbCommand.append(" (uid flags internaldate rfc822.size envelope");
        sbCommand.append(" body.peek[header.fields (references)]");
        sbCommand.append(')');
        Log.debug("command = |" + sbCommand.toString() + "|");
        session.writeTagged(sbCommand.toString());
        StatusBarProgressNotifier progressNotifier =
            new StatusBarProgressNotifier(this);
        progressNotifier.progressStart();
        try {
            FastStringBuffer sb = new FastStringBuffer();
            final String endPrefix = session.lastTag() + " ";
            while (true) {
                String s = session.readLine();
                if (s == null) {
                    Log.debug("retrieveMessageHeaders s == null");
                    break;
                }
                if (s.startsWith(endPrefix))
                    break;
                if (s.indexOf("ENVELOPE (") >= 0) {
                    // New entry starting.
                    if (sb.length() > 0) {
                        // Add previous entry to list.
                        addEntry(list, sb.toString(), uidBegin, recent);
                        progressNotifier.progress(getProgressText(list.size()));
                        if (cancelled) {
                            Log.debug("retrieveMessageHeaders cancelled, disconnecting...");
                            session.disconnect();
                            return list;
                        }
                        sb.setLength(0);
                    }
                    sb.append(s);
                } else {
                    // Continuation of previous entry.
                    sb.append("\r\n");
                    sb.append(s);
                }
            }
            if (sb.length() > 0) {
                // Add final entry to list.
                addEntry(list, sb.toString(), uidBegin, recent);
                progressNotifier.progress(getProgressText(list.size()));
            }
        }
        catch (Exception e) {
            Log.error(e);
        }
        finally {
            progressNotifier.progressStop();
            long elapsed = System.currentTimeMillis() - start;
            Log.debug("retrieveMessageHeaders " + list.size() + " messages in " + elapsed + " ms");
        }
        return list;
    }

    private void addEntry(List list, String s, int uidBegin, boolean recent)
    {
        ImapMailboxEntry entry = ImapMailboxEntry.parseEntry(this, s);
        if (entry != null) {
            if (entry.getUid() >= uidBegin) {
                if (recent)
                    entry.setFlags(entry.getFlags() | MailboxEntry.RECENT);
                list.add(entry);
            } else {
                Log.debug("not adding message " + entry.getMessageNumber() +
                    " uid " + entry.getUid() + " uidBegin = " + uidBegin);
                if (messageCount < 0) {
                    // Mailbox message count is invalid.
                    messageCount = entry.getMessageNumber();
                } else if (entry.getMessageNumber() != messageCount)
                    Debug.bug();
            }
        } else
            Log.error("can't parse envelope |" + s + "|");
    }

    // This method does not make any assumptions about the order of the entries.
    private void updateLastUid()
    {
        uidLast = 0;
        if (entries != null) {
            for (int i = entries.size()-1; i >= 0; i--) {
                ImapMailboxEntry entry = (ImapMailboxEntry) entries.get(i);
                if (entry.getUid() > uidLast)
                    uidLast = entry.getUid();
            }
        }
    }

    public void readOnlyError()
    {
        Runnable r = new Runnable() {
            public void run()
            {
                MessageDialog.showMessageDialog("Mailbox is read-only",
                    "Error");
            }
        };
        SwingUtilities.invokeLater(r);
    }

    private void fatal(final String text, final String title)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                MessageDialog.showMessageDialog(Editor.currentEditor(),
                    text, title);
                if (Editor.getBufferList().contains(ImapMailbox.this))
                    kill();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    private boolean getAllMessageHeaders()
    {
        Thread t = new Thread() {
            public void run()
            {
                mailboxCache = ImapMailboxCache.readCache(ImapMailbox.this);
            }
        };
        t.start();
        if (!session.verifyConnected()){
            Log.error("getAllMessageHeaders can't connect to " +
                session.getHost() + " on port " + session.getPort());
            setBusy(false);
            if (!cancelled)
                fatal(session.getErrorText(), "Error");
            return false;
        }
        if (!session.verifySelected(folderName)) {
            Log.error("getAllMessageHeaders can't SELECT " + folderName +
                " on " + session.getHost());
            setBusy(false);
            if (!cancelled)
                fatal(session.getErrorText(), "Error");
            return false;
        }
        if (t != null) {
            try {
                t.join();
            }
            catch (InterruptedException e) {
                Log.error(e);
            }
        }
        entries = null;
        uidLast = 0;
        if (mailboxCache != null && mailboxCache.isValid()) {
            Log.debug("mailboxCache is valid");
            List cachedEntries = mailboxCache.getEntries();
            Log.debug("cachedEntries.size() = " + cachedEntries.size());
            updateCachedEntries(cachedEntries);
            int size = cachedEntries.size();
            entries = new ArrayList(size);
            // Add entries from cache, skipping any that have been nulled out.
            for (int i = 0; i < size; i++) {
                Object o = cachedEntries.get(i);
                if (o != null)
                    entries.add(o);
            }
            Log.debug("entries.size() = " + entries.size());
            // We don't need the cache any more.
            mailboxCache = null;
            updateLastUid();
        }
        if (cancelled) {
            session.disconnect();
            return false;
        }
        // Get any new entries from the server. Set the recent flag on these
        // entries unless we're retrieving the whole mailbox (uidLast == 0).
        boolean recent = uidLast > 0;
        final List newEntries = retrieveMessageHeaders(uidLast+1, -1, recent);
        if (newEntries != null && newEntries.size() > 0) {
            addEntriesToAddressBook(newEntries);
            processIncomingFilters(newEntries);
            if (entries != null)
                entries.addAll(newEntries);
            else
                entries = new ArrayList(newEntries);
        }
        uidValidity = session.getUidValidity();
        if (entries == null)
            entries = new ArrayList();
        else if (entries instanceof ArrayList)
            ((ArrayList)entries).trimToSize();
        new ImapMailboxCache(this).writeCache();
        updateLastUid();
        return true;
    }

    private boolean getNewMessageHeaders()
    {
        List newEntries = retrieveMessageHeaders(uidLast+1, -1, true);
        if (newEntries == null || newEntries.size() == 0)
            return false;
        addEntriesToAddressBook(newEntries);
        processIncomingFilters(newEntries);
        entries.addAll(newEntries);
        if (entries instanceof ArrayList)
            ((ArrayList)entries).trimToSize();
        new ImapMailboxCache(this).writeCache();
        updateLastUid();
        return true;
    }

    private void processIncomingFilters(List entryList)
    {
        Log.debug("processIncomingFilters");
        final List filterList = IncomingFilter.getFilterList();
        if (filterList == null || filterList.size() == 0)
            return;
        // For now, we just process incoming filters for the user's inbox.
        String inbox = Editor.preferences().getStringProperty(Property.INBOX);
        if (inbox == null)
            return;
        MailboxURL inboxUrl = MailboxURL.parse(inbox);
        if (!(inboxUrl instanceof ImapURL)) {
            Log.debug("processIncomingFilters not inbox " + url.toString());
            return;
        }
        if (!url.getHost().equals(inboxUrl.getHost())) {
            Log.debug("processIncomingFilters not inbox " + url.toString());
            return;
        }
        if (!folderName.equals(((ImapURL)inboxUrl).getFolderName())) {
            Log.debug("processIncomingFilters not inbox " + url.toString());
            return;
        }
        SmtpSession smtp = null;
        for (int i = 0; i < entryList.size(); i++) {
            ImapMailboxEntry entry = (ImapMailboxEntry) entryList.get(i);
            // Don't process deleted entries.
            if (entry.isDeleted())
                continue;
            for (int j = 0; j < filterList.size(); j++) {
                IncomingFilter incomingFilter = (IncomingFilter) filterList.get(j);
                if (incomingFilter != null) {
                    MailboxFilter mf = incomingFilter.getFilter();
                    if (mf != null && mf.accept(entry)) {
                        switch (incomingFilter.getAction()) {
                            case IncomingFilter.MOVE: {
                                processMove(entry, incomingFilter.getParameter());
                                break;
                            }
                            case IncomingFilter.BOUNCE: {
                                boolean succeeded = false;
                                if (smtp == null)
                                    smtp = SmtpSession.getDefaultSession();
                                if (smtp != null)
                                    succeeded = processBounce(entry, incomingFilter.getParameter(), smtp);
                                if (!succeeded)
                                    Log.error("processBounce failed");
                                break;
                            }
                            case IncomingFilter.BOUNCE_AND_DELETE: {
                                boolean succeeded = false;
                                if (smtp == null)
                                    smtp = SmtpSession.getDefaultSession();
                                if (smtp != null)
                                    succeeded = processBounce(entry, incomingFilter.getParameter(), smtp);
                                if (succeeded)
                                    processDelete(entry);
                                else
                                    Log.error("processBounce failed");
                                break;
                            }
                            default:
                                break;
                        }
                        break;
                    }
                }
            }
        }
        // Processing completed.
        if (smtp != null)
            smtp.quit();
    }

    private void processMove(ImapMailboxEntry entry, String destination)
    {
        if (destination != null) {
            Log.debug("destination = |" + destination + "|");
            ArrayList list = new ArrayList(1);
            list.add(entry);
            try {
                Log.debug("processMove calling moveToFolder");
                moveToFolder(list, destination);
                Log.debug("processMove back from moveToFolder");
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    private boolean processBounce(ImapMailboxEntry entry, String bounceTo, SmtpSession smtp)
    {
        if (bounceTo == null)
            return false;
        Log.debug("bounceTo = |" + bounceTo + "|");
        MailAddress[] to = MailAddress.parseAddresses(bounceTo);
        if (to == null)
            return false;
        if (to.length == 0)
            return false;
        Message message = getMessage(entry, null);
        if (message == null) {
            Log.error("processBounce getMessage() failed");
            return false;
        }
        return Mail.bounceMessage(message, to, smtp);
    }

    private void processDelete(ImapMailboxEntry entry)
    {
        ArrayList list  = new ArrayList(1);
        list.add(entry);
        try {
            delete(list);
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    private void updateCachedEntries(List cachedEntries)
    {
        if (cachedEntries == null)
            return;
        final int size = cachedEntries.size();
        if (size == 0)
            return;
        long start = System.currentTimeMillis();
        session.writeTagged("uid fetch 1:* (uid flags)");
        HashMap map = new HashMap(size);
        Iterator iter = cachedEntries.iterator();
        while (iter.hasNext()) {
            ImapMailboxEntry entry = (ImapMailboxEntry) iter.next();
            map.put(new Integer(entry.getUid()), entry);
        }
        Log.debug("built map " + (System.currentTimeMillis() - start) + " ms");
        iter = null;
        final String endPrefix = session.lastTag() + " ";
        // Set mailbox field and update flags for all messages on server.
        while (true) {
            final String s = session.readLine();
            if (s == null) {
                Log.debug("updateCachedEntries s is null");
                break;
            }
            if (s.startsWith(endPrefix))
                break;
            int uid = ImapMailboxEntry.parseUid(s);
            if (uid == 0) {
                Log.debug("uid = 0 s = |" + s + "|");
                continue;
            }
            ImapMailboxEntry entry =
                (ImapMailboxEntry) map.get(new Integer(uid));
            if (entry != null) {
                Debug.assertTrue(entry.getMailbox() == null);
                entry.setMailbox(this);
                entry.setFlags(ImapMailboxEntry.parseFlags(s));
            }
        }
        map.clear();
        // Null out entries whose mailbox field is not set (i.e. entries
        // deleted from server).
        for (int i = 0; i < size; i++) {
            ImapMailboxEntry entry = (ImapMailboxEntry) cachedEntries.get(i);
            if (entry.getMailbox() == null)
                cachedEntries.set(i, null);
        }
        long elapsed = System.currentTimeMillis() - start;
        Log.debug("updateCachedEntries " + elapsed + " ms " +
            (float)elapsed/cachedEntries.size() + " ms per entry");
    }

    public void readMessage(Line line)
    {
        readMessage(line, false);
    }

    public void readMessageOtherWindow(Line line)
    {
        readMessage(line, true);
    }

    private void readMessage(Line line, boolean useOtherWindow)
    {
        Editor editor = Editor.currentEditor();
        ImapMailboxEntry entry =
            (ImapMailboxEntry) ((MailboxLine)line).getMailboxEntry();
        Buffer buf = null;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer b = it.nextBuffer();
            if (b instanceof ImapMessageBuffer) {
                ImapMessageBuffer mb = (ImapMessageBuffer) b;
                if (mb.getMailboxEntry() == entry) {
                    buf = b;
                    break;
                }
            }
        }
        if (buf != null) {
            // Found existing buffer.
            activateMessageBuffer(editor, (ImapMessageBuffer) buf,
                useOtherWindow);
            return;
        }
        if (getBooleanProperty(Property.IMAP_USE_LOCAL_CACHE)) {
            String rawText = getMessageTextFromCache(entry.getUid());
            if (rawText != null) {
                ImapMessageBuffer mb =
                    new ImapMessageBuffer(this, entry, rawText);
                activateMessageBuffer(editor, mb, useOtherWindow);
                if ((entry.getFlags() & MailboxEntry.SEEN) == 0) {
                    if (session.isReadOnly())
                        markReadLocal(entry);
                    else
                        markRead(entry);
                }
                return;
            }
        }
        // Lock the mailbox before creating the message buffer. It will be
        // unlocked at the end of the message buffer's loadProcess.run().
        if (lock()) {
            setBusy(true);
            ImapMessageBuffer mb = new ImapMessageBuffer(this, entry);
            activateMessageBuffer(editor, mb, useOtherWindow);
        } else
            editor.status("Mailbox is locked");
    }

    private void markRead(final ImapMailboxEntry entry)
    {
        Runnable markReadRunnable = new Runnable() {
            public void run()
            {
                try {
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        session.uidStore(entry.getUid(), "+flags.silent (\\seen)");
                        if (session.getResponse() == ImapSession.OK)
                            markReadLocal(entry);
                    }
                }
                finally {
                    setBusy(false);
                    unlock();
                    Editor.updateDisplayLater(ImapMailbox.this);
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(markReadRunnable).start();
        }
    }

    private void markReadLocal(ImapMailboxEntry entry)
    {
        entry.setFlags(entry.getFlags() | MailboxEntry.SEEN);
        updateEntry(entry);
        countMessages();
    }

    public ImapMailboxEntry getMailboxEntry(String messageId)
    {
        for (int i = entries.size()-1; i >= 0; i--) {
            ImapMailboxEntry entry = (ImapMailboxEntry) entries.get(i);
            if (messageId.equals(entry.getMessageId()))
                return entry;
        }
        return null;
    }

    public Message getMessage(MailboxEntry entry,
        ProgressNotifier progressNotifier)
    {
        if (!(entry instanceof ImapMailboxEntry))
            return null;
        final int uid = ((ImapMailboxEntry)entry).getUid();
        if (getBooleanProperty(Property.IMAP_USE_LOCAL_CACHE)) {
            String rawText = getMessageTextFromCache(uid);
            if (rawText != null)
                return new Message(rawText);
        }
        if (!isLocked())
            Debug.bug("ImapMailbox.getMessage mailbox is not locked!");
        if (!session.verifyConnected())
            return null;
        if (progressNotifier != null && progressNotifier.cancelled())
            return null;
        if (!session.verifySelected(folderName))
            return null;
        if (progressNotifier != null && progressNotifier.cancelled())
            return null;
        String header = fetchPart(uid, "header", null, progressNotifier);
        if (header == null)
            return null;
        Headers headers = Headers.parse(header);
        String charset = null;
        String contentType = headers.getValue(Headers.CONTENT_TYPE);
        if (contentType != null)
            charset = Utilities.getCharsetFromContentType(contentType);
        Log.debug("charset = " + charset);
        String encoding = Utilities.getEncodingFromCharset(charset);
        if (encoding.equalsIgnoreCase("us-ascii"))
            encoding = null;
        else if (encoding.equalsIgnoreCase("iso-8859-1"))
            encoding = null;
        String body = fetchPart(uid, "text", encoding, progressNotifier);
        if (body == null)
            return null;
        if (getBooleanProperty(Property.IMAP_USE_LOCAL_CACHE))
            cacheMessage(uid, header + body, encoding);
        return new Message(header.concat(body), headers);
    }

    private String fetchPart(int uid, String part, String encoding,
        ProgressNotifier progressNotifier)
    {
        FastStringBuffer sb = new FastStringBuffer("uid fetch ");
        sb.append(uid);
        sb.append(" body[");
        sb.append(part);
        sb.append(']');
        session.writeTagged(sb.toString());
        sb = null;
        int length = -1;
        try {
            if (progressNotifier != null && progressNotifier.cancelled()) {
                session.disconnect();
                return null;
            }
            String s = session.readLine();
            if (s == null)
                return null;
            if (s.startsWith("* ")) {
                int index = s.indexOf('(');
                if (index < 0) {
                    Log.error("can't parse response s = |" + s + "|");
                    return null;
                }
                String before = s.substring(0, index).trim();
                if (!before.endsWith(" FETCH")) {
                    Log.error("no FETCH");
                    return null;
                }
                String after = s.substring(index);
                int begin = after.indexOf('{');
                if (begin < 0) {
                    Log.error("no '{'");
                    Log.error("s = |" + s + "|");
                    begin = after.indexOf('"');
                    if (begin < 0)
                        return null;
                    else
                        return parseQuotedString(after.substring(begin));
                }
                int end = after.indexOf('}', begin + 1);
                if (end < 0) {
                    Log.error("no '}'");
                    return null;
                }
                try {
                    length = Integer.parseInt(after.substring(begin + 1, end));
                }
                catch (NumberFormatException e) {
                    Log.error(e);
                }
            }
            if (length < 0) {
                Log.error("can't determine length");
                Log.error("s = |" + s + "|");
                return null;
            }
            sb = new FastStringBuffer(length + 64);
            while (true) {
                if (progressNotifier != null && progressNotifier.cancelled()) {
                    session.disconnect();
                    return null;
                }
                s = session.readLine();
                if (s == null)
                    break;
                if (s.startsWith(session.lastTag() + " OK"))
                    break;
                // Otherwise we need to append the string...
                if (encoding != null) {
                    // Must do conversion.
                    int len = s.length();
                    byte[] bytes = new byte[len];
                    for (int i = 0; i < len; i++)
                        bytes[i] = (byte) s.charAt(i);
                    try {
                        s = new String(bytes, encoding);
                    }
                    catch (UnsupportedEncodingException e) {
                        Log.debug(e);
                        // Conversion isn't going to work, so give up on it.
                        encoding = null;
                    }
                }
                sb.append(s);
                sb.append("\r\n");
                if (progressNotifier != null)
                    progressNotifier.progress("Received ", sb.length(), length);
            }
        }
        catch (Exception e) {
            Log.error(e);
        }
        if (sb != null) {
            Log.debug("advertised length = " + length);
            Log.debug("actual length = " + sb.length());
            sb.setLength(length);
            return sb.toString();
        } else
            return null;
    }

    private String parseQuotedString(final String s)
    {
        Debug.assertTrue(s.length() > 0);
        Debug.assertTrue(s.charAt(0) == '"');
        FastStringBuffer sb = new FastStringBuffer();
        final int length = s.length();
        for (int i = 1; i < length; i++) {
            char c = s.charAt(i);
            if (c == '\\' && i+1 < length)
                sb.append(s.charAt(++i));
            else if (c == '"')
                break;
            else
                sb.append(c);
        }
        return sb.toString();
    }

    public void dispose()
    {
        Log.debug("ImapMailbox.dispose " + folderName + " on " +
            session.getHost());
        Runnable r = new Runnable() {
            public void run()
            {
                session.logout();
            }
        };
        new Thread(r).start();
        MailboxProperties.saveProperties(this);
    }

    protected void finalize() throws Throwable
    {
        Log.debug("ImapMailbox.finalize " + folderName + " on " +
            session.getHost());
        super.finalize();
    }

    private String getProgressText(int n)
    {
        FastStringBuffer sb = new FastStringBuffer(32);
        sb.append("Retrieved ");
        sb.append(n);
        sb.append(" message header");
        if (n > 1)
            sb.append('s');
        return sb.toString();
    }

    // Package scope for testing.
    /*package*/ static String getMessageSet(List list)
    {
        FastStringBuffer sb = new FastStringBuffer();
        int limit = list.size();
        int begin = -1;
        int end = -1;
        for (int i = 0; i < limit; i++) {
            ImapMailboxEntry entry = (ImapMailboxEntry) list.get(i);
            if (begin < 0) {
                begin = entry.getUid();
                end = entry.getUid();
            } else if (entry.getUid() == end + 1) {
                end = entry.getUid();
            } else {
                if (sb.length() > 0)
                    sb.append(',');
                if (begin != end) {
                    Debug.assertTrue(end > begin);
                    sb.append(begin);
                    sb.append(':');
                    sb.append(end);
                    begin = end = entry.getUid();
                } else {
                    sb.append(begin);
                    begin = end = entry.getUid();
                }
            }
        }
        if (sb.length() > 0)
            sb.append(',');
        if (begin != end) {
            Debug.assertTrue(end > begin);
            sb.append(begin);
            sb.append(':');
            sb.append(end);
        } else
            sb.append(begin);
        return sb.toString();
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

    private ImapMessageCache messageCache;

    private void cacheMessage(int uid, String message, String encoding)
    {
        if (messageCache != null) {
            if (messageCache.getUidValidity() != session.getUidValidity())
                messageCache = null;
        }
        if (messageCache == null) {
            messageCache = ImapMessageCache.getMessageCache(this);
            if (messageCache == null)
                return;
        }
        messageCache.store(uid, message, encoding);
    }

    private String getMessageTextFromCache(int uid)
    {
        if (messageCache != null) {
            if (messageCache.getUidValidity() != session.getUidValidity())
                messageCache = null;
        }
        if (messageCache == null) {
            messageCache = ImapMessageCache.getMessageCache(this);
            if (messageCache == null)
                return null;
        }
        return messageCache.getMessageText(uid);
    }
}
