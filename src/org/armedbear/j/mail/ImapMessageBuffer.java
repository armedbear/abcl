/*
 * ImapMessageBuffer.java
 *
 * Copyright (C) 2000-2005 Peter Graves
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

import javax.swing.SwingUtilities;
import org.armedbear.j.BackgroundProcess;
import org.armedbear.j.Debug;
import org.armedbear.j.Editor;
import org.armedbear.j.Headers;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.ProgressNotifier;
import org.armedbear.j.StatusBarProgressNotifier;

public final class ImapMessageBuffer extends MessageBuffer
{
    private boolean cancelled;

    /*package*/ ImapMessageBuffer(ImapMailbox mailbox, ImapMailboxEntry entry)
    {
        super();
        // Mailbox is locked in ImapMailbox.readMessage() before this
        // constructor is called.
        Debug.assertTrue(mailbox.isLocked());
        init(mailbox, entry);
    }

    /*package*/ ImapMessageBuffer(ImapMailbox mailbox, ImapMailboxEntry entry,
        String rawText)
    {
        super();
        init(mailbox, entry);
        message = new Message(rawText);
        parseMessage();
        title = message.getHeaderValue(Headers.SUBJECT);
        if (title == null)
            title = "";
        allHeaders = message.getAllHeaders();
        defaultHeaders = getDefaultHeaders(allHeaders);
        rawBody = message.getRawBody();
        setText();
        unmodified();
        renumber();
        formatter.parseBuffer();
        setLoaded(true);
    }

    private void init(ImapMailbox mailbox, ImapMailboxEntry entry)
    {
        this.mailbox = mailbox;
        showRawText = mailbox.showRawText;
        showFullHeaders = mailbox.showFullHeaders;
        setEntry(entry);
        initializeUndo();
        type = TYPE_NORMAL;
        lineSeparator = "\n";
        mode = MessageMode.getMode();
        setFormatter(mode.getFormatter(this));
        readOnly = true;
    }

    public int load()
    {
        if (isLoaded())
            return LOAD_COMPLETED;
        if (!mailbox.isLocked()) {
            Debug.bug();
            setText("");
            return LOAD_FAILED;
        }
        setBusy(true);
        new Thread(loadProcess).start();
        return LOAD_PENDING;
    }

    private BackgroundProcess loadProcess = new BackgroundProcess() {
        private ProgressNotifier progressNotifier;

        public void run()
        {
            // Mailbox is locked in ImapMailbox.readMessage() before calling
            // ImapMessageBuffer constructor.
            if (!mailbox.isLocked()) {
                Debug.bug();
                return;
            }
            try {
                setBackgroundProcess(this);
                progressNotifier =
                    new StatusBarProgressNotifier(ImapMessageBuffer.this);
                progressNotifier.progressStart();
                loadMessage(progressNotifier);
                progressNotifier.setText("");
                progressNotifier.progressStop();
                setBackgroundProcess(null);
                setBusy(false);
            }
            finally {
                mailbox.unlock();
                mailbox.setBusy(false);
                Editor.updateDisplayLater(mailbox);
            }
        }

        public void cancel()
        {
            Log.debug("loadProcess.cancel cancelled!");
            cancelled = true;
            progressNotifier.cancel();
            setBusy(false);
            Log.debug("loadProcess.cancel calling kill");
            kill();
            Log.debug("loadProcess.cancel back from kill");
        }
    };

    public void deleteMessage()
    {
        storeFlagsInternal(ACTION_DELETE);
    }

    public void flagMessage()
    {
        storeFlagsInternal(ACTION_FLAG);
    }

    private static final int ACTION_DELETE = 0;
    private static final int ACTION_FLAG   = 1;

    private void storeFlagsInternal(final int action)
    {
        final Editor editor = Editor.currentEditor();
        if (!mailbox.lock()) {
            editor.status("Mailbox is locked");
            return;
        }
        final int uid = ((ImapMailboxEntry)entry).getUid();
        Runnable deleteMessageRunnable = new Runnable() {
            public void run()
            {
                try {
                    ImapSession session = ((ImapMailbox)mailbox).getSession();
                    String folderName = ((ImapMailbox)mailbox).getFolderName();
                    if (session.verifyConnected() && session.verifySelected(folderName)) {
                        if (session.isReadOnly()) {
                            Log.debug("deleteMessage - read-only - reselecting...");
                            session.reselect(folderName);
                            if (session.isReadOnly()) {
                                ((ImapMailbox)mailbox).readOnlyError();
                                return;
                            }
                        }
                        session.setEcho(true);
                        switch (action) {
                            case ACTION_DELETE:
                                session.uidStore(uid, "+flags.silent (\\deleted)");
                                break;
                            case ACTION_FLAG:
                                // Toggle.
                                if (entry.isFlagged())
                                    session.uidStore(uid, "-flags.silent (\\flagged)");
                                else
                                    session.uidStore(uid, "+flags.silent (\\flagged)");
                                break;
                            default:
                                Debug.assertTrue(false);
                                break;
                        }
                        if (session.getResponse() == ImapSession.OK) {
                            switch (action) {
                                case ACTION_DELETE:
                                    entry.setFlags(entry.getFlags() | MailboxEntry.DELETED);
                                    break;
                                case ACTION_FLAG:
                                    entry.toggleFlag();
                                    break;
                                default:
                                    Debug.assertTrue(false);
                                    break;
                            }
                            mailbox.updateEntry(entry);
                        }
                        session.setEcho(false);
                        MailboxEntry nextEntry = mailbox.getNextUndeleted(entry);
                        if (nextEntry != null) {
                            mailbox.setDotEntry(nextEntry);
                            setEntry(nextEntry);
                            loadMessage(null);
                        } else {
                            Runnable messageIndexRunnable = new Runnable() {
                                public void run()
                                {
                                    MailCommands.messageIndex(editor);
                                    editor.status("Last undeleted message");
                                }
                            };
                            SwingUtilities.invokeLater(messageIndexRunnable);
                        }
                        setBusy(false);
                        editor.updateDisplayLater();
                    }
                }
                finally {
                    setBusy(false);
                    mailbox.unlock();
                }
            }
        };
        setBusy(true);
        new Thread(deleteMessageRunnable).start();
    }

    public void moveMessage()
    {
        final Editor editor = Editor.currentEditor();
        final ImapMailboxEntry toBeMoved = (ImapMailboxEntry) entry;
        String title = "Move Message to Folder";
        String s = ChooseFolderDialog.chooseFolder(editor, title);
        if (s == null)
            return;
        if (!s.startsWith("mailbox:")) {
            // Not local. Extract folder name from URL.
            s = ((ImapMailbox)mailbox).extractFolderName(s);
            if (s == null) {
                MessageDialog.showMessageDialog(editor, "Invalid destination",
                    "Error");
                return;
            }
        }
        if (!mailbox.lock()) {
            editor.status("Mailbox is locked");
            return;
        }
        final String destination = s;
        Runnable moveMessageRunnable = new Runnable() {
            public void run()
            {
                try {
                    ImapSession session = ((ImapMailbox) mailbox).getSession();
                    String folderName = ((ImapMailbox) mailbox).getFolderName();
                    if (session.verifyConnected() &&
                        session.verifySelected(folderName)) {
                        if (session.isReadOnly()) {
                            Log.debug("moveMessage - read-only - reselecting...");
                            session.reselect(folderName);
                            if (session.isReadOnly()) {
                                ((ImapMailbox) mailbox).readOnlyError();
                                return;
                            }
                        }
                        session.setEcho(true);
                        boolean succeeded = false;
                        if (destination.startsWith("mailbox:")) {
                            succeeded = Mail.writeFcc(message, destination,
                                toBeMoved.getFlags() & ~MailboxEntry.TAGGED);
                        } else {
                            session.writeTagged("uid copy " + toBeMoved.getUid() + " " + destination);
                            succeeded = session.getResponse() == ImapSession.OK;
                        }
                        if (succeeded) {
                            session.writeTagged("uid store " + toBeMoved.getUid() + " +flags.silent (\\deleted)");
                            if (session.getResponse() == ImapSession.OK) {
                                toBeMoved.setFlags(toBeMoved.getFlags() | MailboxEntry.DELETED);
                                mailbox.updateEntry(toBeMoved);
                            } else
                                succeeded = false;
                        }
                        session.setEcho(false);
                        if (succeeded) {
                            MailboxEntry nextEntry = mailbox.getNextUndeleted(entry);
                            if (nextEntry != null) {
                                mailbox.setDotEntry(nextEntry);
                                setEntry(nextEntry);
                                loadMessage(null);
                            } else {
                                Runnable messageIndexRunnable = new Runnable() {
                                    public void run()
                                    {
                                        MailCommands.messageIndex(editor);
                                        editor.status("Last undeleted message");
                                    }
                                };
                                SwingUtilities.invokeLater(messageIndexRunnable);
                            }
                            setBusy(false);
                            editor.updateDisplayLater();
                        } else {
                            setBusy(false);
                            Runnable reportError = new Runnable() {
                                public void run()
                                {
                                    editor.updateDisplay();
                                    MessageDialog.showMessageDialog(editor, "Operation failed", "Error");
                                }
                            };
                            SwingUtilities.invokeLater(reportError);
                        }
                    }
                }
                finally {
                    setBusy(false);
                    mailbox.unlock();
                }
            }
        };
        setBusy(true);
        new Thread(moveMessageRunnable).start();
    }
}
