/*
 * MailCommands.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import java.awt.AWTEvent;
import java.awt.event.MouseEvent;
import java.util.List;
import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.Constants;
import org.armedbear.j.Debug;
import org.armedbear.j.Directories;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.Frame;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.History;
import org.armedbear.j.IdleThread;
import org.armedbear.j.InputDialog;
import org.armedbear.j.Line;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.PasswordDialog;
import org.armedbear.j.Position;
import org.armedbear.j.Property;
import org.armedbear.j.Sidebar;
import org.armedbear.j.SimpleEdit;

public final class MailCommands implements Constants
{
    public static void inbox()
    {
        if (!Editor.isMailEnabled())
            return;
        final Editor editor = Editor.currentEditor();
        String inbox = Editor.preferences().getStringProperty(Property.INBOX);
        if (inbox != null)
            openMailbox(editor, inbox);
        else
            openMailbox(editor); // Prompt for mailbox.
    }

    public static void openMailbox()
    {
        if (!Editor.isMailEnabled())
            return;
        final Editor editor = Editor.currentEditor();
        openMailbox(editor);
    }

    public static void openMailbox(String args)
    {
        openMailbox(Editor.currentEditor(), args);
    }

    private static void openMailbox(Editor editor)
    {
        InputDialog d = new InputDialog(editor, "Mailbox:", "Open Mailbox", null);
        d.setHistory(new History("openMailbox"));
        editor.centerDialog(d);
        d.show();
        String s = d.getInput();
        if (s == null || s.length() == 0)
            return;
        editor.repaintNow();
        openMailbox(editor, s);
    }

    public static void openMailbox(Editor editor, String input)
    {
        final MailboxURL url = MailboxURL.parse(input);
        if (url != null)
            openMailbox(editor, url);
        else
            MessageDialog.showMessageDialog("Invalid mailbox name",
                "Open Mailbox");
    }

    public static void openMailbox(Editor editor, MailboxURL url)
    {
        String limitPattern = url.getLimitPattern();
        Log.debug("limitPattern = |" + limitPattern + "|");
        MailboxFilter filter = null;
        boolean badLimitPattern = false;
        if (limitPattern != null) {
            filter = MailboxFilter.getMailboxFilter(limitPattern);
            if (filter == null) {
                MessageDialog.showMessageDialog("Bad limit pattern",
                    "Open Mailbox");
                limitPattern = null;
                badLimitPattern = true;
            }
        }
        if (url instanceof ImapURL || url instanceof PopURL) {
            Mailbox mb = getMailbox(editor, url);
            if (mb != null) {
                if (mb.isLoaded()) {
                    if (filter != null && mb.getLimitFilter() == null) {
                        mb.limit(filter);
                        mb.setLimitPattern(limitPattern);
                    } else if (!badLimitPattern &&
                        mb == Editor.currentEditor().getBuffer()) {
                        mb.limit(filter);
                        mb.setLimitPattern(limitPattern);
                    }
                } else {
                    mb.setLimitFilter(filter);
                    mb.setLimitPattern(limitPattern);
                }
                editor.makeNext(mb);
                editor.switchToBuffer(mb);
                FolderTreeModel.getDefaultModel().maybeAddNodeForFolder(url);
                // Add appropriate idle thread tasks.
                IdleThread idleThread = IdleThread.getInstance();
                if (idleThread != null) {
                    idleThread.maybeAddTask(CheckMailTask.getInstance());
                    if (mb instanceof PopMailbox)
                        idleThread.maybeAddTask(
                            RewriteMailboxesTask.getInstance());
                }
            }
        } else {
            // Local mailbox (or local drafts folder).
            Debug.assertTrue(url instanceof LocalMailboxURL);
            final File file = ((LocalMailboxURL)url).getFile();
            Mailbox mb = null;
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf instanceof LocalMailbox) {
                    if (((LocalMailbox)buf).getMailboxFile().equals(file)) {
                        mb = (LocalMailbox) buf;
                        break;
                    }
                } else if (buf instanceof Drafts) {
                    if (((Drafts)buf).getDirectory().equals(file)) {
                        mb = (Drafts) buf;
                        break;
                    }
                }
            }
            if (mb == null) {
                // Not found.
                if (file.equals(Directories.getDraftsFolder()))
                    mb = new Drafts((LocalMailboxURL)url);
                else
                    mb = new LocalMailbox((LocalMailboxURL)url);
            }
            mb.setLimitFilter(filter);
            mb.setLimitPattern(limitPattern);
            editor.makeNext(mb);
            editor.switchToBuffer(mb);
        }
    }

    public static Buffer getMailboxBuffer(Editor editor, MailboxURL url)
    {
        return getMailbox(editor, url);
    }

    public static Mailbox getMailbox(Editor editor, MailboxURL url)
    {
        if (url instanceof ImapURL) {
            // IMAP.
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf instanceof ImapMailbox) {
                    ImapMailbox mb = (ImapMailbox) buf;
                    if (mb.getUrl().equals(url))
                        return mb;
                }
            }
            final ImapURL imapUrl = (ImapURL) url;
            ImapSession session = ImapSession.getSession(imapUrl);
            if (session == null) {
                String user = imapUrl.getUser();
                if (user == null) {
                    user = InputDialog.showInputDialog(editor, "Login:",
                        "Login on " + imapUrl.getHost());
                    if (user == null || user.length() == 0)
                        return null;
                    session = ImapSession.getSession(imapUrl, user);
                }
                if (session == null) {
                    String password = PasswordDialog.showPasswordDialog(editor,
                        "Password:", "Password");
                    if (password == null || password.length() == 0)
                        return null;
                    session = ImapSession.getSession(imapUrl, user, password);
                }
            }
            if (session != null)
                return new ImapMailbox(imapUrl, session);
        } else if (url instanceof PopURL) {
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf instanceof PopMailbox) {
                    PopMailbox mb = (PopMailbox) buf;
                    if (mb.getUrl().equals(url))
                        return mb;
                }
            }
            final PopURL popUrl = (PopURL) url;
            PopSession session = PopSession.getSession(popUrl);
            if (session == null) {
                String user = popUrl.getUser();
                if (user == null) {
                    user = InputDialog.showInputDialog(editor, "Login:",
                        "Login on " + popUrl.getHost());
                    if (user == null || user.length() == 0)
                        return null;
                    session = PopSession.getSession(popUrl, user);
                }
                if (session == null) {
                    String password = PasswordDialog.showPasswordDialog(editor,
                        "Password:", "Password");
                    if (password == null || password.length() == 0)
                        return null;
                    session = PopSession.getSession(popUrl, user, password);
                }
            }
            if (session != null)
                return new PopMailbox(popUrl, session);
        }
        return null;
    }

    public static void compose()
    {
        if (!Editor.isMailEnabled())
            return;
        activateMailCompositionBuffer(Editor.currentEditor(),
            new SendMail());
    }

    public static void ccGroup()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof SendMail)
            ((SendMail)buffer).ccGroup();
    }

    public static void attachFile()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof SendMail)
            ((SendMail)buffer).attachFile();
    }

    public static void send()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof SendMail)
            ((SendMail)buffer).send();
    }

    public static void sendMailElectricColon()
    {
        final Editor editor = Editor.currentEditor();
        final Line dotLine = editor.getDotLine();
        final int dotOffset = editor.getDotOffset();
        if (editor.getModeId() != SEND_MAIL_MODE || editor.getMark() != null ||
            dotOffset != dotLine.length()) {
            editor.insertNormalChar(':');
            return;
        }
        final SendMail sm = (SendMail) editor.getBuffer();
        if (sm.isHeaderLine(dotLine)) {
            // If the line begins with maybe some whitespace, followed by some
            // non-whitespace chars and now a colon, force the non-whitespace
            // chars (the header name) into column 0.
            int i;
            for (i = 0; i < dotOffset; i++) {
                char c = dotLine.charAt(i);
                if (c != ' ' && c != '\t')
                    break;
            }
            if (i == dotOffset) {
                // The colon we're inserting will be the first non-whitespace
                // character.
                editor.insertNormalChar(':');
                return;
            }
            final int begin = i;
            for (++i; i < dotOffset; i++) {
                char c = dotLine.charAt(i);
                if (c == ' ' || c == '\t') {
                    // Whitespace after non-whitespace: don't be electric.
                    editor.insertNormalChar(':');
                    return;
                }
            }
            try {
                sm.lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                editor.addUndo(SimpleEdit.LINE_EDIT);
                dotLine.setText(dotLine.substring(begin).concat(":"));
                sm.modified();
            }
            finally {
                sm.unlockWrite();
            }
            editor.getDot().setOffset(dotLine.length());
            editor.moveCaretToDotCol();
            Editor.updateInAllEditors(dotLine);
        } else
            editor.insertNormalChar(':');
    }

    public static void sendMailTab()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof SendMail)
            ((SendMail)buffer).tab(editor);
    }

    public static void sendMailBackTab()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof SendMail)
            ((SendMail)buffer).backTab(editor);
    }

    public static void messageMoveToFolder()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).moveMessage();
    }

    public static void messageDelete()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).deleteMessage();
    }

    public static void messageFlag()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).flagMessage();
    }

    public static void messageNext()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof NewsGroupMessageBuffer)
            ((NewsGroupMessageBuffer)buffer).nextArticle();
        else if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).nextMessage();
    }

    public static void messagePrevious()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof NewsGroupMessageBuffer)
            ((NewsGroupMessageBuffer)buffer).previousArticle();
        else if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).previousMessage();
    }

    public static void messageNextInThread()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).nextInThread();
    }

    public static void messagePreviousInThread()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).previousInThread();
    }

    public static void messageParent()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).parentMessage();
    }

    public static void messageForward()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof MessageBuffer) {
            MessageBuffer messageBuffer = (MessageBuffer) buffer;
            SendMail sm = new SendMail(messageBuffer);
            activateMailCompositionBuffer(editor, sm);
        }
    }

    public static void messageReplyToSender()
    {
        messageReply(false);
    }

    public static void messageReplyToGroup()
    {
        messageReply(true);
    }

    private static void messageReply(boolean replyToGroup)
    {
        Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof MessageBuffer) {
            MessageBuffer messageBuffer = (MessageBuffer) buffer;
            if (messageBuffer.getMailbox() != null) {
                SendMail sm = new SendMail(messageBuffer, replyToGroup);
                activateMailCompositionBuffer(editor, sm);
            }
        }
    }

    private static void activateMailCompositionBuffer(Editor editor,
        SendMail sm)
    {
        editor.makeNext(sm);
        Frame frame = editor.getFrame();
        editor.switchToBuffer(sm);
        // Switching buffers might close the original editor.
        Editor ed =
            frame.contains(editor) ? editor : frame.getCurrentEditor();
        if (ed.getBuffer() == sm) {
            ed.setDot(sm.getInitialDotPos());
            ed.moveCaretToDotCol();
            ed.updateDisplay();
        }
    }

    public static final void messageIndex()
    {
        messageIndex(Editor.currentEditor());
    }

    public static void messageIndex(Editor editor)
    {
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof MessageBuffer) {
            MessageBuffer messageBuffer = (MessageBuffer) buffer;
            Mailbox mailbox = messageBuffer.getMailbox();
            if (mailbox == null)
                return;
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf == mailbox) {
                    final Line line =
                        mailbox.getLineForEntry(messageBuffer.getMailboxEntry());
                    if (editor == Editor.currentEditor()) {
                        Editor otherEditor = editor.getOtherEditor();
                        if (otherEditor != null && messageBuffer.isTransient()) {
                            messageBuffer.saveWindowState(editor);
                            editor.otherWindow();
                            editor.unsplitWindow();
                            editor = Editor.currentEditor();
                        }
                    }
                    editor.activate(mailbox);
                    messageBuffer.kill();
                    Sidebar.refreshSidebarInAllFrames();
                    if (line != null) {
                        editor.getDot().moveTo(line, 0);
                        editor.setUpdateFlag(REFRAME);
                        editor.setMark(null);
                        editor.moveCaretToDotCol();
                    }
                    return;
                }
            }
        }
    }

    public static void mailboxLastMessage()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Mailbox) {
            if (editor.getDot() != null) {
                Position end = buffer.getEnd();
                if (end != null) {
                    editor.moveDotTo(end.getLine(), 0);
                    editor.setUpdateFlag(REFRAME);
                }
            }
        }
    }

    public static void mailboxGetNewMessages()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).getNewMessages();
    }

    public static void mailboxLimit()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).limit();
    }

    public static void mailboxUnlimit()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).unlimit();
    }

    public static void mailboxReadMessage()
    {
        mailboxReadMessage(false);
    }

    public static void mailboxReadMessageOtherWindow()
    {
        mailboxReadMessage(true);
    }

    private static void mailboxReadMessage(boolean useOtherWindow)
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Mailbox && editor.getDot() != null) {
            // If this method is invoked via a mouse event mapping, move dot to
            // location of mouse click first.
            AWTEvent e = editor.getDispatcher().getLastEvent();
            if (e instanceof MouseEvent)
                editor.mouseMoveDotToPoint((MouseEvent) e);
            if (useOtherWindow)
                ((Mailbox)buffer).readMessageOtherWindow(editor.getDotLine());
            else
                ((Mailbox)buffer).readMessage(editor.getDotLine());
        }
    }

    public static void mailboxCreateFolder()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).createFolder();
    }

    public static void mailboxDeleteFolder()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).deleteFolder();
    }

    public static void mailboxSaveToFolder()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).saveToFolder();
    }

    public static void mailboxMoveToFolder()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).moveToFolder();
    }

    public static void mailboxDelete()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).delete();
    }

    public static void mailboxUndelete()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).undelete();
    }

    public static void mailboxMarkRead()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).markRead();
    }

    public static void mailboxMarkUnread()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).markUnread();
    }

    public static void mailboxFlag()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).flag();
    }

    public static void mailboxTag()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).tag(editor, editor.getDotLine());
    }

    public static void mailboxTagPattern()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).tagPattern();
    }

    public static void mailboxUntagAll()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).untagAll();
    }

    public static void mailboxToggleRaw()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).toggleRaw();
    }

    public static void mailboxExpunge()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).expunge();
    }

    public static final void mailboxStop()
    {
        Editor.currentEditor().cancelBackgroundProcess();
    }

    public static void messageToggleHeaders()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).toggleHeaders();
    }

    public static void messageToggleRaw()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).toggleRaw();
    }

    public static void messageToggleWrap()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).toggleWrap();
    }

    public static void messageViewAttachment()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).viewAttachment();
        else if (buffer instanceof NewsGroupMessageBuffer)
            ((NewsGroupMessageBuffer)buffer).viewInline();
    }

    public static void messageSaveAttachment()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).saveAttachment();
    }

    public static void bounce()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).bounce();
        else if (buffer instanceof MessageBuffer)
            ((MessageBuffer)buffer).bounce();
    }

    public static MailAddress[] bounceGetTo(Editor editor, int count)
    {
        FastStringBuffer sb = new FastStringBuffer("Bounce ");
        sb.append(count);
        sb.append(" message");
        if (count > 1)
            sb.append('s');
        sb.append(" to:");
        InputDialog d = new InputDialog(editor, sb.toString(), "Bounce", null);
        d.setHistory(new History("bounceMessage"));
        editor.centerDialog(d);
        d.show();
        String input = d.getInput();
        if (input == null)
            return null;
        MailAddress[] to = MailAddress.parseAddresses(input);
        if (to == null || to.length == 0)
            return null;
        return to;
    }

    public static void toggleGroupByThread()
    {
        final Buffer buffer = Editor.currentEditor().getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).toggleGroupByThread();
    }

    public static void foldThread()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).foldThread(editor.getDotLine());
    }

    public static void foldThreads()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof Mailbox)
            ((Mailbox)buffer).foldThreads();
    }
}
