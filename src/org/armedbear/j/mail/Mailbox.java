/*
 * Mailbox.java
 *
 * Copyright (C) 2000-2006 Peter Graves
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Iterator;
import java.util.List;
import javax.swing.Icon;
import javax.swing.SwingUtilities;
import org.armedbear.j.Buffer;
import org.armedbear.j.Debug;
import org.armedbear.j.Dispatcher;
import org.armedbear.j.Display;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.History;
import org.armedbear.j.InputDialog;
import org.armedbear.j.Line;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.Position;
import org.armedbear.j.ProgressNotifier;
import org.armedbear.j.Property;
import org.armedbear.j.PropertyList;
import org.armedbear.j.Sidebar;
import org.armedbear.j.Utilities;
import org.armedbear.j.View;

public abstract class Mailbox extends Buffer
{
    public static final int SORT_BY_DATE_SENT = 0;

    protected MailboxURL url;

    boolean showFullHeaders;
    boolean showRawText;

    protected boolean dirty;

    protected int unreadMessageCount;
    protected int newMessageCount;

    protected List entries;

    private long lastCheckMillis;

    private int sortBy = SORT_BY_DATE_SENT;

    private MessageBuffer previewBuffer;

    protected Mailbox()
    {
    }

    protected Mailbox(MailboxURL url)
    {
        this.url = url;
        PropertyList props = MailboxProperties.getProperties(url);
        if (props != null)
            properties.putAll(props);
    }

    public MessageBuffer getPreviewBuffer()
    {
        return previewBuffer;
    }

    public void setPreviewBuffer(MessageBuffer buf)
    {
        previewBuffer = buf;
    }

    public Buffer getSecondary()
    {
        return previewBuffer;
    }

    public final MailboxURL getUrl()
    {
        return url;
    }

    public final int getSortBy()
    {
        return sortBy;
    }

    public abstract String getName();

    public synchronized final void setEntries(List entries)
    {
        this.entries = entries;
    }

    public synchronized final long getLastCheckMillis()
    {
        return lastCheckMillis;
    }

    protected synchronized final void setLastCheckMillis(long when)
    {
        lastCheckMillis = when;
    }

    public synchronized long getLastErrorMillis()
    {
        return 0;
    }

    public abstract void getNewMessages();

    public void getNewMessages(boolean userInitiated)
    {
    }

    public abstract void readMessage(Line line);

    public void readMessageOtherWindow(Line line)
    {
    }

    protected void activateMessageBuffer(Editor editor,
        MessageBuffer messageBuffer, boolean useOtherWindow)
    {
        editor.makeNext(messageBuffer);
        if (useOtherWindow) {
            Buffer oldBuffer = null;
            Editor ed = editor.getOtherEditor();
            if (ed != null)
                oldBuffer = ed.getBuffer();
            messageBuffer.setTransient(true);
            editor.activateInOtherWindow(messageBuffer,
                                         messageBuffer.getSplit());
            previewBuffer = messageBuffer;
            if (oldBuffer != null && oldBuffer != messageBuffer) {
                if (oldBuffer.isTransient())
                    oldBuffer.kill();
            }
        } else
            editor.activate(messageBuffer);
    }

    public abstract void createFolder();
    public abstract void deleteFolder();
    public abstract void saveToFolder();
    public abstract void moveToFolder();

    public abstract void delete();

    public abstract void undelete();

    public abstract void markRead();

    public abstract void markUnread();

    public void flag()
    {
    }

    public abstract void setAnsweredFlag(MailboxEntry entry);

    public final int getUnreadMessageCount()
    {
        return unreadMessageCount;
    }

    public final int getNewMessageCount()
    {
        return newMessageCount;
    }

    // Count new and unread messages, respecting the limit that is in force
    // (if any).
    public void countMessages()
    {
        unreadMessageCount = newMessageCount = 0;
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof MailboxLine) {
                MailboxEntry entry = ((MailboxLine)line).getMailboxEntry();
                if (entry.isNew())
                    ++newMessageCount;
                if (entry.isUnread())
                    ++unreadMessageCount;
            }
        }
    }

    // Clear RECENT flag for all messages.
    // Returns true if there was any change.
    protected boolean clearRecent()
    {
        if (!isLocked())
            Debug.bug("clearRecent mailbox not locked!");
        boolean changed = false;
        if (entries != null) {
            final int size = entries.size();
            for (int i = 0; i < size; i++) {
                MailboxEntry entry = (MailboxEntry) entries.get(i);
                if ((entry.getFlags() & MailboxEntry.RECENT) != 0) {
                    entry.setFlags(entry.getFlags() & ~MailboxEntry.RECENT);
                    changed = true;
                }
            }
        }
        if (changed)
            setDirty(true);
        return changed;
    }

    public final void setDirty(boolean b)
    {
        dirty = b;
    }

    public final boolean isDirty()
    {
        return dirty;
    }

    public void tag(Editor editor, Line line)
    {
        if (line instanceof MailboxLine) {
            MailboxEntry entry = ((MailboxLine)line).getMailboxEntry();
            entry.toggleTag();
            Editor.updateInAllEditors(line);
            if (line.next() != null) {
                editor.getDot().setLine(line.next());
                editor.setMark(null);
                editor.moveDotToCaretCol();
            }
        }
    }

    public void tagPattern()
    {
        Editor editor = Editor.currentEditor();
        InputDialog d = new InputDialog(editor, "Pattern:", "Tag Pattern", null);
        d.setHistory(new History("mailboxTagPattern"));
        editor.centerDialog(d);
        d.show();
        String pattern = d.getInput();
        if (pattern == null)
            return;
        pattern = pattern.trim();
        if (pattern.length() == 0)
            return;
        editor.repaintNow();
        MailboxFilter filter = MailboxFilter.getMailboxFilter(pattern);
        if (filter != null)
            tag(filter);
        else
            MessageDialog.showMessageDialog("Bad pattern", "Tag Pattern");
    }

    private void tag(MailboxFilter filter)
    {
        Editor.currentEditor().setWaitCursor();
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof MailboxLine) {
                MailboxEntry entry = ((MailboxLine)line).getMailboxEntry();
                if (!entry.isTagged()) {
                    if (filter.accept(entry)) {
                        entry.tag();
                        Editor.updateInAllEditors(line);
                    }
                }
            }
        }
    }

    public void untagAll()
    {
        Editor.currentEditor().setWaitCursor();
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof MailboxLine) {
                MailboxEntry entry = ((MailboxLine)line).getMailboxEntry();
                if (entry.isTagged()) {
                    entry.untag();
                    Editor.updateInAllEditors(line);
                }
            }
        }
    }

    public void toggleRaw()
    {
        showRawText = !showRawText;
        Editor.currentEditor().status("Raw mode " + (showRawText ? "on" : "off"));
    }

    public List getEntries()
    {
        return entries;
    }

    public List getTaggedEntries()
    {
        ArrayList taggedEntries = null;
        final int size = entries.size();
        for (int i = 0; i < size; i++) {
            MailboxEntry entry = (MailboxEntry) entries.get(i);
            if (entry.isTagged()) {
                if (taggedEntries == null)
                    taggedEntries = new ArrayList();
                taggedEntries.add(entry);
            }
        }
        return taggedEntries;
    }

    public MailboxEntry getEntryAtDot(Editor editor)
    {
        Line line = editor.getDotLine();
        if (line instanceof MailboxLine)
            return ((MailboxLine)line).getMailboxEntry();
        else
            return null;
    }

    public abstract void expunge();

    public abstract int getMessageCount();

    public Message getMessage(MailboxEntry entry, ProgressNotifier progressNotifier)
    {
        Debug.assertTrue(false);
        return null;
    }

    public MailboxEntry getNextUndeleted(MailboxEntry entry)
    {
        Line line;
        for (line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof MailboxLine) {
                if (entry == ((MailboxLine)line).getMailboxEntry())
                    break;
            }
        }
        if (line != null) {
            for (line = line.next(); line != null; line = line.next()) {
                if (line instanceof MailboxLine) {
                    MailboxEntry maybe = ((MailboxLine)line).getMailboxEntry();
                    if (!maybe.isDeleted())
                        return maybe;
                }
            }
        }
        return null;
    }

    public MailboxEntry getPreviousUndeleted(MailboxEntry entry)
    {
        Line line;
        for (line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof MailboxLine) {
                if (entry == ((MailboxLine)line).getMailboxEntry())
                    break;
            }
        }
        if (line != null) {
            for (line = line.previous(); line != null; line = line.previous()) {
                if (line instanceof MailboxLine) {
                    MailboxEntry maybe = ((MailboxLine)line).getMailboxEntry();
                    if (!maybe.isDeleted())
                        return maybe;
                }
            }
        }
        return null;
    }

    public MailboxEntry getNextInThread(MailboxEntry entry)
    {
        String subject = entry.getSubject();
        if (subject == null)
            return null; // But there are other things we could try...
        if (subject.toLowerCase().startsWith("re: "))
            subject = subject.substring(4);
        // Find current entry.
        Line line;
        for (line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof MailboxLine)
                if (entry == ((MailboxLine)line).getMailboxEntry())
                    break;
        }
        if (line != null) {
            // Search later entries.
            for (line = line.next(); line != null; line = line.next()) {
                if (line instanceof MailboxLine) {
                    MailboxEntry maybe = ((MailboxLine)line).getMailboxEntry();
                    // Note that we don't skip over deleted messages here.
                    String s = maybe.getSubject();
                    if (s != null) {
                        if (s.toLowerCase().startsWith("re: "))
                            s = s.substring(4);
                        if (s.equals(subject))
                            return maybe;
                    }
                }
            }
        }
        return null;
    }

    public MailboxEntry getPreviousInThread(MailboxEntry entry)
    {
        String subject = entry.getSubject();
        if (subject == null)
            return null; // But there are other things we could try...
        if (subject.toLowerCase().startsWith("re: "))
            subject = subject.substring(4);
        // Find current entry.
        Line line;
        for (line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof MailboxLine)
                if (entry == ((MailboxLine)line).getMailboxEntry())
                    break;
        }
        if (line != null) {
            // Search earlier entries.
            for (line = line.previous(); line != null; line = line.previous()) {
                if (line instanceof MailboxLine) {
                    MailboxEntry maybe = ((MailboxLine)line).getMailboxEntry();
                    // Note that we don't skip over deleted messages here.
                    String s = maybe.getSubject();
                    if (s != null) {
                        if (s.toLowerCase().startsWith("re: "))
                            s = s.substring(4);
                        if (s.equals(subject))
                            return maybe;
                    }
                }
            }
        }
        return null;
    }

    public MailboxEntry getEntryForMessageId(String messageId)
    {
        if (messageId == null)
            return null;
        if (entries != null) {
            for (int i = entries.size()-1; i >= 0; i--) {
                MailboxEntry entry = (MailboxEntry) entries.get(i);
                if (entry != null) {
                    if (messageId.equals(entry.getMessageId()))
                        return entry;
                }
            }
        }
        return null;
    }

    public void bounce()
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        final Editor editor = Editor.currentEditor();
        List list = getTaggedEntries();
        if (list == null) {
            MailboxEntry entry = getEntryAtDot(editor);
            if (entry == null)
                return;
            list = new ArrayList(1);
            list.add(entry);
        }
        final List toBeBounced = list;
        // Get bounce addresses from user.
        final MailAddress[] to = MailCommands.bounceGetTo(editor, toBeBounced.size());
        if (to == null)
            return;
        Runnable bounceRunnable = new Runnable() {
            public void run()
            {
                boolean succeeded = false;
                try {
                    succeeded = bounceMessages(toBeBounced, to);
                }
                finally {
                    unlock();
                    setBusy(false);
                    editor.updateDisplayLater();
                }
                if (succeeded) {
                    Runnable successRunnable = new Runnable() {
                        public void run()
                        {
                            final int size = toBeBounced.size();
                            FastStringBuffer sb = new FastStringBuffer(String.valueOf(size));
                            sb.append(" message");
                            if (size > 1)
                                sb.append('s');
                            sb.append(" bounced");
                            editor.status(sb.toString());
                        }
                    };
                    SwingUtilities.invokeLater(successRunnable);
                } else {
                    Runnable errorRunnable = new Runnable() {
                        public void run()
                        {
                            MessageDialog.showMessageDialog(editor, "Failed", "Bounce");
                        }
                    };
                    SwingUtilities.invokeLater(errorRunnable);
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(bounceRunnable).start();
        } else
            editor.status("Mailbox is locked");
    }

    private boolean bounceMessages(List toBeBounced, MailAddress[] to)
    {
        Log.debug("bounceMessages initializing SMTP session...");
        SmtpSession smtp = SmtpSession.getDefaultSession();
        if (smtp == null)
            return false;
        try {
            for (int i = 0; i < toBeBounced.size(); i++) {
                MailboxEntry entry = (MailboxEntry) toBeBounced.get(i);
                if (!Mail.bounceMessage(getMessage(entry, null), to, smtp))
                    return false;
            }
            return true;
        }
        finally {
            Debug.assertTrue(smtp != null);
            Log.debug("bounceMessages closing SMTP session...");
            smtp.quit();
        }
    }

    private String limitPattern;

    public final String getLimitPattern()
    {
        return limitPattern;
    }

    public final void setLimitPattern(String pattern)
    {
        limitPattern = pattern;
    }

    public void limit()
    {
        Editor editor = Editor.currentEditor();
        InputDialog d = new InputDialog(editor, "Pattern:", "Limit", limitPattern);
        d.setHistory(new History("mailboxLimit"));
        editor.centerDialog(d);
        d.show();
        String pattern = d.getInput();
        if (pattern == null)
            return; // No change (user cancelled input dialog).
        editor.repaintNow();
        pattern = pattern.trim();
        if (pattern.length() == 0)
            unlimit();
        else {
            MailboxFilter filter = MailboxFilter.getMailboxFilter(pattern);
            if (filter != null) {
                if (lock()) {
                    try {
                        limitPattern = pattern;
                        limit(filter);
                    }
                    finally {
                        unlock();
                    }
                } else
                    editor.status("Mailbox is locked");
            } else
                MessageDialog.showMessageDialog("Bad limit pattern", "Limit");
        }
    }

    public void unlimit()
    {
        if (lock()) {
            try {
                limitPattern = null;
                limit(null);
            }
            finally {
                unlock();
            }
        } else
            Editor.currentEditor().status("Mailbox is locked");
    }

    // The limit filter that's currently in effect.
    private MailboxFilter limitFilter;

    public final MailboxFilter getLimitFilter()
    {
        return limitFilter;
    }

    public final void setLimitFilter(MailboxFilter filter)
    {
        limitFilter = filter;
    }

    public void limit(MailboxFilter filter)
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        final Editor editor = Editor.currentEditor();
        editor.repaintNow();
        editor.setWaitCursor();
        // Remember where we are.
        MailboxEntry currentEntry = null;
        if (editor.getDot() != null && editor.getDotLine() instanceof MailboxLine)
            currentEntry = ((MailboxLine)editor.getDotLine()).getMailboxEntry();
        limitFilter = filter;
        refreshBuffer();
        // Update message count in sidebar buffer list.
        Sidebar.repaintBufferListInAllFrames();
        Line dotLine = null;
        if (getFirstLine() != null) {
            dotLine = getFirstLine();
            if (currentEntry != null) {
                boolean groupByThread =
                    getBooleanProperty(Property.GROUP_BY_THREAD);
                for (Line line = getFirstLine(); line != null; line = line.next()) {
                    MailboxEntry entry = ((MailboxLine)line).getMailboxEntry();
                    if (entry == currentEntry) {
                        dotLine = line;
                        break;
                    }
                    if (!groupByThread) {
                        if (RFC822Date.compare(entry.getDate(), currentEntry.getDate()) < 0)
                            dotLine = line;
                        else
                            break;
                    }
                }
            }
        }
        invalidate();
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.updateLocation();
                Display display = ed.getDisplay();
                if (dotLine != null) {
                    ed.setDot(dotLine, 0);
                    display.moveCaretToDotCol();
                    ed.setMark(null);
                    display.setTopLine(getFirstLine());
                    display.setUpdateFlag(REPAINT);
                } else {
                    ed.setDot(null);
                    ed.setMark(null);
                    display.setTopLine(null);
                }
                ed.updateDisplay();
            }
        }
        editor.setDefaultCursor();
    }

    public MailboxLine getLineForEntry(MailboxEntry entry)
    {
        if (entry != null) {
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                if (line instanceof MailboxLine)
                    if (((MailboxLine)line).getMailboxEntry() == entry)
                        return (MailboxLine)line;
            }
        }
        return null;
    }

    public MailboxLine findLineForEntry(MailboxEntry entry)
    {
        if (entry != null) {
            // First look through all the entries for an exact match.
            for (Line l = getFirstLine(); l != null; l = l.next()) {
                MailboxLine line = (MailboxLine) l;
                MailboxEntry e = line.getMailboxEntry();
                if (e == entry)
                    return line;
            }
            // We didn't find an exact match.
            boolean groupByThread =
                getBooleanProperty(Property.GROUP_BY_THREAD);
            for (Line l = getFirstLine(); l != null; l = l.next()) {
                if (l instanceof MailboxLine) {
                    MailboxLine line = (MailboxLine) l;
                    MailboxEntry e = line.getMailboxEntry();
                    // Only check date and size.
                    if (e.getDate().equals(entry.getDate())) {
                        if (e.getSize() == entry.getSize()) {
                            // Found it!
                            Log.debug("findLineForEntry date/size match");
                            return line;
                        }
                    } else if (!groupByThread) {
                        if (RFC822Date.compare(e.getDate(), entry.getDate()) > 0) {
                            // We're past it (assuming mailbox is sorted by date).
                            // The entry we're looking for was deleted. Return the
                            // current line.
                            return line;
                        }
                    } else if (line.next() == null) {
                        // Last line of mailbox.
                        return line;
                    }
                }
            }
        }
        return null;
    }

    public int getLineNumberForEntry(MailboxEntry entry)
    {
        Line line = getLineForEntry(entry);
        if (line != null)
            return line.lineNumber();
        else
            return -1;
    }

    // Find first message that's not seen and not deleted.
    public Position getInitialDotPos()
    {
        if (getFirstLine() == null)
            return null;
        Line line = getFirstLine();
        while (true) {
            if (line instanceof MailboxLine) {
                MailboxEntry entry = ((MailboxLine)line).getMailboxEntry();
                if (entry != null) {
                    int flags = entry.getFlags();
                    if ((flags & MailboxEntry.SEEN) == 0)
                        if ((flags & MailboxEntry.DELETED) == 0)
                            return new Position(line, 0);
                }
            }
            if (line.next() == null)
                break; // Reached last line.
            line = line.next();
        }
        return new Position(line, 0);
    }

    public MailboxEntry getInitialEntry()
    {
        if (getFirstLine() == null)
            return null;
        Line line = getFirstLine();
        while (true) {
            if (line instanceof MailboxLine) {
                MailboxEntry entry = ((MailboxLine)line).getMailboxEntry();
                if (entry != null) {
                    int flags = entry.getFlags();
                    if ((flags & MailboxEntry.SEEN) == 0)
                        if ((flags & MailboxEntry.DELETED) == 0)
                            return entry;
                }
            }
            if (line.next() == null)
                break; // Reached last line.
            line = line.next();
        }
        return ((MailboxLine)line).getMailboxEntry();
    }

    public void updateEntry(MailboxEntry entry)
    {
        MailboxLine line = getLineForEntry(entry);
        if (line != null) {
            line.setText(entry.toString(line.getDepth()));
            Editor.updateInAllEditors(this, line);
        }
    }

    public final void toggleGroupByThread()
    {
        boolean groupByThread = getBooleanProperty(Property.GROUP_BY_THREAD);
        setProperty(Property.GROUP_BY_THREAD, !groupByThread);
        sort();
    }

    private void sort()
    {
        final Editor editor = Editor.currentEditor();
        // Remember where we are.
        final MailboxEntry currentEntry;
        if (editor.getDot() != null && editor.getDotLine() instanceof MailboxLine)
            currentEntry = ((MailboxLine)editor.getDotLine()).getMailboxEntry();
        else
            currentEntry = null;
        final Runnable completionRunnable = new Runnable() {
            public void run()
            {
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    View view = new View();
                    view.setDotEntry(currentEntry != null ? currentEntry : getInitialEntry());
                    ed.setView(Mailbox.this, view);
                    if (ed.getBuffer() == Mailbox.this) {
                        ed.bufferActivated(true);
                        ed.updateDisplay();
                    }
                }
            }
        };
        Runnable sortRunnable = new Runnable() {
            public void run()
            {
                editor.setWaitCursor();
                try {
                    refreshBuffer();
                    SwingUtilities.invokeLater(completionRunnable);
                }
                finally {
                    unlock();
                    setBusy(false);
                    editor.setDefaultCursor();
                }
            }
        };
        if (lock()) {
            setBusy(true);
            new Thread(sortRunnable).start();
        }
    }

    public void foldThread(Line line)
    {
        if (!getBooleanProperty(Property.GROUP_BY_THREAD))
            return;
        if (line instanceof MailboxLine) {
            MailboxLine begin = (MailboxLine) line;
            // Find start of thread.
            while (begin.getDepth() > 1) {
                Line prev = begin.previous();
                if (prev instanceof MailboxLine)
                    begin = (MailboxLine) prev;
                else
                    break;
            }
            if (begin.next() instanceof MailboxLine) {
                MailboxLine end = (MailboxLine) begin.next();
                while (end.getDepth() > 1) {
                    Line next = end.next();
                    if (next instanceof MailboxLine)
                        end = (MailboxLine) next;
                    else
                        break;
                }
                for (Line toBeHidden = begin.next(); toBeHidden != end && toBeHidden != null; toBeHidden = toBeHidden.next())
                    toBeHidden.hide();
                renumber();
                Editor.unhideDotInAllFrames(this);
            }
        }
    }

    public void foldThreads()
    {
        if (!getBooleanProperty(Property.GROUP_BY_THREAD))
            return;
        for (Line line = getFirstLine(); line != null; line = line.nextVisible()) {
            if (line instanceof MailboxLine) {
                MailboxLine begin = (MailboxLine) line;
                if (begin.getDepth() == 1) {
                    if (begin.next() instanceof MailboxLine) {
                        MailboxLine end = (MailboxLine) begin.next();
                        while (end.getDepth() > 1) {
                            Line next = end.next();
                            if (next instanceof MailboxLine)
                                end = (MailboxLine) next;
                            else
                                break;
                        }
                        for (Line toBeHidden = begin.next(); toBeHidden != end && toBeHidden != null; toBeHidden = toBeHidden.next())
                            toBeHidden.hide();
                    }
                }
            }
        }
        renumber();
        Editor.unhideDotInAllFrames(this);
    }

    protected void refreshBuffer()
    {
        if (getBooleanProperty(Property.GROUP_BY_THREAD)) {
            long start = System.currentTimeMillis();
            SortByThread sort = new SortByThread(entries);
            sort.run();
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                synchronized (this) {
                    empty();
                    sort.addEntries(this, limitFilter);
                    renumber();
                    countMessages();
                    setLoaded(true);
                }
            }
            finally {
                unlockWrite();
            }
            long elapsed = System.currentTimeMillis() - start;
            Log.debug("refreshBuffer " + elapsed + " ms");
        } else {
            Debug.assertTrue(sortBy == SORT_BY_DATE_SENT);
            // Don't change order of entries!
            ArrayList temp = new ArrayList(entries);
            sortEntriesByDate(temp);
            List matchingEntries = getMatchingEntries(temp, limitFilter);
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                synchronized (this) {
                    empty();
                    final int size = matchingEntries.size();
                    for (int i = 0; i < size; i++)
                        appendLine(((MailboxEntry)matchingEntries.get(i)));
                    renumber();
                    countMessages();
                    setLoaded(true);
                }
            }
            finally {
                unlockWrite();
            }
        }
    }

    // Never returns null.
    private static List getMatchingEntries(List list, MailboxFilter filter)
    {
        if (list == null)
            return new ArrayList();
        if (filter == null)
            return new ArrayList(list);
        ArrayList matchingEntries = new ArrayList();
        final int size = list.size();
        for (int i = 0; i < size; i++) {
            MailboxEntry entry = (MailboxEntry) list.get(i);
            if (filter.accept(entry))
                matchingEntries.add(entry);
        }
        return matchingEntries;
    }

    public final void appendLine(MailboxEntry entry)
    {
        appendLine(new MailboxLine(entry));
    }

    public final void appendLine(MailboxEntry entry, int depth)
    {
        appendLine(new MailboxLine(entry, depth));
    }

    private static void sortEntriesByDate(List list)
    {
        Comparator c = new Comparator() {
            public int compare(Object o1, Object o2)
            {
                return RFC822Date.compare(((MailboxEntry)o1).getDate(),
                    ((MailboxEntry)o2).getDate());
            }
        };
        Collections.sort(list, c);
        int sequenceNumber = 1;
        Iterator iter = list.iterator();
        while (iter.hasNext()) {
            MailboxEntry entry = (MailboxEntry) iter.next();
            entry.setSequenceNumber(sequenceNumber++);
        }
    }

    protected void addEntriesToAddressBook(List list)
    {
        if (list == null)
            return;
        MailAddress userMailAddress = Mail.getUserMailAddress();
        if (userMailAddress == null)
            return;
        AddressBook addressBook = AddressBook.getGlobalAddressBook();
        for (int i = 0; i < list.size(); i++) {
            MailboxEntry sourceEntry = (MailboxEntry) list.get(i);
            int flags = sourceEntry.getFlags();
            if ((flags & MailboxEntry.DELETED) != 0)
                continue;
            MailAddress[] from = sourceEntry.getFrom();
            MailAddress[] to = sourceEntry.getTo();
            MailAddress[] cc = sourceEntry.getCc();
            boolean add = false;
            if ((flags & MailboxEntry.ANSWERED) != 0) {
                // I replied to this message.
                add = true;
            } else if (from != null) {
                for (int j = 0; j < from.length; j++) {
                    MailAddress a = from[j];
                    if (a.matches(userMailAddress)) {
                        // Message is from me.
                        add = true;
                        break;
                    }
                }
            }
            if (!add) {
                if (to != null) {
                    for (int j = 0; j < to.length; j++) {
                        MailAddress a = to[j];
                        if (a.matches(userMailAddress)) {
                            // Message to me.
                            add = true;
                            break;
                        }
                    }
                }
                if (!add && cc != null) {
                    for (int j = 0; j < cc.length; j++) {
                        MailAddress a = cc[j];
                        if (a.matches(userMailAddress)) {
                            // Message copied to me.
                            add = true;
                            break;
                        }
                    }
                }
            }
            if (add) {
                if (to != null)
                    for (int j = 0; j < to.length; j++)
                        addressBook.maybeAddMailAddress(to[j]);
                if (cc != null)
                    for (int j = 0; j < cc.length; j++)
                        addressBook.maybeAddMailAddress(cc[j]);
                if (from != null)
                    for (int j = 0; j < from.length; j++)
                        addressBook.maybeAddMailAddress(from[j]);
            }
        }
        AddressBook.saveGlobalAddressBook();
    }

    // For the buffer list.
    public Icon getIcon()
    {
        return Utilities.getIconFromFile(newMessageCount > 0 ? "mailbox_new.png" : "mailbox.png");
    }

    public String getFileNameForDisplay()
    {
        return "";
    }

    protected void newMessagesStatus()
    {
        if (newMessageCount > 0) {
            FastStringBuffer sb = new FastStringBuffer(32);
            sb.append(String.valueOf(newMessageCount));
            sb.append(" new message");
            if (newMessageCount > 1)
                sb.append('s');
            status(sb.toString());
        } else
            status("No new messages");
    }

    protected void status(final String s)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                for (int i = 0; i < Editor.getFrameCount(); i++) {
                    Editor ed = Editor.getFrame(i).getCurrentEditor();
                    if (ed.getBuffer() == Mailbox.this)
                        ed.status(s);
                }
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public MailAddress getUserMailAddress()
    {
        String address = getStringProperty(Property.USER_MAIL_ADDRESS);
        if (address == null)
            return null;
        return new MailAddress(getStringProperty(Property.USER_FULL_NAME), address);
    }

    public boolean isChildVisible()
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Buffer buffer = it.nextEditor().getBuffer();
            if (buffer instanceof MessageBuffer)
                if (((MessageBuffer)buffer).getMailbox() == this)
                    return true;
        }
        return false;
    }

    // Returns true if mailbox has been idle for the specified number of
    // seconds, depending on whether it's in the foreground or background.
    public boolean isIdle(int fg, int bg)
    {
        if (!isVisible() && !isChildVisible()) {
            // Mailbox is in the background.
            if (bg == 0)
                return false;
            else if (System.currentTimeMillis() - Dispatcher.getLastEventMillis() > bg * 1000)
                return true;
            else
                return false;
        } else {
            // Mailbox (or one of its messages) is in the foreground.
            if (fg == 0)
                return false;
            else if (System.currentTimeMillis() - Dispatcher.getLastEventMillis() > fg * 1000)
                return true;
            else
                return false;
        }
    }

    protected void error(final String text, final String title)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                Editor editor = Editor.currentEditor();
                // Restore default cursor.
                setBusy(false);
                editor.updateDisplay();
                MessageDialog.showMessageDialog(editor, text, title);
            }
        };
        SwingUtilities.invokeLater(r);
    }

    protected void success(final String text)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                Editor.currentEditor().status(text);
            }
        };
        SwingUtilities.invokeLater(r);
    }

    protected void saveDisplayState()
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this)
                ed.saveView();
        }
    }

    protected void updateDisplay()
    {
        SwingUtilities.invokeLater(updateDisplayRunnable);
    }

    private Runnable updateDisplayRunnable = new Runnable() {
        public void run()
        {
            invalidate();
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == Mailbox.this) {
                    View view = ed.getView(ed.getBuffer());
                    if (view.getDotEntry() != null) {
                        try {
                            lockRead();
                        }
                        catch (InterruptedException e) {
                            Log.error(e);
                            return;
                        }
                        try {
                            Line topLine = findLineForEntry(view.getTopEntry());
                            if (topLine == null)
                                topLine = getFirstLine();
                            ed.setTopLine(topLine);
                            Line dotLine = findLineForEntry(view.getDotEntry());
                            if (dotLine != null) {
                                int offset = view.getDotOffset();
                                if (offset > dotLine.length())
                                    offset = dotLine.length();
                                ed.setDot(dotLine, offset);
                                ed.moveCaretToDotCol();
                            } else {
                                dotLine = getLastLine();
                                if (dotLine != null) {
                                    Log.debug("updateDisplayRunnable setting dotLine to last line");
                                    ed.setDot(dotLine, 0);
                                    ed.moveCaretToDotCol();
                                } else
                                    ed.setDot(null);
                            }
                            ed.setMark(null);
                        }
                        finally {
                            unlockRead();
                        }
                    } else {
                        Line firstLine = getFirstLine();
                        if (firstLine != null) {
                            ed.setDot(firstLine, 0);
                            ed.moveCaretToDotCol();
                            ed.setMark(null);
                            ed.setTopLine(firstLine);
                        } else {
                            ed.setDot(null);
                            ed.setMark(null);
                            ed.setTopLine(null);
                        }
                    }
                    ed.setUpdateFlag(REPAINT);
                    ed.updateDisplay();
                }
            }
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_CHANGED);
            Sidebar.repaintBufferListInAllFrames();
        }
    };

    protected void advanceDot(final Line dotLine)
    {
        final Line nextLine = dotLine.next();
        if (nextLine != null)
            setDotLine(nextLine);
    }

    public void setDotEntry(MailboxEntry entry)
    {
        final MailboxLine line = findLineForEntry(entry);
        if (line != null && line.getMailboxEntry() == entry)
            setDotLine(line);
    }

    private void setDotLine(final Line line)
    {
        Runnable r = new Runnable() {
            public void run()
            {
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == Mailbox.this) {
                        if (ed.getDot() != null) {
                            ed.update(ed.getDotLine());
                            ed.getDot().moveTo(line, 0);
                            ed.update(line);
                            ed.moveCaretToDotCol();
                            ed.clearStatusText();
                            ed.updateDisplay();
                        }
                    }
                }
            }
        };
        if (SwingUtilities.isEventDispatchThread())
            r.run();
        else
            SwingUtilities.invokeLater(r);
    }

    public String getStatusText(Editor editor)
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (editor.getDot() != null) {
            sb.append("Message ");
            sb.append(String.valueOf(editor.getDotLineNumber()+1));
            sb.append(" of ");
            sb.append(String.valueOf(getLineCount()));
            final int u = getUnreadMessageCount();
            if (u > 0) {
                final int n = getNewMessageCount();
                sb.append(" (");
                if (n > 0) {
                    sb.append(n);
                    sb.append(" new, ");
                }
                sb.append(u);
                sb.append(" unread)");
            }
        } else {
            sb.append("No messages");
        }
        return sb.toString();
    }

    public void saveView(Editor editor)
    {
        final View view = saveViewInternal(editor);
        final Line topLine = editor.getTopLine();
        if (topLine instanceof MailboxLine)
            view.setTopEntry(((MailboxLine)topLine).getMailboxEntry());
        if (editor.getDot() != null) {
            Line dotLine = editor.getDotLine();
            if (dotLine instanceof MailboxLine)
                view.setDotEntry(((MailboxLine)dotLine).getMailboxEntry());
        }
        editor.setView(this, view);
        setLastView(view);
    }

    public void restoreView(Editor editor)
    {
        final Display display = editor.getDisplay();
        final View view = editor.getView(this);
        Debug.assertTrue(view != null);
        if (view != null) {
            if (view.getDotEntry() == null) {
                super.restoreView(editor);
                return;
            }
            Line topLine = findLineForEntry(view.getTopEntry());
            if (topLine == null)
                topLine = getFirstLine();
            display.setTopLine(topLine);
            Line dotLine = findLineForEntry(view.getDotEntry());
            if (dotLine == null)
                dotLine = getFirstLine();
            if (view.getTopLine() == topLine && view.getDot() != null && view.getDot().getLine() == dotLine) {
                editor.setDot(new Position(view.getDot()));
                editor.setMark(view.getMark() == null ? null : new Position(view.getMark()));
                editor.setSelection(view.getSelection());
                display.setTopLine(view.getTopLine());
                display.setShift(view.getShift());
                display.setCaretCol(view.getCaretCol());
            } else {
                editor.setDot(dotLine != null ? new Position(dotLine, 0) : null);
                editor.moveCaretToDotCol();
                editor.setMark(null);
            }
        }
    }

    protected void notImplemented(String s)
    {
        Log.error(s.concat(" is not implemented"));
    }
}
