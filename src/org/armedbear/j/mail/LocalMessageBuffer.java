/*
 * LocalMessageBuffer.java
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

import org.armedbear.j.Editor;

/*package*/ final class LocalMessageBuffer extends MessageBuffer
{
    /*package*/ LocalMessageBuffer(LocalMailbox mailbox, MailboxEntry entry)
    {
        super();
        this.mailbox = mailbox;
        showFullHeaders = mailbox.showFullHeaders;
        showRawText = mailbox.showRawText;
        setEntry(entry);
        initializeUndo();
        type = TYPE_NORMAL;
        lineSeparator = "\n";
        mode = MessageMode.getMode();
        formatter = mode.getFormatter(this);
        readOnly = true;
    }

    public int load()
    {
        if (mailbox.lock()) {
            try {
                loadMessage(null);
                setLoaded(true);
                return LOAD_COMPLETED;
            }
            finally {
                mailbox.unlock();
            }
        } else
            Editor.currentEditor().status("Mailbox is locked");
        return LOAD_FAILED;
    }

    public void deleteMessage()
    {
        if (mailbox.lock()) {
            try {
                if (!entry.isDeleted()) {
                    entry.setFlags(entry.getFlags() | MailboxEntry.DELETED);
                    mailbox.setDirty(true);
                    mailbox.updateEntry(entry);
                }
                MailboxEntry nextEntry = mailbox.getNextUndeleted(entry);
                if (nextEntry != null) {
                    setEntry(nextEntry);
                    loadMessage(null);
                } else {
                    Editor editor = Editor.currentEditor();
                    MailCommands.messageIndex(editor);
                    editor.status("Last undeleted message");
                }
            }
            finally {
                mailbox.unlock();
            }
        }
    }

    public void flagMessage()
    {
        if (mailbox.lock()) {
            try {
                if (entry.isFlagged())
                    entry.unflag();
                else
                    entry.flag();
                mailbox.setDirty(true);
                mailbox.updateEntry(entry);
                MailboxEntry nextEntry = mailbox.getNextUndeleted(entry);
                if (nextEntry != null) {
                    setEntry(nextEntry);
                    loadMessage(null);
                } else {
                    Editor editor = Editor.currentEditor();
                    MailCommands.messageIndex(editor);
                    editor.status("Last undeleted message");
                }
            }
            finally {
                mailbox.unlock();
            }
        }
    }
}
