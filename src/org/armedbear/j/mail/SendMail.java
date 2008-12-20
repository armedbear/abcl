/*
 * SendMail.java
 *
 * Copyright (C) 2000-2007 Peter Graves
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
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA.
 */

package org.armedbear.j.mail;

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.FileInputStream;
import java.io.FileWriter;
import java.io.IOException;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.Writer;
import java.text.SimpleDateFormat;
import java.util.ArrayList;
import java.util.Calendar;
import java.util.List;
import java.util.Random;
import javax.swing.Icon;
import javax.swing.SwingUtilities;
import javax.swing.undo.CompoundEdit;
import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.Debug;
import org.armedbear.j.Directories;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.Expansion;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.Headers;
import org.armedbear.j.Line;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.OpenFileDialog;
import org.armedbear.j.Platform;
import org.armedbear.j.Position;
import org.armedbear.j.Preferences;
import org.armedbear.j.Property;
import org.armedbear.j.Region;
import org.armedbear.j.Sidebar;
import org.armedbear.j.SimpleEdit;
import org.armedbear.j.Utilities;
import org.armedbear.j.Version;

public final class SendMail extends Buffer
{
    private final static String HEADER_SEPARATOR = "--text follows this line--";
    private final static String DEFAULT_TITLE = "Compose";

    private static Preferences preferences;

    private boolean reply;
    private List group;
    private Mailbox mailbox;
    private MailboxEntry entryRepliedTo;
    private String boundary;
    private String smtp;
    private SmtpSession session;
    private boolean hasBeenSent;

    public SendMail()
    {
        super();
        init();
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            appendFrom();
            appendLine("To: ");
            appendLine("Subject: ");
            appendDefaultHeaders();
            appendLine(HEADER_SEPARATOR);
            appendLine("");
            appendSignature();
            renumber();
            formatter.parseBuffer();
            setLoaded(true);
        }
        finally {
            unlockWrite();
        }
    }

    // Re-opening an unsent message.
    public SendMail(File file)
    {
        super();
        setFile(file);
        init();
    }

    // Forward.
    public SendMail(MessageBuffer messageBuffer)
    {
        super();
        init();
        mailbox = messageBuffer.getMailbox();
        entryRepliedTo = messageBuffer.getMailboxEntry();
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            appendFrom();
            appendLine("To: ");
            appendLine("Subject: [Fwd: " + entryRepliedTo.getSubject() + "]");
            appendDefaultHeaders();
            appendLine(HEADER_SEPARATOR);
            appendLine("");
            Position pos = new Position(getLastLine(), 0);
            insertString(pos, "----- Forwarded message -----\n\n");
            insertString(pos, messageBuffer.getText());
            insertString(pos, "\n\n----- End of forwarded message -----");
            unmodified();
            renumber();
            formatter.parseBuffer();
            setLoaded(true);
        }
        finally {
            unlockWrite();
        }
    }

    // Reply.
    public SendMail(MessageBuffer messageBuffer, boolean replyToGroup)
    {
        super();
        init();
        mailbox = messageBuffer.getMailbox();
        entryRepliedTo = messageBuffer.getMailboxEntry();
        reply = true;
        MailAddress[] replyTo = entryRepliedTo.getReplyTo();
        MailAddress[] from = entryRepliedTo.getFrom();
        MailAddress[] to = entryRepliedTo.getTo();
        MailAddress[] cc = entryRepliedTo.getCc();
        boolean skipTo = false;
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            appendFrom();
            // Get senders.
            List senders = null;
            if (replyTo != null && replyTo.length > 0) {
                senders = new ArrayList();
                for (int i = 0; i < replyTo.length; i++)
                    senders.add(replyTo[i]);
            } else if (from != null && from.length > 0) {
                senders = new ArrayList();
                for (int i = 0; i < from.length; i++)
                    senders.add(from[i]);
            }
            if (senders != null) {
                Debug.assertTrue(senders.size() > 0);
                // Remove user's address from list of senders.
                for (int i = senders.size(); i-- > 0;) {
                    MailAddress a = (MailAddress) senders.get(i);
                    if (a.addressMatches(Mail.getUserMailAddress()))
                        senders.remove(i);
                }
                if (senders.size() == 0) {
                    // User was the only sender of the original message.
                    // Use "To" addresses instead.
                    for (int i = 0; i < to.length; i++)
                        senders.add(to[i]);
                    // Don't add "To" addresses to group.
                    skipTo = true;
                }
                removeDuplicateAddresses(senders);
                appendAddressHeader("To: ", senders);
            }
            // Gather addresses for reply to group.
            group = new ArrayList();
            if (!skipTo && to != null) {
                for (int i = 0;  i < to.length; i++) {
                    MailAddress a = to[i];
                    if (!a.addressMatches(Mail.getUserMailAddress()))
                        group.add(a);
                }
            }
            if (cc != null) {
                for (int i = 0;  i < cc.length; i++) {
                    MailAddress a = cc[i];
                    if (!a.addressMatches(Mail.getUserMailAddress()))
                        group.add(a);
                }
            }
            removeDuplicateAddresses(group);
            if (senders != null) {
                // Make sure we don't duplicate entries in the "To:" header.
                for (int i = senders.size(); i-- > 0;) {
                    MailAddress toAddress = (MailAddress) senders.get(i);
                    for (int j = group.size(); j-- > 0;) {
                        MailAddress a = (MailAddress) group.get(j);
                        if (a.equals(toAddress)) {
                            // It's a duplicate. Remove it from the group.
                            group.remove(j);
                            break;
                        }
                    }
                }
            }
            // Add group to recipients if applicable.
            if (replyToGroup && group.size() > 0)
                appendAddressHeader("Cc: ", group);
            String subject = entryRepliedTo.getSubject();
            if (!subject.toLowerCase().startsWith("re:"))
                subject = "Re: ".concat(subject);
            appendLine("Subject: ".concat(subject));
            appendLine("In-Reply-To: " + entryRepliedTo.getMessageId());
            appendReferences();
            appendDefaultHeaders();
            appendLine(HEADER_SEPARATOR);
            String attribution = getAttribution(messageBuffer);
            if (attribution != null)
                appendLine(attribution);
            String s =
                messageBuffer.quoteBody(getIntegerProperty(Property.WRAP_COL));
            if (s != null)
                append(s);
            appendLine("");
            appendSignature();
            unmodified();
            renumber();
            formatter.parseBuffer();
            setLoaded(true);
        }
        finally {
            unlockWrite();
        }
    }

    private void init()
    {
        if (preferences == null)
            preferences = Editor.preferences();
        initializeUndo();
        type = TYPE_NORMAL;
        title = DEFAULT_TITLE;
        if (getFile() == null) {
            final File dir = Directories.getDraftsFolder();
            if (!dir.isDirectory())
                dir.mkdirs();
            if (dir.isDirectory()) {
                setFile(Utilities.getTempFile(dir));
            } else {
                // Use temp directory as fallback. (Shouldn't happen.)
                setFile(Utilities.getTempFile());
            }
        }
        autosaveEnabled = true;
        mode = SendMailMode.getMode();
        formatter = mode.getFormatter(this);
        if (!getFile().isFile())
            lineSeparator = "\n";
        setInitialized(true);
    }

    public int load()
    {
        super.load();
        title = getSubject();
        if (title == null || title.length() == 0)
            title = DEFAULT_TITLE;
        return LOAD_COMPLETED;
    }

    public boolean save()
    {
        boolean result = super.save();
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer buf = it.nextBuffer();
            if (buf instanceof Drafts) {
                Drafts drafts = (Drafts) buf;
                drafts.reload();
                break;
            }
        }
        return result;
    }

    public boolean hasBeenSent()
    {
        return hasBeenSent;
    }

    private static void removeDuplicateAddresses(List list)
    {
        // Remove duplicate entries from list.
        for (int i = list.size(); i-- > 0;) {
            MailAddress ma = (MailAddress) list.get(i);
            String addr = ma.getAddress();
            for (int j = i-1; j >= 0; j--) {
                MailAddress ma2 = (MailAddress) list.get(j);
                if (ma.equals(ma2)) {
                    list.remove(i);
                    break;
                }
                // Not strictly equal. Check for same address.
                String addr2 = ma2.getAddress();
                if (addr.equals(addr2)) {
                    if (i < list.size())
                        list.remove(i);
                    // We've removed ma from the list.
                    // Don't lose any extra information it may have.
                    if (ma2.getPersonal() == null || ma2.getPersonal().length() == 0) {
                        if (ma.getPersonal() != null && ma.getPersonal().length() > 0)
                            list.set(j, ma);
                    }
                }
            }
        }
    }

    private void appendAddressHeader(String prefix, List list)
    {
        if (list == null)
            return;
        if (list.size() == 0)
            return;
        append(MailUtilities.constructAddressHeader(prefix, list));
    }

    private void appendFrom()
    {
        if (!preferences.getBooleanProperty(Property.CONFIRM_SEND)) {
            MailAddress ma = Mail.getUserMailAddress();
            if (ma != null)
                appendLine("From: ".concat(ma.toString()));
        }
    }

    private void replaceFrom(String from)
    {
        final Editor editor = Editor.currentEditor();
        final Position savedDot = editor.getDotCopy();
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            // Remove existing "From:" header(s).
            removeHeaders(editor, "from:");
            // Move dot to beginning of buffer.
            editor.addUndo(SimpleEdit.MOVE);
            editor.getDot().moveTo(getFirstLine(), 0);
            // Insert new "From:" header.
            editor.addUndo(SimpleEdit.INSERT_STRING);
            insertString(editor.getDot(), "From: ".concat(from).concat("\n"));
            // Restore dot to saved position if possible.
            Line dotLine = savedDot.getLine();
            if (contains(dotLine)) {
                int dotOffset = savedDot.getOffset();
                if (dotOffset > dotLine.length())
                    dotOffset = dotLine.length();
                editor.addUndo(SimpleEdit.MOVE);
                editor.getDot().moveTo(dotLine, dotOffset);
            }
            editor.addUndo(SimpleEdit.MOVE);
            editor.moveCaretToDotCol();
            editor.endCompoundEdit(compoundEdit);
            getFormatter().parseBuffer();
        }
        finally {
            unlockWrite();
        }
        repaint();
    }

    private void replaceBcc(List bccList)
    {
        final Editor editor = Editor.currentEditor();
        final Position savedDot = editor.getDotCopy();
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            // Remove existing "Bcc:" header(s).
            removeHeaders(editor, "bcc:");
            // Move dot to end of header area and insert new "Bcc:" header.
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                if (line.getText().equals(HEADER_SEPARATOR)) {
                    editor.addUndo(SimpleEdit.MOVE);
                    editor.getDot().moveTo(line, 0);
                    editor.addUndo(SimpleEdit.INSERT_STRING);
                    insertString(editor.getDot(),
                        MailUtilities.constructAddressHeader("Bcc: ", bccList).concat("\n"));
                    break;
                }
            }
            // Restore dot to saved position.
            Line dotLine = savedDot.getLine();
            if (contains(dotLine)) {
                int dotOffset = savedDot.getOffset();
                if (dotOffset > dotLine.length())
                    dotOffset = dotLine.length();
                editor.addUndo(SimpleEdit.MOVE);
                editor.getDot().moveTo(dotLine, dotOffset);
            }
            editor.addUndo(SimpleEdit.MOVE);
            editor.moveCaretToDotCol();
            editor.endCompoundEdit(compoundEdit);
            getFormatter().parseBuffer();
        }
        finally {
            unlockWrite();
        }
        repaint();
    }

    private void appendReferences()
    {
        if (entryRepliedTo != null) {
            final String prefix = "References: ";
            FastStringBuffer sb = new FastStringBuffer(prefix);
            int length = prefix.length();
            String[] oldReferences = entryRepliedTo.getReferences();
            final String[] references;
            if (oldReferences != null) {
                references = new String[oldReferences.length+1];
                System.arraycopy(oldReferences, 0, references, 0, oldReferences.length);
            } else
                references = new String[1];
            references[references.length-1] = entryRepliedTo.getMessageId();
            for (int i = 0; i < references.length; i++) {
                String s = references[i];
                if (i > 0 && length + s.length() > 990) {
                    // Won't fit on current line.
                    sb.append(lineSeparator);
                    final String indent = "        ";
                    sb.append(indent); // Continuation.
                    sb.append(s);
                    length = indent.length() + s.length();
                } else {
                    if (i > 0) {
                        sb.append(" ");
                        length++;
                    }
                    sb.append(s);
                    length += s.length();
                }
            }
            append(sb.toString());
        }
    }

    private void appendDefaultHeaders()
    {
        String replyTo = preferences.getStringProperty("replyTo");
        if (replyTo != null)
            appendLine("Reply-To: ".concat(replyTo));
        String bcc = preferences.getStringProperty("bcc");
        if (bcc != null)
            appendLine("Bcc: ".concat(bcc));
    }

    private void appendSignature()
    {
        File file = null;
        String fileName = preferences.getStringProperty(Property.SIGNATURE);
        if (fileName != null)
            file = File.getInstance(fileName);
        else if (Platform.isPlatformUnix())
            file = File.getInstance(Directories.getUserHomeDirectory(),
                ".signature");
        if (file == null || !file.isFile())
            return;
        FastStringBuffer sb = new FastStringBuffer();
        try {
            BufferedReader reader =
                new BufferedReader(new InputStreamReader(file.getInputStream()));
            while (true) {
                String s = reader.readLine();
                if (s == null)
                    break;
                if (sb.length() > 0)
                    sb.append('\n');
                sb.append(s);
            }
            reader.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
        if (sb.length() > 0)
            append(sb.toString());
    }

    public static final String getHeaderSeparator()
    {
        return HEADER_SEPARATOR;
    }

    public void modified()
    {
        super.modified();
        setTitle();
    }

    private void setTitle()
    {
        String s = getSubject();
        if (s == null || s.length() == 0)
            s = DEFAULT_TITLE;
        if (title == null || !title.equals(s)) {
            title = s;
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_CHANGED);
        }
    }

    public void attachFile()
    {
        Editor editor = Editor.currentEditor();
        File file = OpenFileDialog.getLocalFile(editor, "Attach File");
        if (file != null && file.isFile()) {
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                if (line.getText().equals(HEADER_SEPARATOR)) {
                    Position pos = new Position(line, 0);
                    insertString(pos,
                        "Attachment: " + file.canonicalPath() + "\n");
                    break;
                }
            }
        }
    }

    public void send()
    {
        if (!checkHeader())
            return;
        checkRecipients();
        checkEmpty();
        if (preferences.getBooleanProperty(Property.CONFIRM_SEND)) {
            if (!confirmSend())
                return;
        }
        Runnable sendRunnable = new Runnable() {
            public void run()
            {
                boolean succeeded = false;
                if (smtp != null)
                    session = SmtpSession.getSession(smtp);
                else
                    session = SmtpSession.getDefaultSession();
                if (session != null) {
                    File messageFile = Utilities.getTempFile();
                    try {
                        OutputStreamWriter writer =
                            new OutputStreamWriter(messageFile.getOutputStream());
                        writeMessageText(writer);
                        writer.flush();
                        writer.close();
                        succeeded = session.sendMessage(SendMail.this,
                            messageFile);
                        if (succeeded)
                            writeFcc(messageFile);
                        messageFile.delete();
                    }
                    catch (IOException e) {
                        Log.error(e);
                    }
                }
                hasBeenSent = succeeded;
                SwingUtilities.invokeLater(succeeded ? succeededRunnable :
                    errorRunnable);
            }
        };
        setBusy(true);
        new Thread(sendRunnable).start();
    }

    private boolean confirmSend()
    {
        final Editor editor = Editor.currentEditor();
        ConfirmSendDialog d = new ConfirmSendDialog(editor, this);
        editor.centerDialog(d);
        d.show();
        if (d.cancelled())
            return false;
        String from = d.getFrom();
        if (from != null)
            replaceFrom(from);
        if (d.bccAddSender() || d.bccAddOther()) {
            List bccList = new ArrayList();
            MailAddress[] bcc = MailAddress.parseAddresses(getBcc());
            if (bcc != null) {
                for (int i = 0; i < bcc.length; i++)
                    bccList.add(bcc[i]);
            }
            if (d.bccAddSender() && from != null)
                bccList.add(MailAddress.parseAddress(from));
            if (d.bccAddOther()) {
                String bccOther = d.getBccOther();
                if (bccOther != null && bccOther.length() > 0)
                    bccList.add(MailAddress.parseAddress(bccOther));
            }
            if (bccList.size() > 0) {
                removeDuplicateAddresses(bccList);
                replaceBcc(bccList);
            }
        }
        smtp = d.getSmtp();
        return true;
    }

    private boolean checkHeader()
    {
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line.getText().equals(HEADER_SEPARATOR))
                return true;
        }
        MessageDialog.showMessageDialog(Editor.currentEditor(),
            "Message separator line is missing", "Error");
        return false;
        // BUG!! Should confirm here that we have a valid "From" address!
    }

    // Make sure all continued address header lines end with commas.
    private void checkRecipients()
    {
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            String text = line.getText();
            if (text.equals(HEADER_SEPARATOR))
                return;
            String lower = text.toLowerCase();
            if (lower.startsWith("to:") || lower.startsWith("cc:") ||
                lower.startsWith("bcc:")) {
                Line continuation = line.next();
                while (continuation != null && continuation.length() > 0) {
                    char c = continuation.charAt(0);
                    if (c == ' ' || c == '\t') {
                        // It's really a continuation line. Make sure the
                        // preceding line ends with a comma.
                        String s = trimTrailing(line.getText());
                        int length = s.length();
                        if (length > 0 && s.charAt(length - 1) != ',')
                            line.setText(s.concat(","));
                        line = continuation;
                        continuation = continuation.next();
                    } else
                        break;
                }
            }
        }
    }

    // Trims trailing whitespace.
    private static String trimTrailing(String s)
    {
        int length = s.length();
        if (length == 0 || !Character.isWhitespace(s.charAt(length - 1)))
            return s;
        do {
            --length;
        } while (length > 0 && Character.isWhitespace(s.charAt(length - 1)));
        return s.substring(0, length);
    }

    private void checkEmpty()
    {
        String eom = getStringProperty(Property.EOM);
        if (eom == null || eom.length() == 0)
            return;
        Line subjectLine = null;
        Line line;
        for (line = getFirstLine(); line != null; line = line.next()) {
            String text = line.getText();
            if (text.toLowerCase().startsWith("subject:"))
                subjectLine = line;
            else if (text.toLowerCase().startsWith("attachment:"))
                return; // Not empty.
            else if (text.equals(HEADER_SEPARATOR))
                break;
        }
        if (subjectLine == null)
            return;
        if (line != null) {
            line = line.next();
            // Now we're on the first line of the message body.
            while (line != null) {
                if (!line.isBlank())
                    return; // Not empty.
                line = line.next();
            }
        }
        // Empty.
        String text = subjectLine.getText();
        if (!text.endsWith(eom))
            subjectLine.setText(text + eom);
    }

    private Runnable succeededRunnable = new Runnable() {
        public void run()
        {
            unmodified();
            if (reply && mailbox != null && entryRepliedTo != null)
                mailbox.setAnsweredFlag(entryRepliedTo);
            File file = getFile();
            if (file.isFile()) {
                Log.debug("deleting draft " + file);
                file.delete();
                for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                    Buffer buf = it.nextBuffer();
                    if (buf instanceof Drafts) {
                        Drafts drafts = (Drafts) buf;
                        drafts.reload();
                        break;
                    }
                }
            }
            setBusy(false);
            kill();
            EditorIterator iter = new EditorIterator();
            while (iter.hasNext())
                iter.nextEditor().updateDisplay();
        }
    };

    private Runnable errorRunnable = new Runnable() {
        public void run()
        {
            setBusy(false);
            final Editor editor = Editor.currentEditor();
            editor.updateDisplay();
            MessageDialog.showMessageDialog(editor,
                session != null ? session.getErrorText() : "Unable to send message",
                "Send Mail");
        }
    };

    private void writeMessageText(Writer writer)
    {
        final String separator = "\n";
        try {
            writer.write("Date: " + RFC822Date.getDateTimeString() + separator);
            if (getFrom() == null)
                writer.write("From: " + getDefaultFromAddress() + separator);
            Line line;
            // Headers.
            boolean inBcc = false;
            List attachments = null;
            for (line = getFirstLine(); line != null; line = line.next()) {
                String text = line.getText();
                if (text.length() == 0) {
                    // Found empty line before reaching end of headers.
                    // Discard it.
                    continue;
                }
                if (text.equals(HEADER_SEPARATOR)) {
                    // Reached end of headers.
                    writer.write("Message-ID: " + Mail.generateMessageId() + separator);
                    writer.write("User-Agent: " + Version.getLongVersionString() + separator);
                    attachments = parseAttachments();
                    if (attachments != null)
                        writer.write(generateMimeHeaders(separator));
                    // Skip header separator line.
                    line = line.next();
                    break;
                }
                if (inBcc) {
                    char c = text.charAt(0);
                    if (c == ' ' || c == '\t') {
                        // It's a continuation line. Skip it.
                        continue;
                    } else {
                        // Start of next header.
                        inBcc = false;
                    }
                }
                if (text.toLowerCase().startsWith("bcc:")) {
                    inBcc = true;
                    continue;
                }
                if (text.toLowerCase().startsWith("attachment:"))
                    continue;
                writer.write(line.getText());
                writer.write(separator);
            }
            final Line startOfBody = line;
            // Scan body of message.
            boolean qp = false;
            for (line = startOfBody; line != null; line = line.next()) {
                if (requiresEncoding(line)) {
                    qp = true;
                    break;
                }
            }
            String transferEncoding = qp ? "quoted-printable" : "7bit";
            String characterEncoding =
                getStringProperty(Property.DEFAULT_ENCODING);
            if (attachments != null) {
                // Append extra separator at end of headers.
                writer.write(separator);
                writer.write("This is a multi-part message in MIME format.");
                writer.write(separator);
                writer.write("--");
                writer.write(getBoundary());
                writer.write(separator);
                writer.write("Content-Type: text/plain");
                if (qp) {
                    writer.write("; charset=");
                    writer.write(getCharSetName(characterEncoding));
                }
                writer.write(separator);
                writer.write("Content-Transfer-Encoding: ");
                writer.write(transferEncoding);
                writer.write(separator);
                writer.write(separator);
            } else {
                // No attachments.
                if (qp) {
                    writer.write("Content-Type: text/plain");
                    if (qp) {
                        writer.write("; charset=");
                        writer.write(getCharSetName(characterEncoding));
                    }
                    writer.write(separator);
                    writer.write("Content-Transfer-Encoding: ");
                    writer.write(transferEncoding);
                    writer.write(separator);
                }
                // Append extra separator at end of headers.
                writer.write(separator);
            }
            // Body.
            for (line = startOfBody; line != null; line = line.next()) {
                String s = line.getText();
                if (qp) {
                    writer.write(QuotedPrintableEncoder.encode(s,
                        characterEncoding, separator));
                } else {
                    // Dot stuffing.
                    if (s.length() > 0 && s.charAt(0) == '.')
                        writer.write('.');
                    writer.write(s);
                }
                writer.write(separator);
            }
            // Attachments.
            if (attachments != null) {
                for (int i = 0; i < attachments.size(); i++) {
                    String fullPath = (String) attachments.get(i);
                    File file = File.getInstance(fullPath);
                    String contentType = getContentTypeForFile(file);
                    Log.debug("contentType = " + contentType);
                    writer.write("--");
                    writer.write(getBoundary());
                    writer.write(separator);
                    writer.write("Content-Type: ");
                    writer.write(contentType);
                    writer.write(separator);
                    writer.write("Content-Transfer-Encoding: base64");
                    writer.write(separator);
                    writer.write("Content-Disposition: attachment; filename=\"");
                    writer.write(file.getName());
                    writer.write('"');
                    writer.write(separator);
                    writer.write(separator);
                    writeEncodedFile(file, writer, separator);
                    writer.write(separator);
                }
                writer.write("--");
                writer.write(getBoundary());
                writer.write("--");
                writer.write(separator);
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    private void writeEncodedFile(File file, Writer writer, String separator)
    {
        if (file == null || !file.isFile() || !file.canRead())
            return;
        try {
            FileInputStream inputStream = file.getInputStream();
            Base64Encoder encoder = new Base64Encoder(inputStream);
            String s;
            while ((s = encoder.encodeLine()) != null) {
                writer.write(s);
                writer.write(separator);
            }
            writer.flush();
            inputStream.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public String getFrom()
    {
        return getHeaderValue("from");
    }

    public String getFromAddress()
    {
        String from = getFrom();
        if (from != null)
            return getAddress(from);
        // No "From:" header.
        return Mail.getUserMailAddress().getAddress();
    }

    private final String getDefaultFromAddress()
    {
        return Mail.getUserMailAddress().toString();
    }

    // Given "Piso Mojado <piso@armedbear.yi.org>", returns
    // "piso@armedbear.yi.org".
    public static String getAddress(String s)
    {
        if (s == null)
            return null;
        int index = s.indexOf('@');
        if (index < 0)
            return null;
        int begin = 0;
        int end = s.length();
        for (int i = index; i-- > 0;) {
            char c = s.charAt(i);
            if (c == ',' || c == '"' || c == '<' || Character.isWhitespace(c)) {
                begin = i + 1;
                break;
            }
        }
        for (int i = index + 1; i < end; i++) {
            char c = s.charAt(i);
            if (c == ',' || c == '"' || c == '>' || Character.isWhitespace(c)) {
                end = i;
                break;
            }
        }
        return s.substring(begin, end);
    }

    public String getTo()
    {
        Log.debug("getTo to = |" + getHeaderValue("to") + "|");
        return getHeaderValue("to");
    }

    public String getCc()
    {
        Log.debug("getCc cc = |" + getHeaderValue("cc") + "|");
        return getHeaderValue("cc");
    }

    public String getBcc()
    {
        Log.debug("getBcc bcc = |" + getHeaderValue("bcc") + "|");
        return getHeaderValue("bcc");
    }

    public void ccGroup()
    {
        if (group == null)
            return;
        // Entries from the original group will come first in the new list.
        List newList = new ArrayList(group);
        // Add the entries from the existing "Cc:" header (if any) back in.
        MailAddress[] cc = MailAddress.parseAddresses(getCc());
        if (cc != null) {
            for (int i = 0; i < cc.length; i++) {
                MailAddress oldAddress = cc[i];
                // Skip entries that are already in the list.
                boolean isDuplicate = false;
                for (int j = newList.size()-1; j >= 0; j--) {
                    MailAddress a = (MailAddress) newList.get(j);
                    if (oldAddress.equals(a)) {
                        isDuplicate = true;
                        break;
                    }
                }
                if (!isDuplicate)
                    newList.add(oldAddress);
            }
        }
        // Make sure we don't duplicate entries in the "To:" header.
        MailAddress[] to = MailAddress.parseAddresses(getTo());
        if (to != null) {
            for (int i = to.length-1; i >= 0; i--) {
                MailAddress toAddress = to[i];
                for (int j = newList.size()-1; j >= 0; j--) {
                    MailAddress a = (MailAddress) newList.get(j);
                    if (a.equals(toAddress)) {
                        // It's a duplicate. Remove it from the new list.
                        Log.debug("removing " + (MailAddress)newList.get(j));
                        newList.remove(j);
                        break;
                    }
                }
            }
        }
        final Editor editor = Editor.currentEditor();
        final Position savedDot = editor.getDotCopy();
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            // Remove all existing "Cc:" headers (there can be more than one).
            removeHeaders(editor, "cc:");
            // Move dot to line after "To:" header.
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                if (line.getText().toLowerCase().startsWith("to:")) {
                    // Found first line of "To:" header.
                    for (Line next = line.next(); next != null; next = next.next()) {
                        if (next.length() == 0 || next.charAt(0) == ' ' ||
                            next.charAt(0) == '\t') {
                            // Continuation line.
                            continue;
                        } else {
                            // Found next header. Put dot here.
                            editor.addUndo(SimpleEdit.MOVE);
                            editor.getDot().moveTo(next, 0);
                            break;
                        }
                    }
                    break;
                }
            }
            // Insert new "Cc:" header.
            editor.addUndo(SimpleEdit.INSERT_STRING);
            FastStringBuffer sb = new FastStringBuffer();
            sb.append(MailUtilities.constructAddressHeader("Cc: ", newList));
            sb.append(lineSeparator);
            insertString(editor.getDot(), sb.toString());
            // Restore dot to saved position if possible.
            Line dotLine = savedDot.getLine();
            if (contains(dotLine)) {
                int dotOffset = savedDot.getOffset();
                if (dotOffset > dotLine.length())
                    dotOffset = dotLine.length();
                editor.addUndo(SimpleEdit.MOVE);
                editor.getDot().moveTo(dotLine, dotOffset);
            }
            editor.addUndo(SimpleEdit.MOVE);
            editor.moveCaretToDotCol();
            editor.endCompoundEdit(compoundEdit);
            getFormatter().parseBuffer();
        }
        finally {
            unlockWrite();
        }
        repaint();
    }

    // Remove all header lines for hdr (e.g "from:", "cc:", "bcc:"). We assume
    // the buffer is write-locked and beginCompoundEdit has been called. The
    // current editor gets passed in to manage undo.
    private void removeHeaders(Editor editor, String hdr)
    {
        // Make sure hdr is all lower case and ends with a colon.
        hdr = hdr.toLowerCase();
        if (!hdr.endsWith(":"))
            hdr = hdr.concat(":");
        while (true) {
            // Find appropriate line in message headers.
            Line beginLine = null;
            // Always start at top of buffer.
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                String text = line.getText();
                if (text.equals(HEADER_SEPARATOR))
                    return;
                if (text.toLowerCase().startsWith(hdr)) {
                    beginLine = line;
                    break;
                }
            }
            // Not found.
            if (beginLine == null)
                return;
            // We want to delete the region up to the start of the next header
            // line.
            Line endLine = null;
            for (Line line = beginLine.next(); line != null; line = line.next()) {
                String text = line.getText();
                if (text.length() == 0)
                    continue;
                if (text.equals(HEADER_SEPARATOR)) {
                    endLine = line;
                    break;
                }
                char c = line.getText().charAt(0);
                if (c != ' ' && c != '\t') {
                    endLine = line;
                    break;
                }
            }
            // endLine should never be null here, since at worst we should
            // always hit the header separator line. But the user might have
            // deleted the header separator...
            if (endLine == null)
                return;
            // Delete the region from beginLine to endLine.
            Region r = new Region(this, new Position(beginLine, 0),
                new Position(endLine, 0));
            editor.addUndo(SimpleEdit.MOVE);
            editor.getDot().moveTo(r.getBegin());
            editor.addUndoDeleteRegion(r);
            r.delete();
        }
    }

    public List getAddressees()
    {
        ArrayList list = new ArrayList();
        appendAddressesFromString(list, getTo());
        appendAddressesFromString(list, getCc());
        appendAddressesFromString(list, getBcc());
        for (int i = 0; i < list.size(); i++)
            Log.debug("|" + (String) list.get(i) + "|");
        return list;
    }

    private static void appendAddressesFromString(List list, String s)
    {
        if (s == null)
            return;
        s = s.trim();
        int length = s.length();
        if (length == 0)
            return;
        FastStringBuffer sb = new FastStringBuffer(64);
        boolean inQuote = false;
        for (int i = 0; i < length; i++) {
            char c = s.charAt(i);
            switch (c) {
                case '"':
                    inQuote = !inQuote;
                    sb.append(c);
                    break;
                case ',':
                    if (inQuote) {
                        // A comma inside a quoted string is just an ordinary
                        // character.
                        sb.append(c);
                    } else {
                        // Otherwise a comma marks the end of the address.
                        String address = sb.toString().trim();
                        if (address.length() > 0)
                            list.add(address);
                        sb.setLength(0);
                    }
                    break;
                default:
                    sb.append(c);
                    break;
            }
        }
        if (sb.length() > 0) {
            String address = sb.toString().trim();
            if (address.length() > 0)
                list.add(address);
        }
    }

    public String getSubject()
    {
        return getHeaderValue("subject");
    }

    // Combines multiple occurrences of "To:", "Cc:", "Bcc:".
    private String getHeaderValue(String headerName)
    {
        boolean combine = false;
        String key = headerName.toLowerCase() + ':';
        if (key.equals("to:") || key.equals("cc:") || key.equals("bcc:"))
            combine = true;
        FastStringBuffer sb = null;
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            String text = line.getText();
            if (text.equals(HEADER_SEPARATOR))
                break;
            if (text.toLowerCase().startsWith(key)) {
                if (sb == null) {
                    sb = new FastStringBuffer();
                } else {
                    // Make sure there's a proper separator when we're
                    // combining address headers.
                    sb.append(", ");
                }
                sb.append(text.substring(key.length()).trim());
                Line continuation = line.next();
                while (continuation != null && continuation.length() > 0) {
                    char c = continuation.charAt(0);
                    if (c == ' ' || c == '\t') {
                        // This is in fact a continuation line.
                        sb.append(continuation.getText());
                        line = continuation;
                        continuation = continuation.next();
                        continue;
                    } else
                        break;
                }
                if (!combine)
                    break;
            }
        }
        return sb != null ? sb.toString() : null;
    }

    public Position getInitialDotPos()
    {
        if (reply) {
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                if (line.getText().equals(HEADER_SEPARATOR)) {
                    line = line.next();
                    if (line != null)
                        return new Position(line, 0);
                }
            }
        } else {
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                if (line.getText().startsWith("To: "))
                    return new Position(line, line.length());
            }
        }
        // Under normal circumstances we shouldn't get here, but anything can
        // happen with a postponed message...
        return new Position(getFirstLine(), 0);
    }

    private List parseAttachments()
    {
        ArrayList attachments = null;
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line.getText().equals(HEADER_SEPARATOR))
                break;
            if (line.getText().toLowerCase().startsWith("attachment:")) {
                String filename = line.getText().substring(11).trim();
                if (filename.length() > 0) {
                    if (attachments == null)
                        attachments = new ArrayList();
                    attachments.add(filename);
                }
            }
        }
        return attachments;
    }

    private String generateMimeHeaders(String separator)
    {
        FastStringBuffer sb = new FastStringBuffer();
        sb.append("MIME-Version: 1.0");
        sb.append(separator);
        sb.append("Content-Type: multipart/mixed; boundary=\"");
        sb.append(getBoundary());
        sb.append('"');
        sb.append(separator);
        return sb.toString();
    }

    private String getBoundary()
    {
        if (boundary == null) {
            FastStringBuffer sb = new FastStringBuffer(16);
            Random random = new Random();
            char[] chars = getBoundaryChars();
            for (int i = 0; i < 16; i++) {
                int index = random.nextInt(chars.length);
                sb.append(chars[index]);
            }
            boundary = sb.toString();
        }
        return boundary;
    }

    private char[] getBoundaryChars()
    {
        return Base64Encoder.getBase64Chars();
    }

    private String getContentTypeForFile(File file)
    {
        boolean isBinary = false;
        try {
            BufferedInputStream inputStream =
                new BufferedInputStream(file.getInputStream());
            int c;
            while ((c = inputStream.read()) >= 0) {
                if (c >= ' ' && c < 127)
                    continue;
                if (c == '\r')
                    continue;
                if (c == '\n')
                    continue;
                if (c == '\t')
                    continue;
                if (c == '\f')
                    continue;
                // If none of the above, it's not text.
                isBinary = true;
                break;
            }
            inputStream.close();
        }
        catch (IOException e) {
            Log.error(e);
            isBinary = true;
        }
        String extension = Utilities.getExtension(file);
        if (extension != null)
            extension = extension.toLowerCase();
        if (isBinary) {
            if (extension != null) {
                // Check for known image types.
                extension = extension.toLowerCase();
                if (extension.equals(".jpeg") || extension.equals(".jpg"))
                    return "image/jpeg";
                if (extension.equals(".gif"))
                    return "image/gif";
                if (extension.equals(".png"))
                    return "image/png";
            }
            // No extension or not a known type.
            return "application/octet-stream";
        } else {
            if (extension != null) {
                if (extension.equals(".html") || extension.equals(".htm"))
                    return "text/html";
                if (extension.equals(".xml"))
                    return "text/xml";
            }
            // No extension or not a known type.
            return "text/plain";
        }
    }

    public File getCurrentDirectory()
    {
        return Directories.getUserHomeDirectory();
    }

    public File getCompletionDirectory()
    {
        return Directories.getUserHomeDirectory();
    }

    // For the buffer list.
    public Icon getIcon()
    {
        if (isModified())
            return Utilities.getIconFromFile("compose_modified.png");
        return Utilities.getIconFromFile("compose.png");
    }

    public String getFileNameForDisplay()
    {
        return "";
    }

    private static boolean requiresEncoding(Line line)
    {
        final String text = line.getText();
        if (text.length() > 990)
            return true;
        if (text.length() == 0)
            return false;
        for (int i = text.length()-1; i >= 0; i--) {
            char c = text.charAt(i);
            if (c < ' ' && c != '\t')
                return true;
            if (c >= 127)
                return true;
        }
        if (text.startsWith("From "))
            return true;
        if (text.charAt(0) == '.')
            return true;
        return false;
    }

    private static final String getCharSetName(String characterEncoding)
    {
        if (characterEncoding.equals("ASCII"))
            return "us-ascii";
        if (characterEncoding.startsWith("ISO8859_"))
            return "iso-8859-" + characterEncoding.substring(8);
        // BUG! There are more cases!
        return characterEncoding;
    }

    public boolean isHeaderLine(Line maybe)
    {
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line == maybe)
                return true;
            if (line.getText().equals(HEADER_SEPARATOR))
                return false;
        }
        return false;
    }

    public void tab(Editor editor)
    {
        Line dotLine = editor.getDotLine();
        if (dotLine.getText().equals(HEADER_SEPARATOR)) {
            Line line = dotLine.next();
            if (line != null)
                editor.moveDotTo(line, 0);
            return;
        }
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line == dotLine) {
                break;
            } else if (line.getText().equals(HEADER_SEPARATOR)) {
                // We're in the body of the message. Just do the normal thing.
                editor.insertTab();
                return;
            }
        }
        // Reaching here, dot is in the header area. Advance to the end of the
        // next header.
        Line line = null;
        for (line = dotLine.next(); line != null; line = line.next()) {
            if (!isContinuationLine(line))
                break;
        }
        if (line == null)
            return;
        // Have we reached the header separator line?
        if (line.getText().equals(HEADER_SEPARATOR)) {
            line = line.next();
            if (line != null)
                editor.moveDotTo(line, 0);
            return;
        }
        // We're at start of next header. Put dot at end of it.
        while (true) {
            Line next = line.next();
            if (next == null)
                return;
            if (!isContinuationLine(next)) {
                // Next line is not a continuation line of current header.
                // Move dot to end of current line.
                editor.moveDotTo(line, line.length());
                return;
            }
            line = next;
        }
    }

    public void backTab(Editor editor)
    {
        Line dotLine = editor.getDotLine();
        // Check to see whether dot is in the header area or in the body of
        // the message.
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line == dotLine) {
                break;
            } else if (line.getText().equals(HEADER_SEPARATOR)) {
                // Dot is in the body of the message. Go back to the end of
                // the last header line.
                line = line.previous();
                if (line != null)
                    editor.moveDotTo(line, line.length());
                return;
            }
        }
        // Reaching here, dot is in the header area. Find the first line of
        // the header dot is in.
        Line line = dotLine;
        while (isContinuationLine(line)) {
            line = line.previous();
            if (line == null)
                return; // Shouldn't happen.
        }
        // Now we're on the first line of the header dot was in. Put dot at
        // the end of the previous line.
        line = line.previous();
        if (line != null)
            editor.moveDotTo(line, line.length());
    }

    private static final boolean isContinuationLine(Line line)
    {
        if (line.length() == 0)
            return false;
        char c = line.charAt(0);
        return c == ' ' || c == '\t';
    }

    // Append message to sent messages file (if so configured).
    private void writeFcc(File messageFile)
    {
        final File sentMessagesFile = Mail.getSentMessagesFile();
        if (sentMessagesFile == null)
            return;
        try {
            BufferedReader reader =
                new BufferedReader(new InputStreamReader(messageFile.getInputStream()));
            BufferedWriter writer =
                new BufferedWriter(new FileWriter(sentMessagesFile.canonicalPath(), true));
            writer.write("From - ");
            SimpleDateFormat dateFormatter =
                new SimpleDateFormat ("EEE MMM d HH:mm:ss yyyy");
            Calendar cal = Calendar.getInstance();
            String dateString = dateFormatter.format(cal.getTime());
            writer.write(dateString);
            writer.write('\n');
            String s;
            while((s = reader.readLine()) != null) {
                writer.write(s);
                writer.write('\n');
            }
            writer.write('\n');
            writer.flush();
            writer.close();
            reader.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
    }

    public Expansion getExpansion(Position dot)
    {
        int endOfHeaders = -1;
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line.getText().equals(HEADER_SEPARATOR)) {
                endOfHeaders = line.lineNumber();
                break;
            }
        }
        if (dot.lineNumber() < endOfHeaders)
            return new MailAddressExpansion(dot);
        else
            return super.getExpansion(dot);
    }

    private static final RE dateRE =
        new UncheckedRE("[A-Za-z]+, [0-9][0-9]? [A-Za-z]+ [0-9][0-9][0-9][0-9]");
    private static final RE timeRE =
        new UncheckedRE("[0-9:]+ ([+-][0-9][0-9][0-9][0-9]|[A-Z][A-Z][A-Z]+)");

    private static String getAttribution(MessageBuffer messageBuffer)
    {
        if (messageBuffer == null)
            return null;
        Mailbox mailbox = messageBuffer.getMailbox();
        if (mailbox == null)
            return null;
        MailboxEntry entry = messageBuffer.getMailboxEntry();
        if (entry == null)
            return null;
        String template = preferences.getStringProperty(Property.ATTRIBUTION);
        if (template == null || template.length() == 0)
            return null;
        FastStringBuffer sb = new FastStringBuffer();
        final int limit = template.length();
        for (int i = 0; i < limit; i++) {
            char c = template.charAt(i);
            if (c == '%' && ++i < limit) {
                c = template.charAt(i);
                switch (c) {
                    case 'd': {
                        // Date/time in sender's time zone.
                        Message message = messageBuffer.getMessage();
                        if (message == null)
                            return null;
                        String dateTime = message.getHeaderValue(Headers.DATE);
                        REMatch m1 = dateRE.getMatch(dateTime);
                        if (m1 != null) {
                            REMatch m2 =
                                timeRE.getMatch(dateTime.substring(m1.getEndIndex()));
                            if (m2 != null) {
                                sb.append(m1.toString());
                                sb.append(" at ");
                                sb.append(m2.toString());
                            }
                        }
                        break;
                    }
                    case 'n': {
                        // Author's real name (or address if missing).
                        MailAddress[] from = entry.getFrom();
                        if (from == null || from.length == 0)
                            return null;
                        String personal = from[0].getPersonal();
                        if (personal != null && personal.length() > 0)
                            sb.append(personal);
                        else {
                            String addr = from[0].getAddress();
                            if (addr != null && addr.length() > 0)
                                sb.append(addr);
                            else
                                return null;
                        }
                        break;
                    }
                    default:
                        Log.error("invalid format sequence \"%" + c + '"' +
                                  " in attribution");
                        return null;
                }
            } else
                sb.append(c);
        }
        return sb.toString();
    }
}
