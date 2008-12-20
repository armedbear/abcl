/*
 * MessageBuffer.java
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

import java.awt.Image;
import java.awt.Rectangle;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.StringReader;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.Vector;
import javax.swing.Icon;
import javax.swing.SwingUtilities;
import org.armedbear.j.Annotation;
import org.armedbear.j.Buffer;
import org.armedbear.j.BufferIterator;
import org.armedbear.j.Debug;
import org.armedbear.j.Display;
import org.armedbear.j.Editor;
import org.armedbear.j.EditorIterator;
import org.armedbear.j.File;
import org.armedbear.j.FastStringBuffer;
import org.armedbear.j.FastStringReader;
import org.armedbear.j.Headers;
import org.armedbear.j.ImageLine;
import org.armedbear.j.ImageLoader;
import org.armedbear.j.Line;
import org.armedbear.j.LineSequence;
import org.armedbear.j.Log;
import org.armedbear.j.MessageDialog;
import org.armedbear.j.MessageHeaderLine;
import org.armedbear.j.Position;
import org.armedbear.j.ProgressNotifier;
import org.armedbear.j.Property;
import org.armedbear.j.SaveFileDialog;
import org.armedbear.j.Sidebar;
import org.armedbear.j.SystemBuffer;
import org.armedbear.j.TextLine;
import org.armedbear.j.Utilities;
import org.armedbear.j.WebBuffer;
import org.armedbear.j.WebFormatter;
import org.armedbear.j.WebLine;
import org.armedbear.j.WebLoader;

public class MessageBuffer extends Buffer
{
    protected Mailbox mailbox;
    protected MailboxEntry entry;
    protected Message message;
    protected boolean showFullHeaders;
    protected boolean showRawText;
    protected String allHeaders;
    protected String defaultHeaders;
    protected String rawBody;
    protected String body;
    protected String mimeBody;
    protected MimePart selectedPart;
    protected int headerLineCount;

    private boolean wrap = true;

    protected MessageBuffer()
    {
        super();
        setInitialized(true);
    }

    public MessageBuffer(String rawText)
    {
        initializeUndo();
        type = TYPE_NORMAL;
        lineSeparator = "\n";
        mode = MessageMode.getMode();
        setFormatter(new MessageFormatter(this));
        readOnly = true;
        message = new Message(rawText);
        parseMessage();
        title = message.getHeaderValue(Headers.SUBJECT);
        if (title == null)
            title = "";
        allHeaders = message.getAllHeaders();
        defaultHeaders = getDefaultHeaders(allHeaders);
        rawBody = message.getRawBody();
        setText();
        renumber();
        formatter.parseBuffer();
        setInitialized(true);
    }

    public final boolean isPrimary()
    {
        if (mailbox == null)
            return true;
        return mailbox.getPreviewBuffer() != this;
    }

    public final boolean isSecondary()
    {
        if (mailbox == null)
            return false;
        return mailbox.getPreviewBuffer() == this;
    }

    public final Buffer getPrimary()
    {
        if (mailbox != null && mailbox.getPreviewBuffer() == this)
            return mailbox;
        return null;
    }

    public final void promote()
    {
        if (mailbox != null && mailbox.getPreviewBuffer() == this)
            mailbox.setPreviewBuffer(null);
    }

    public int getHeaderLineCount()
    {
        return headerLineCount;
    }

    public int getDisplayHeight()
    {
        int height = 0;
        for (Line line = getFirstLine(); line != null; line = line.nextVisible())
            height += line.getHeight();
        return height;
    }

    public int getDisplayWidth()
    {
        int width = 0;
        for (Line line = getFirstLine(); line != null; line = line.nextVisible()) {
            int lineWidth = line.getWidth();
            if (lineWidth > width)
                width = lineWidth;
        }
        return Display.getGutterWidth(this) + width + Display.getCharWidth();
    }

    // Returns cumulative height to top of target line.
    public int getY(Line target)
    {
        int y = 0;
        for (Line line = getFirstLine(); line != null && line != target; line = line.nextVisible())
            y += line.getHeight();
        return y;
    }

    protected void setEntry(MailboxEntry entry)
    {
        Debug.assertTrue(entry != null);
        reset();
        this.entry = entry;
        FastStringBuffer sb = new FastStringBuffer();
        sb.append(mailbox.getLineNumberForEntry(entry) + 1);
        sb.append('/');
        sb.append(mailbox.getLineCount());
        sb.append(' ');
        sb.append(entry.formatSubject());
        title = sb.toString();
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_REPAINT_BUFFER_LIST);
    }

    protected void reset()
    {
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            empty();
            message = null;
            mimeBody = null;
            selectedPart = null;
        }
        finally {
            unlockWrite();
        }
    }

    public int load()
    {
        Debug.assertTrue(false); // Shouldn't be called.
        return LOAD_COMPLETED;
    }

    protected void loadMessage(ProgressNotifier progressNotifier)
    {
        Debug.assertTrue(entry != null);
        mailbox.setBusy(true);
        message = mailbox.getMessage(entry, progressNotifier);
        mailbox.setBusy(false);
        if (message == null)
            return;
        parseMessage();
        allHeaders = message.getAllHeaders();
        defaultHeaders = getDefaultHeaders(allHeaders);
        rawBody = message.getRawBody();
        wrap = true;
        setText();
        formatter.parseBuffer();
        int flags = entry.getFlags();
        if ((flags & MailboxEntry.SEEN) == 0) {
            flags |= MailboxEntry.SEEN;
            flags &= ~MailboxEntry.RECENT;
            entry.setFlags(flags);
            mailbox.setDirty(true);
            mailbox.updateEntry(entry);
            mailbox.countMessages();
        }
        Runnable r = new Runnable() {
            public void run()
            {
                setBusy(false);
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == MessageBuffer.this) {
                        ed.setDot(getFirstLine(), 0);
                        ed.setUpdateFlag(REFRAME);
                        ed.moveCaretToDotCol();
                        ed.setTopLine(getFirstLine());
                        ed.setMark(null);
                        ed.setUpdateFlag(REPAINT);
                        ed.updateDisplay();
                    } else if (ed.getBuffer() == mailbox)
                        ed.updateDisplay();
                }
                Sidebar.repaintBufferListInAllFrames();
            }
        };
        SwingUtilities.invokeLater(r);
    }

    public final MailboxEntry getMailboxEntry()
    {
        return entry;
    }

    public final Message getMessage()
    {
        return message;
    }

    public final int getMessageNumber()
    {
        Debug.assertTrue(entry != null);
        return entry.getMessageNumber();
    }

    public final Mailbox getMailbox()
    {
        return mailbox;
    }

    public void nextMessage()
    {
        if (entry == null)
            return;
        Editor editor = Editor.currentEditor();
        MailboxEntry nextEntry = mailbox.getNextUndeleted(entry);
        if (nextEntry != null) {
            mailbox.setDotEntry(nextEntry);
            load(editor, nextEntry);
        } else {
            MailCommands.messageIndex(editor);
            editor.status("Last undeleted message");
        }
    }

    public void previousMessage()
    {
        if (entry == null)
            return;
        Editor editor = Editor.currentEditor();
        MailboxEntry previousEntry = mailbox.getPreviousUndeleted(entry);
        if (previousEntry != null) {
            mailbox.setDotEntry(previousEntry);
            load(editor, previousEntry);
        } else {
            MailCommands.messageIndex(editor);
            editor.status("First undeleted message");
        }
    }

    public void nextInThread()
    {
        if (entry == null)
            return;
        Editor editor = Editor.currentEditor();
        MailboxEntry nextEntry = mailbox.getNextInThread(entry);
        if (nextEntry != null) {
            mailbox.setDotEntry(nextEntry);
            load(editor, nextEntry);
        } else {
            MailCommands.messageIndex(editor);
            editor.status("Last message in thread");
        }
    }

    public void previousInThread()
    {
        if (entry == null)
            return;
        Editor editor = Editor.currentEditor();
        MailboxEntry previousEntry = mailbox.getPreviousInThread(entry);
        if (previousEntry != null) {
            mailbox.setDotEntry(previousEntry);
            load(editor, previousEntry);
        } else {
            MailCommands.messageIndex(editor);
            editor.status("First message in thread");
        }
    }

    public void parentMessage()
    {
        final Editor editor = Editor.currentEditor();
        String inReplyTo = message.getHeaderValue(Headers.IN_REPLY_TO);
        Log.debug("inReplyTo = |" + inReplyTo + "|");
        if (inReplyTo != null) {
            String msgId = extractMessageId(inReplyTo);
            if (msgId != null) {
                Log.debug("msgId = |" + msgId + "|");
                MailboxEntry parentEntry = mailbox.getEntryForMessageId(msgId);
                if (parentEntry != null) {
                    load(editor, parentEntry);
                    return;
                }
            }
        }
        String references = message.getHeaderValue(Headers.REFERENCES);
        Log.debug("references = |" + references + "|");
        if (references != null) {
            List list = extractAllMessageIds(references);
            if (list != null) {
                for (int i = 0; i < list.size(); i++) {
                    String msgId = (String) list.get(i);
                    if (msgId != null) {
                        Log.debug("msgId = |" + msgId + "|");
                        MailboxEntry parentEntry =
                            mailbox.getEntryForMessageId(msgId);
                        if (parentEntry != null) {
                            load(editor, parentEntry);
                            return;
                        }
                    }
                }
            }
        }
        editor.status("No parent");
    }

    private static String extractMessageId(String s)
    {
        if (s == null)
            return null;
        int begin = s.indexOf('<');
        if (begin < 0)
            return null;
        int end = s.indexOf('>');
        if (end < 0)
            return null;
        return s.substring(begin, end + 1);
    }

    private static List extractAllMessageIds(String s)
    {
        if (s == null)
            return null;
        ArrayList list = null;
        while (s.length() > 2) {
            int begin = s.indexOf('<');
            if (begin < 0)
                break;
            int end = s.indexOf('>');
            if (end < 0)
                break;
            String msgId = s.substring(begin, end + 1);
            if (list == null)
                list = new ArrayList();
            Log.debug("adding |" + msgId + "|");
            list.add(msgId);
            s = s.substring(end+1);
        }
        return list;
    }

    private void load(Editor editor, MailboxEntry nextEntry)
    {
        if (mailbox.lock()) {
            setBusy(true);
            setEntry(nextEntry);
            Runnable r = new Runnable() {
                public void run()
                {
                    try {
                        loadMessage(null);
                    }
                    finally {
                        mailbox.unlock();
                    }
                }
            };
            new Thread(r).start();
        } else
            editor.status("Mailbox is locked");
    }

    // Might return null.
    public String quoteBody(int wrapCol)
    {
        String toBeQuoted = mimeBody != null ? mimeBody : rawBody;
        if (toBeQuoted == null) {
            Log.debug("quoteBody toBeQuoted is null");
            return null;
        }
        if (Utilities.isWhitespace(toBeQuoted)) {
            Log.debug("quoteBody toBeQuoted is whitespace");
            return null;
        }
        String wrapped = wrap(toBeQuoted, wrapCol-2, 8);
        FastStringReader reader = new FastStringReader(wrapped);
        FastStringBuffer sb = new FastStringBuffer(4096);
        String s;
        while ((s = reader.readLine()) != null) {
            sb.append('>');
            if (s.length() > 0) {
                sb.append(' ');
                sb.append(s);
            }
            sb.append('\n');
        }
        return sb.toString();
    }

    private static final String wrap(String s, int wrapCol, int tabWidth)
    {
        FastStringReader reader = new FastStringReader(s);
        FastStringBuffer sb = new FastStringBuffer(4096);
        java.io.StringWriter writer = new java.io.StringWriter();
        String line;
        while ((line = reader.readLine()) != null) {
            if (line.length() == 0) {
                if (sb.length() > 0) {
                    writer.write(Utilities.wrap(sb.toString(), wrapCol,
                        tabWidth));
                    sb.setLength(0);
                    writer.write('\n');
                }
                writer.write('\n');
            } else {
                // Line is not empty.
                if (sb.length() == 0 &&
                    Utilities.getDetabbedLength(line, tabWidth) <= 78) {
                    // Line is a reasonable length.
                    writer.write(line);
                    writer.write('\n');
                    continue;
                }
                char c = line.charAt(0);
                if (c == ' ' || c == '\t' || c == '>') {
                    if (sb.length() > 0) {
                        writer.write(Utilities.wrap(sb.toString(), wrapCol,
                            tabWidth));
                        sb.setLength(0);
                        writer.write('\n');
                    }
                    writer.write(line);
                    writer.write('\n');
                } else {
                    if (sb.length() > 0) {
                        // Make sure last char is a space or tab.
                        c = sb.charAt(sb.length()-1);
                        if (c != ' ' && c != '\t')
                            sb.append(' ');
                    }
                    sb.append(line);
                }
            }
        }
        if (sb.length() > 0) {
            writer.write(Utilities.wrap(sb.toString(), wrapCol, tabWidth));
            sb.append('\n');
        }
        return writer.toString();
    }

    public void deleteMessage() {}

    public void flagMessage() {}

    public void moveMessage() {}

    public void bounce()
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        final Editor editor = Editor.currentEditor();
        final MailAddress[] to = MailCommands.bounceGetTo(editor, 1);
        if (to == null)
            return;
        Runnable bounceRunnable = new Runnable() {
            public void run()
            {
                Log.debug("MessageBuffer bounceRunnable.run()");
                boolean succeeded = false;
                try {
                    succeeded = Mail.bounceMessage(message, to);
                }
                finally {
                    setBusy(false);
                    editor.updateDisplayLater();
                }
                if (succeeded) {
                    Runnable successRunnable = new Runnable() {
                        public void run()
                        {
                            editor.status("Message bounced");
                        }
                    };
                    SwingUtilities.invokeLater(successRunnable);
                } else {
                    Runnable errorRunnable = new Runnable() {
                        public void run()
                        {
                            MessageDialog.showMessageDialog(editor, "Failed",
                                "Bounce Message");
                        }
                    };
                    SwingUtilities.invokeLater(errorRunnable);
                }
            }
        };
        setBusy(true);
        new Thread(bounceRunnable).start();
    }

    protected String getBeautifiedHeaders()
    {
        if (message == null)
            return "";
        Headers headers = Headers.parse(message.getRawHeaders());
        ArrayList names = new ArrayList(); // Header names.
        names.add("From");
        names.add("To");
        names.add("Cc");
        names.add("Subject");
        names.add("Date");
        int width = 0;
        for (Iterator it = names.iterator(); it.hasNext();) {
            int w = ((String)it.next()).length();
            if (w > width)
                width = w;
        }
        FastStringBuffer sb = new FastStringBuffer();
        for (Iterator it = names.iterator(); it.hasNext();) {
            String name = (String) it.next();
            String value = headers.getValue(name);
            if (value != null) {
                if (name == "From" || name== "To" || name == "Cc") {
                    MailAddress[] array =
                        MailAddress.parseAddresses(RFC2047.decode(value));
                    ArrayList list = new ArrayList();
                    for (int i = 0; i < array.length; i++)
                        list.add(array[i]);
                    String prefix =
                        Utilities.rightJustify(name, width).concat(": ");
                    sb.append(MailUtilities.constructAddressHeader(prefix,
                        list, prefix.length()));
                } else {
                    // Subject, date.
                    sb.append(Utilities.rightJustify(name, width));
                    sb.append(": ");
                    sb.append(RFC2047.decode(value));
                }
                sb.append('\n');
            }
        }
        return sb.toString();
    }

    protected String getDefaultHeaders(String s)
    {
        FastStringBuffer sb = new FastStringBuffer();
        if (s.length() > 0) {
            BufferedReader reader = new BufferedReader(new StringReader(s));
            boolean maybeContinuation = false;
            try {
                while (true) {
                    String text = reader.readLine();
                    if (text == null || text.length() == 0)
                        break;
                    if (maybeContinuation &&
                        Character.isWhitespace(text.charAt(0))) {
                        sb.append(text);
                        sb.append('\n');
                    } else if (isDefaultHeader(text)) {
                        sb.append(text);
                        sb.append('\n');
                        maybeContinuation = true;
                    } else
                        maybeContinuation = false;
                }
            }
            catch (IOException e) {
                Log.error(e);
            }
        }
        return sb.toString();
    }

    private boolean isDefaultHeader(String s)
    {
        s = s.toLowerCase();
        if (s.startsWith("from:") ||
            s.startsWith("to:") ||
            s.startsWith("cc:") ||
            s.startsWith("subject:") ||
            s.startsWith("date:"))
            return true;
        return false;
    }

    public void viewAttachment()
    {
        final MimePart part = getAttachmentAtDot();
        if (part == null)
            return;
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        final String contentType = part.getContentType();
        if (contentType != null && contentType.equals("message/rfc822")) {
            final String rawText = part.getRawBody();
            Buffer buf = null;
            // See if we already have this attachment open in a buffer.
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer b = it.nextBuffer();
                if (b instanceof MessageBuffer) {
                    MessageBuffer mb = (MessageBuffer) b;
                    Message m = mb.getMessage();
                    if (m.getRawText().equals(rawText)) {
                        buf = b;
                        break;
                    }
                }
            }
            if (buf == null) // Attachment is not in an existing buffer.
                buf = new MessageBuffer(rawText);
            editor.makeNext(buf);
            editor.switchToBuffer(buf);
            return;
        }
        File cache = part.cacheDecoded();
        if (cache == null || !cache.isFile()) {
            MessageDialog.showMessageDialog("Unable to decode attachment",
                "View Attachment");
            return;
        }
        if (contentType != null && contentType.equals("text/html")) {
            WebBuffer.browse(editor, cache, null);
            return;
        }
        Buffer buf = editor.openFile(cache);
        if (buf != null) {
            editor.makeNext(buf);
            editor.switchToBuffer(buf);
        }
    }

    public void saveAttachment()
    {
        MimePart part = getAttachmentAtDot();
        if (part == null)
            return;
        final Editor editor = Editor.currentEditor();
        SaveFileDialog d =
            new SaveFileDialog(editor, "Save Attachment", "File:");
        File suggested = File.getInstance(editor.getCurrentDirectory(),
            part.getAttachmentFileName());
        if (suggested != null)
            d.setInitialText(suggested.canonicalPath());
        editor.centerDialog(d);
        d.show();
        File saveAs = d.getDestination();
        if (saveAs == null)
            return;
        editor.repaintNow();
        editor.setWaitCursor();
        boolean success = part.saveDecoded(saveAs);
        editor.setDefaultCursor();
        if (!success)
            MessageDialog.showMessageDialog("Unable to save attachment",
                "Save Attachment");
    }

    private MimePart getAttachmentAtDot()
    {
        Position dot = Editor.currentEditor().getDot();
        if (dot != null) {
            Annotation annotation = dot.getLine().getAnnotation();
            if (annotation != null) {
                Object obj = annotation.getUserObject();
                if (obj instanceof MimePart)
                    return (MimePart) obj;
            }
        }
        return null;
    }

    public void toggleRaw()
    {
        showRawText = !showRawText;
        if (mailbox != null)
            mailbox.showRawText = showRawText;
        reloadInternal();
        FastStringBuffer sb = new FastStringBuffer("Raw mode ");
        sb.append(showRawText ? "on" : "off");
        Editor.currentEditor().status(sb.toString());
    }

    public void toggleHeaders()
    {
        if (mailbox != null)
            mailbox.showFullHeaders = showFullHeaders = !showFullHeaders;
        reloadInternal();
    }

    public void toggleWrap()
    {
        wrap = !wrap;
        reloadInternal();
        FastStringBuffer sb = new FastStringBuffer("Wrap ");
        sb.append(wrap ? "on" : "off");
        Editor.currentEditor().status(sb.toString());
    }

    private void reloadInternal()
    {
        setText();
        formatter.parseBuffer();
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.setDot(getFirstLine(), 0);
                ed.moveCaretToDotCol();
                ed.setMark(null);
                ed.setTopLine(getFirstLine());
                ed.repaintDisplay();
            }
        }
    }

    protected void parseMessage()
    {
        if (mimeBody != null)
            Debug.bug();
        message.parse();
        List parts = message.getParts();
        if (parts == null || parts.size() == 0) {
            // Not multipart.
            mimeBody = message.getDecodedBody();
            return;
        }
        for (Iterator it = parts.iterator(); it.hasNext();) {
            MimePart part = (MimePart) it.next();
            final String contentType = part.getContentType();
            if (contentType == null || contentType.equals("text/plain")) {
                selectedPart = part;
                mimeBody = part.getDecodedBody();
                return;
            }
        }
        for (Iterator it = parts.iterator(); it.hasNext();) {
            MimePart part = (MimePart) it.next();
            final String contentType = part.getContentType();
            if (contentType == null || contentType.equals("text/html")) {
                selectedPart = part;
                mimeBody = part.getDecodedBody();
                return;
            }
        }
        mimeBody = "";
    }

    private List getAttachmentLines()
    {
        List parts = message.getParts();
        if (parts == null || parts.size() == 0) {
            // Not multipart.
            return null;
        }
        int shown = -1;
        ArrayList list = new ArrayList();
        list.add(new MessageHeaderLine("Parts/Attachments:"));
        for (int i = 0; i < parts.size(); i++) {
            FastStringBuffer sb = new FastStringBuffer();
            sb.append("   ");
            sb.append(i+1);
            MimePart part = (MimePart) parts.get(i);
            final String contentType = part.getContentType();
            if (part == selectedPart) {
                sb.append(" Shown");
            } else {
                String filename = part.getAttachmentFileName();
                if (filename != null && filename.length() > 0) {
                    if (part.isAttachment())
                        sb.append(" Attachment:");
                    else if (part.isInline())
                        sb.append(" Inline:");
                    sb.append(' ');
                    sb.append(filename);
                } else if (part.isAttachment()) {
                    sb.append(" Attachment");
                } else if (part.isInline()) {
                    sb.append(" Inline");
                }
            }
            sb.append(" (");
            if (contentType != null) {
                sb.append(contentType);
                sb.append(", ");
            }
            final String encoding = part.getTransferEncoding();
            if (encoding != null) {
                sb.append(encoding);
                sb.append(", ");
            }
            sb.append(part.getSize());
            sb.append(" bytes)");
            Line line = new MessageHeaderLine(sb.toString());
            line.setAnnotation(new Annotation(part));
            list.add(line);
        }
        return list;
    }

    protected void setText()
    {
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            _setText();
            setLoaded(true);
        }
        finally {
            unlockWrite();
        }
    }

    private void _setText()
    {
        empty();
        if (showRawText) {
            body = rawBody;
            setText(message.getRawText());
            headerLineCount = Utilities.countLines(allHeaders);
            if (!(formatter instanceof MessageFormatter))
                setFormatter(new MessageFormatter(this));
            return;
        }
        // Not raw mode.
        final String headers;
        if (showFullHeaders)
            headers = allHeaders;
        else if (Editor.preferences().getBooleanProperty(Property.BEAUTIFY_HEADERS))
            headers = getBeautifiedHeaders();
        else
            headers = defaultHeaders;
        final String contentType = message.getContentType();
        if (contentType != null) {
            if (contentType.startsWith("image/")) {
                File cache = message.cacheDecoded();
                if (cache != null && cache.isFile()) {
                    ImageLoader loader = new ImageLoader(cache);
                    Image image = loader.loadImage();
                    appendHeaderLines(headers);
                    appendHeaderLine("");
                    if (image != null) {
                        final int lineHeight = new TextLine("").getHeight();
                        final int imageHeight = image.getHeight(null);
                        final int imageWidth = image.getWidth(null);
                        int y = 0;
                        while (y < imageHeight) {
                            Rectangle r = new Rectangle(0, y, imageWidth,
                                Math.min(lineHeight, imageHeight - y));
                            appendLine(new ImageLine(image, r));
                            y += lineHeight;
                        }
                    }
                    renumber();
                } else {
                    appendHeaderLines(headers);
                    appendHeaderLine("");
                    append(body);
                }
                renumber();
                if (!(formatter instanceof MessageFormatter))
                    setFormatter(new MessageFormatter(this));
                return;
            }
            if (contentType.equals("text/html")) {
                appendHeaderLines(headers);
                appendHeaderLine("");
                StringReader reader =
                    new StringReader(message.getDecodedBody());
                WebLoader loader = new WebLoader(reader);
                LineSequence lines = loader.load();
                Line lastLine = getLastLine();
                lastLine.setNext(lines.getFirstLine());
                lines.getFirstLine().setPrevious(lastLine);
                renumber();
                if (!(formatter instanceof WebFormatter))
                    setFormatter(new WebFormatter(this));
                return;
            }
        }
        appendHeaderLines(headers);
        headerLineCount = Utilities.countLines(headers);
        List attachmentLines = getAttachmentLines();
        if (attachmentLines != null) {
            appendHeaderLine("");
            ++headerLineCount;
            Iterator iter = attachmentLines.iterator();
            while (iter.hasNext()) {
                Line line = (Line) iter.next();
                if (!(line instanceof MessageHeaderLine))
                    Debug.bug();
                appendLine(line);
                ++headerLineCount;
            }
        }
        if (mimeBody != null)
            body = mimeBody;
        else
            body = rawBody;
        if (selectedPart != null && "text/html".equals(selectedPart.getContentType())) {
            appendLine("");
            StringReader reader = new StringReader(mimeBody);
            WebLoader loader = new WebLoader(reader);
            LineSequence lines = loader.load();
            Line lastLine = getLastLine();
            lastLine.setNext(lines.getFirstLine());
            lines.getFirstLine().setPrevious(lastLine);
            if (!(formatter instanceof WebFormatter))
                setFormatter(new WebFormatter(this));
        } else {
            if (wrap)
                body = wrapBody(body);
            appendLine("");
            append(body);
            if (!(formatter instanceof MessageFormatter))
                setFormatter(new MessageFormatter(this));
        }
        // Don't try to display images inline if the message is too big.
        if (message.getSize() < 1024 * 1024) {
            List parts = message.getParts();
            if (parts != null) {
                for (int i = 0; i < parts.size(); i++) {
                    MimePart part = (MimePart) parts.get(i);
                    String partContentType = part.getContentType();
                    if (partContentType == null)
                        continue;
                    File cache = null;
                    if (partContentType.equals("application/octet-stream")) {
                        String filename = part.getAttachmentFileName();
                        if (filename == null)
                            continue;
                        filename = filename.toLowerCase();
                        if (filename.endsWith(".jpg") ||
                            filename.endsWith(".jpeg") ||
                            filename.endsWith(".png") ||
                            filename.endsWith(".gif"))
                            cache = part.cacheDecoded();
                    } else if (partContentType.startsWith("image/"))
                        cache = part.cacheDecoded();
                    if (cache != null && cache.isFile()) {
                        ImageLoader loader = new ImageLoader(cache);
                        Image image = loader.loadImage();
                        if (image != null) {
                            if (getLastLine() instanceof ImageLine)
                                appendLine("");
                            final int lineHeight = new TextLine("").getHeight();
                            final int imageHeight = image.getHeight(null);
                            final int imageWidth = image.getWidth(null);
                            int y = 0;
                            while (y < imageHeight) {
                                Rectangle r = new Rectangle(0, y, imageWidth,
                                    Math.min(lineHeight, imageHeight - y));
                                appendLine(new ImageLine(image, r));
                                y += lineHeight;
                            }
                        }
                    }
                }
            }
        }
        renumber();
    }

    private static String wrapBody(String body)
    {
        final int wrapCol = Editor.currentEditor().getDisplay().getColumns();
        final int tabWidth = 8;
        final int IN_DIFF = 1;
        SystemBuffer buf = new SystemBuffer();
        FastStringReader reader = new FastStringReader(body);
        String s;
        while ((s = reader.readLine()) != null)
            buf.appendLine(s);
        boolean containsDiff = false;
        boolean inDiff = false;
        for (Line line = buf.getFirstLine(); line != null; line = line.next()) {
            if (inDiff) {
                if (MessageFormatter.isDiffContinuation(line))
                    line.setFlags(IN_DIFF);
                else {
                    inDiff = false;
                    line.setFlags(0);
                }
                continue;
            }
            // Not in diff.
            if (MessageFormatter.isDiffStart(line)) {
                inDiff = true;
                line.setFlags(IN_DIFF);
                containsDiff = true;
                continue;
            }
            // Not start of diff.
            line.setFlags(0);
        }
        if (!containsDiff)
            return Utilities.wrap(body, wrapCol, tabWidth);
        // Buffer contains diff.
        FastStringBuffer out = new FastStringBuffer();
        FastStringBuffer in = new FastStringBuffer();
        for (Line line = buf.getFirstLine(); line != null; line = line.next()) {
            if (line.flags() == IN_DIFF) {
                if (in.length() > 0) {
                    out.append(Utilities.wrap(in.toString(), wrapCol, tabWidth));
                    in.setLength(0);
                }
                out.append(line.getText());
                out.append('\n');
            } else {
                // Not in diff.
                in.append(line.getText());
                in.append('\n');
            }
        }
        if (in.length() > 0)
            out.append(Utilities.wrap(in.toString(), wrapCol, tabWidth));
        return out.toString();
    }

    protected void appendHeaderLines(String headers)
    {
        if (headers != null) {
            FastStringReader reader = new FastStringReader(headers);
            String s;
            while ((s = reader.readLine()) != null)
                appendHeaderLine(s);
        }
    }

    protected void appendHeaderLine(String s)
    {
        appendLine(new MessageHeaderLine(s));
    }

    public String toString()
    {
        return title;
    }

    // For the buffer list.
    public Icon getIcon()
    {
        return Utilities.getIconFromFile("message.png");
    }

    public String getFileNameForDisplay()
    {
        return "";
    }

    private static final String SPLIT_KEY = "MessageBuffer.split";

    public void saveWindowState(Editor editor)
    {
        if (editor.getBuffer() != this)
            return;
        Editor otherEditor = editor.getOtherEditor();
        if (otherEditor != null) {
            float height = (float) editor.getHeight();
            float split =
                height / (editor.getFrame().getEditorPane().getHeight());
            Editor.getSessionProperties().setFloatProperty(SPLIT_KEY, split);
        }
    }

    public float getSplit()
    {
        return Editor.getSessionProperties().getFloatProperty(SPLIT_KEY, 0.5F);
    }

    public void windowClosing()
    {
        Editor editor = Editor.currentEditor();
        if (editor.getBuffer() == this)
            saveWindowState(editor);
        else {
            Editor otherEditor = editor.getOtherEditor();
            if (otherEditor != null && otherEditor.getBuffer() == this)
                saveWindowState(otherEditor);
        }
    }

    public void dispose()
    {
        if (mailbox != null && mailbox.getPreviewBuffer() == this)
            mailbox.setPreviewBuffer(null);
        flushImages();
    }

    public void empty()
    {
        flushImages();
        super.empty();
    }

    private void flushImages()
    {
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            if (line instanceof ImageLine)
                ((ImageLine)line).flushImage();
        }
    }
}
