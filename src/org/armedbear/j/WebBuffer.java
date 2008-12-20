/*
 * WebBuffer.java
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

package org.armedbear.j;

import java.awt.AWTEvent;
import java.awt.Cursor;
import java.awt.Image;
import java.awt.Rectangle;
import java.awt.event.MouseEvent;
import java.io.InputStream;
import java.util.Hashtable;
import javax.swing.SwingUtilities;

public final class WebBuffer extends Buffer implements WebConstants
{
    private String ref;
    private WebHistory history;
    private Hashtable refs;
    private String contentType;
    private String errorText;

    private WebBuffer(File file, File cache, String ref)
    {
        super();
        setFile(file);
        setCache(cache);
        this.ref = ref;
        init();
    }

    private void init()
    {
        initializeUndo();
        type = TYPE_NORMAL;
        forceReadOnly = true;
        mode = Editor.getModeList().getMode(WEB_MODE);
        formatter = new WebFormatter(this);
        setInitialized(true);
    }

    public final WebHistory getHistory()
    {
        return history;
    }

    public final void setHistory(WebHistory history)
    {
        this.history = history;
    }

    public final String getContentType()
    {
        return contentType;
    }

    public final void setContentType(String contentType)
    {
        this.contentType = contentType;
    }

    private int maxChars;

    private final int maxChars()
    {
        if (maxChars == 0) {
            Display display = Editor.currentEditor().getDisplay();
            int charWidth = display.getCharWidth();
            if (charWidth > 0)
                maxChars = display.getWidth() / charWidth - 2;
            else
                maxChars = 80;
        }
        return maxChars;
    }

    public Position getInitialDotPos()
    {
        if (ref != null) {
            Position pos = findRef(ref);
            if (pos != null)
                return pos;
        }
        return new Position(getFirstLine(), 0);
    }

    public static void browse(Editor editor, File file, String ref)
    {
        if (file == null)
            return;
        Buffer buf = null;
        // Look for existing buffer.
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer b = it.nextBuffer();
            if (b instanceof WebBuffer && b.getFile().equals(file)) {
                buf = b;
                break;
            }
        }
        if (buf == null)
            buf = createWebBuffer(file, null, ref);
        editor.makeNext(buf);
        editor.switchToBuffer(buf);
    }

    public static WebBuffer createWebBuffer(File file, File cache, String ref)
    {
        return new WebBuffer(file, cache, ref);
    }

    public Position findRef(String ref)
    {
        if (ref != null && refs != null) {
            Object obj = refs.get(ref);
            if (obj instanceof Integer) {
                Position pos = getPosition(((Integer) obj).intValue());
                if (pos != null)
                    pos.skipWhitespace();
                return pos;
            }
        }
        return null;
    }

    public static void viewPage()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getModeId() != HTML_MODE)
            return;
        final Buffer buffer = editor.getBuffer();
        File file = buffer.getFile();
        if (file == null)
            return;
        Buffer buf = null;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer b = it.nextBuffer();
            if (b instanceof WebBuffer && file.equals(b.getFile())) {
                buf = b;
                break;
            }
        }
        if (buf == null) {
            buf = WebBuffer.createWebBuffer(file, buffer.getCache(), null);
            ((WebBuffer)buf).setContentType("text/html");
        }
        editor.makeNext(buf);
        editor.activate(buf);
    }

    public static void viewSource()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (!(buffer instanceof WebBuffer))
            return;
        final Line dotLine = editor.getDotLine();
        File file = buffer.getFile();
        Buffer buf = Editor.getBufferList().findBuffer(file);
        if (buf == null)
            buf = Buffer.createBuffer(file, buffer.getCache(), null);
        if (buf != null) {
            editor.makeNext(buf);
            editor.activate(buf);
            if (dotLine instanceof WebLine) {
                int sourceOffset = ((WebLine)dotLine).getSourceOffset();
                Position pos = buf.getPosition(sourceOffset);
                editor.moveDotTo(pos);
            }
        }
    }

    private boolean empty = true;

    public int load()
    {
        if (getFirstLine() == null)
            setText("");
        setLoaded(true);
        go(getFile(), 0, contentType);
        return LOAD_COMPLETED;
    }

    protected void loadFile(File localFile)
    {
        WebLoader loader = new WebLoader(localFile);
        LineSequence lines = loader.load();
        if (lines != null) {
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.debug(e);
                return;
            }
            try {
                setFirstLine(lines.getFirstLine());
                setLastLine(lines.getLastLine());
                renumberOriginal();
                empty = false;
            }
            finally {
                unlockWrite();
            }
        }
        refs = loader.getRefs();
        final File file = getFile();
        if (file != null && file.equals(localFile))
            setLastModified(localFile.lastModified());
        setLoaded(true);
    }

    public Cursor getDefaultCursor(Position pos)
    {
        if (pos != null && pos.getLine() instanceof WebLine) {
            HtmlLineSegment segment =
                ((WebLine)pos.getLine()).findSegment(pos.getOffset());
            if (segment != null && segment.getLink() != null)
                return Cursor.getPredefinedCursor(Cursor.HAND_CURSOR);
        }
        return super.getDefaultCursor();
    }

    public final int getMaximumColumns()
    {
        return maxChars();
    }

    public static void followLink()
    {
        followLink(false);
    }

    public static void mouseFollowLink()
    {
        final Editor editor = Editor.currentEditor();
        // If this method is invoked via a mouse event mapping, move dot to
        // location of mouse click first.
        AWTEvent e = editor.getDispatcher().getLastEvent();
        if (e instanceof MouseEvent)
            editor.mouseMoveDotToPoint((MouseEvent) e);
        followLink(true);
    }

    public static void followLink(boolean exact)
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (!(buffer instanceof WebBuffer))
            return;
        final WebBuffer wb = (WebBuffer) buffer;
        if (wb.getFile() == null)
            return;
        final Line dotLine = editor.getDotLine();
        if (!(dotLine instanceof WebLine))
            return;

        final File historyFile = wb.getFile();
        final int historyOffset = wb.getAbsoluteOffset(editor.getDot());
        final String historyContentType = wb.getContentType();

        final int dotOffset = editor.getDotOffset();

        Link link = null;
        HtmlLineSegment segment = ((WebLine)dotLine).findSegment(dotOffset);
        if (segment != null)
            link = segment.getLink();
        if (link == null) {
            if (exact)
                return;
            segment = findLink(dotLine, dotOffset);
            if (segment == null)
                return;
            link = segment.getLink();
        }
        if (link == null)
            return;
        Debug.assertTrue(link == segment.getLink());
        final String target = link.getTarget();
        if (target == null) {
            Debug.bug("target is null");
            return;
        }
        if (target.startsWith("mailto:")) {
            MessageDialog.showMessageDialog(editor,
                "Sorry, mailto URLs are not yet supported.", "Sorry...");
            return;
        }

        int index = target.indexOf('#');
        if (index == 0) {
            // It's a fragment identifier referring to a location in the same
            // buffer.
            Position pos = wb.findRef(target.substring(1));
            if (pos != null) {
                wb.saveHistory(historyFile, historyOffset, historyContentType);
                editor.moveDotTo(pos);
                editor.setUpdateFlag(REFRAME);
            }
            return;
        }

        String filename;
        String ref = null;
        if (index >= 0) {
            filename = target.substring(0, index);
            ref = target.substring(index + 1);
        } else
            filename = target;

        final File destination =
            resolve(wb.getFile().getParentFile(), filename);

        if (destination.equals(wb.getFile())) {
            Position pos = wb.findRef(ref);
            if (pos != null) {
                wb.saveHistory(historyFile, historyOffset, historyContentType);
                editor.moveDotTo(pos);
            }
            return;
        }

        final HtmlLineSegment theSegment = segment;
        final Link theLink = link;
        if (destination instanceof HttpFile) {
            final String theRef = ref;
            final HttpLoadProcess httpLoadProcess =
                new HttpLoadProcess(wb, (HttpFile) destination);
            Runnable successRunnable = new Runnable() {
                public void run()
                {
                    final String contentType = httpLoadProcess.getContentType();
                    Log.debug("content-type = " + contentType);
                    boolean isImage = false;
                    if (contentType != null && contentType.toLowerCase().startsWith("image/"))
                        isImage = true;
                    else {
                        String extension = Utilities.getExtension(destination);
                        if (extension != null) {
                            extension = extension.toLowerCase();
                            if (extension.equals(".jpg") || extension.equals(".gif") || extension.equals(".png"))
                                isImage = true;
                        }
                    }
                    if (isImage) {
                        if (theLink instanceof ImageLink) {
                            ImageLoader loader = new ImageLoader(httpLoadProcess.getCache());
                            java.awt.Image image = loader.loadImage();
                            if (image != null)
                                wb.insertImage(editor, dotLine, theSegment, image);
                        } else {
                            // Normal link.
                            // BUG!! This isn't right either. We should display the image in
                            // the current buffer.
                            ImageBuffer buf = ImageBuffer.createImageBuffer(destination, httpLoadProcess.getCache(), null);
                            editor.makeNext(buf);
                            editor.activate(buf);
                            editor.updateDisplay();
                        }
                        return;
                    }
                    if (wb.loadLocalFile(httpLoadProcess.getCache(), contentType, httpLoadProcess.getCache().getEncoding())) {
                        wb.saveHistory(historyFile, historyOffset, historyContentType);
                        wb.setFile(httpLoadProcess.getFile());
                        wb.setCache(httpLoadProcess.getCache());
                        Position pos = null;
                        if (theRef != null)
                            pos = wb.findRef(theRef);
                        if (pos == null)
                            pos = new Position(wb.getFirstLine(), 0);
                        wb.update(pos);
                    }
                    for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                        Editor ed = it.nextEditor();
                        if (ed != null && ed.getBuffer() == wb)
                            ed.setDefaultCursor();
                    }
                }
            };
            ErrorRunnable errorRunnable = new ErrorRunnable("Operation failed") {
                public void run()
                {
                    Log.debug("followLink errorRunnable.run");
                    String errorText = httpLoadProcess.getErrorText();
                    if (errorText == null || errorText.length() == 0)
                        errorText = "Unable to load " + destination.netPath();
                    setMessage(errorText);
                    super.run();
                }
            };
            httpLoadProcess.setSuccessRunnable(successRunnable);
            httpLoadProcess.setErrorRunnable(errorRunnable);
            httpLoadProcess.setProgressNotifier(new StatusBarProgressNotifier(wb));
            editor.setWaitCursor();
            new Thread(httpLoadProcess).start();
        } else {
            // Local file.
            String extension = Utilities.getExtension(destination);
            if (extension != null) {
                extension = extension.toLowerCase();
                if (extension.equals(".jpg") || extension.equals(".gif") || extension.equals(".png")) {
                    if (theLink instanceof ImageLink) {
                        ImageLoader loader = new ImageLoader(destination);
                        java.awt.Image image = loader.loadImage();
                        if (image != null)
                            wb.insertImage(editor, dotLine, theSegment, image);
                    } else {
                        // BUG!! Should display image in same buffer.
                        ImageBuffer buf = ImageBuffer.createImageBuffer(destination, null, null);
                        editor.makeNext(buf);
                        editor.activate(buf);
                        editor.updateDisplay();
                    }
                    return;
                }
            }
            if (wb.loadLocalFile(destination)) {
                wb.saveHistory(historyFile, historyOffset, historyContentType);
                wb.setFile(destination);
                wb.setCache(null);
                Position pos = null;
                if (ref != null)
                    pos = wb.findRef(ref);
                if (pos == null)
                    pos = new Position(wb.getFirstLine(), 0);
                wb.update(pos);
            }
        }
    }

    public static HtmlLineSegment findLink(Line line, int offset)
    {
        HtmlLineSegment segment = null;
        if (line instanceof WebLine) {
            LineSegmentList segmentList = ((WebLine)line).getSegmentList();
            if (segmentList != null) {
                int where = 0;
                final int size = segmentList.size();
                for (int i = 0; i < size; i++) {
                    HtmlLineSegment seg =
                        (HtmlLineSegment) segmentList.getSegment(i);
                    if (seg.getLink() != null) {
                        if (segment == null || where < offset)
                            segment = seg;
                    }
                    where += seg.length();
                    if (where > offset && segment != null)
                        break;
                }
            }
        }
        return segment;
    }

    private static File resolve(File base, String fileName)
    {
        while (fileName.startsWith("../")) {
            File parent = base.getParentFile();
            if (parent != null) {
                base = parent;
                fileName = fileName.substring(3);
            } else
                break;
        }
        // Strip any remaining (i.e. bogus) lead dots.
        while (fileName.startsWith("../")) {
            fileName = fileName.substring(3);
        }
        return File.getInstance(base, fileName);
    }

    // BUG!! Optimize this - containsImageLine flag
    public int getDisplayHeight()
    {
        int height = 0;
        for (Line line = getFirstLine(); line != null; line = line.nextVisible())
            height += line.getHeight();
        return height;
    }

    private void insertImage(final Editor editor, final Line line,
        final HtmlLineSegment segment, final Image image)
    {
        final int imageHeight = image.getHeight(null);
        if (imageHeight == 0)
            return;

        LineSegmentList segments = ((WebLine)line).getSegmentList();
        segments.removeSegment(segment);

        // Force next call to line.getText() to enumerate the segments again.
        line.setText(null);

        Line before;
        if (line.isBlank() && line.previous() != null) {
            // Current line is blank. Put image in its place.
            before = line.previous();
        } else {
            // Put image after current line.
            before = line;
        }

        final int lineHeight = new TextLine("").getHeight();
        final int imageWidth = image.getWidth(null);
        Line dotLine = null;
        for (int y = 0; y < imageHeight; y += lineHeight) {
            Rectangle r = new Rectangle(0, y, imageWidth,
                Math.min(lineHeight, imageHeight - y));
            ImageLine imageLine = new ImageLine(image, r);
            imageLine.insertAfter(before);
            before = imageLine;
            if (dotLine == null)
                dotLine = imageLine;
        }
        renumber();
        Debug.assertTrue(dotLine != null);
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.getDot().moveTo(dotLine, 0);
                ed.setMark(null); // Enforce sanity.
                ed.setUpdateFlag(REFRAME | REPAINT);
                ed.updateDisplay();
            }
        }
    }

    public void saveHistory(File historyFile, int historyOffset,
        String historyContentType)
    {
        if (history == null)
            history = new WebHistory();
        else
            history.truncate();
        history.append(historyFile, historyOffset, historyContentType);
        history.reset();
    }

    private boolean loadLocalFile(File localFile)
    {
        return loadLocalFile(localFile, "text/html");
    }

    private boolean loadLocalFile(File localFile, String contentType)
    {
        return loadLocalFile(localFile, contentType, null);
    }

    private boolean loadLocalFile(File localFile, String contentType,
        String encoding)
    {
        // Look for Unicode byte order mark. If we find it, use it to
        // determine the encoding.
        InputStream inputStream;
        try {
            inputStream = localFile.getInputStream();
        }
        catch (Exception e) {
            Log.error(e);
            errorText = "File not found";
            return false; // File not found.
        }
        byte[] buf = new byte[2];
        try {
            int bytesRead = inputStream.read(buf);
            inputStream.close();
            if (bytesRead == 2) {
                byte byte1 = buf[0];
                byte byte2 = buf[1];
                if (byte1 == (byte) 0xfe && byte2 == (byte) 0xff)
                    encoding = "UnicodeBig";
                else if (byte1 == (byte) 0xff && byte2 == (byte) 0xfe)
                    encoding = "UnicodeLittle";
            }
        }
        catch (Exception e) {
            Log.error(e);
            return false;
        }
        try {
            inputStream = localFile.getInputStream();
        }
        catch (Exception e) {
            Log.error(e);
            errorText = "File not found";
            return false; // File not found.
        }
        boolean isHtml = false;
        if (contentType != null) {
            if (contentType.toLowerCase().startsWith("text/html"))
                isHtml = true;
        } else {
            if (Editor.getModeList().modeAccepts(HTML_MODE, localFile.getName()))
                isHtml = true;
        }
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return false;
        }
        try {
            empty();
            if (isHtml) {
                loadFile(localFile);
                setContentType(contentType);
                setFormatter(new WebFormatter(this));
            } else {
                super.load(inputStream, encoding);
                if (getFirstLine() == null) {
                    // New or 0-byte file.
                    appendLine("");
                    lineSeparator = System.getProperty("line.separator");
                }
                renumberOriginal();
                setContentType(contentType);
                Mode mode = Editor.getModeList().getModeForContentType(contentType);
                if (mode == null) {
                    File file = getFile();
                    if (file != null)
                        mode = Editor.getModeList().getModeForFileName(file.getName());
                }
                if (mode != null) {
                    setFormatter(mode.getFormatter(this));
                    formatter.parseBuffer();
                } else
                    setFormatter(new PlainTextFormatter(this));
            }
        }
        finally {
            unlockWrite();
        }
        return true;
    }

    public static void back()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (!(buffer instanceof WebBuffer))
            return;
        final WebBuffer wb = (WebBuffer) buffer;
        WebHistory history = wb.getHistory();
        if (history == null)
            return;
        boolean atEnd = history.atEnd();
        WebHistoryEntry current = history.getCurrent();
        WebHistoryEntry previous = history.getPrevious();
        if (previous != null) {
            if (atEnd)
                history.append(wb.getFile(),
                    wb.getAbsoluteOffset(editor.getDot()), wb.getContentType());
            else if (current != null)
                current.setOffset(wb.getAbsoluteOffset(editor.getDot()));
            else
                Debug.bug();
            if (previous.getFile().equals(wb.getFile()))
                wb.update(previous.getOffset());
            else
                wb.go(previous.getFile(), previous.getOffset(), previous.getContentType());
        }
    }

    public static void forward()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (!(buffer instanceof WebBuffer))
            return;
        final WebBuffer wb = (WebBuffer) buffer;
        WebHistory history = wb.getHistory();
        if (history == null)
            return;
        WebHistoryEntry current = history.getCurrent();
        WebHistoryEntry next = history.getNext();
        if (next != null) {
            if (current != null)
                current.setOffset(wb.getAbsoluteOffset(editor.getDot()));
            else
                Debug.bug();
            if (next.getFile().equals(wb.getFile()))
                wb.update(next.getOffset());
            else
                wb.go(next.getFile(), next.getOffset(), next.getContentType());
        }
    }

    public static void refresh()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof WebBuffer)
            ((WebBuffer)buffer).reload(editor);
    }

    private void reload(Editor editor)
    {
        final File file = getFile();
        if (file instanceof HttpFile) {
            HttpFile httpFile = (HttpFile) file;
            File cache = httpFile.getCache();
            if (cache != null) {
                if (cache.isFile())
                    cache.delete();
                httpFile.setCache(null);
            }
        }
        int offset = getAbsoluteOffset(editor.getDot());
        go(file, offset, contentType);
    }

    public void go(final File destination, final int offset, String contentType)
    {
        if (destination == null)
            return;

        if (destination.isLocal()) {
            if (loadLocalFile(destination, contentType)) {
                setFile(destination);
                update(offset);
                setLastModified(destination.lastModified());
            } else {
                // Error!
                Runnable errorRunnable = new Runnable() {
                    public void run()
                    {
                        if (empty && Editor.getBufferList().contains(WebBuffer.this))
                            kill();
                        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                            Editor ed = it.nextEditor();
                            if (empty || ed.getBuffer() == WebBuffer.this) {
                                ed.updateLocation();
                                ed.updateDisplay();
                            }
                        }
                        if (errorText == null)
                            errorText = "Unable to load " + destination.canonicalPath();
                        MessageDialog.showMessageDialog(errorText, "Error");
                    }
                };
                SwingUtilities.invokeLater(errorRunnable);
            }
            return;
        }

        // Destination is not local.
        if (destination.equals(getFile())) {
            File cache = getCache();
            if (cache != null && cache.isFile()) {
                // We have a cache.
                if (loadLocalFile(cache, contentType)) {
                    setFile(destination);
                    update(offset);
                }
                return;
            }
        }

        Debug.assertTrue(destination instanceof HttpFile);
        final HttpFile httpFile = (HttpFile) destination;
        File cache = httpFile.getCache();
        if (cache != null) {
            if (cache.isFile()) {
                if (contentType == null)
                    contentType = httpFile.getContentType();
                if (loadLocalFile(cache, contentType)) {
                    setFile(destination);
                    setCache(cache);
                    update(offset);
                }
                return;
            }
            cache = null;
            httpFile.setCache(null);
        }

        Debug.assertTrue(cache == null);
        Log.debug("go cache is null");
        final File oldFile = getFile();
        setFile(destination);
        final HttpLoadProcess httpLoadProcess = new HttpLoadProcess(this, httpFile);
        Runnable httpSuccessRunnable = new Runnable() {
            public void run()
            {
                File localCache = httpLoadProcess.getCache();
                if (localCache != null && localCache.isFile()) {
                    if (loadLocalFile(localCache, httpLoadProcess.getContentType(), localCache.getEncoding())) {
                        setFile(httpLoadProcess.getFile());
                        setCache(localCache);
                        setContentType(httpLoadProcess.getContentType());
                        update(offset);
                    }
                }
            }
        };
        Runnable httpCancelRunnable = new Runnable() {
            public void run()
            {
                setFile(oldFile);
                setBusy(false);
                if (empty && Editor.getBufferList().contains(WebBuffer.this))
                    kill();
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed != null && ed.getBuffer() == WebBuffer.this) {
                        ed.status("Transfer cancelled");
                        ed.setDefaultCursor();
                    }
                }
                Editor.currentEditor().updateDisplay();
                MessageDialog.showMessageDialog("Transfer cancelled", httpFile.netPath());
            }
        };
        ErrorRunnable httpErrorRunnable = new ErrorRunnable("Load failed") {
            public void run()
            {
                setFile(oldFile);
                setBusy(false);
                if (empty && Editor.getBufferList().contains(WebBuffer.this))
                    kill();
                if (!httpLoadProcess.cancelled()) {
                    errorText = httpLoadProcess.getErrorText();
                    if (errorText == null || errorText.length() == 0)
                        errorText = "Unable to load " + httpFile.netPath();
                    setMessage(errorText);
                }
                super.run();
            }
        };
        httpLoadProcess.setSuccessRunnable(httpSuccessRunnable);
        httpLoadProcess.setCancelRunnable(httpCancelRunnable);
        httpLoadProcess.setErrorRunnable(httpErrorRunnable);
        httpLoadProcess.setProgressNotifier(new StatusBarProgressNotifier(WebBuffer.this));
        httpLoadProcess.start();
    }

    private void update(int offset)
    {
        Position pos = getPosition(offset);
        if (pos == null)
            pos = new Position(getFirstLine(), 0);
        setBusy(false);
        update(pos);
    }

    private void update(Position pos)
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this) {
                ed.setDot(pos.copy());
                ed.setMark(null);
                ed.moveCaretToDotCol();
                ed.setTopLine(getFirstLine());
                ed.setUpdateFlag(REFRAME);
                ed.repaintDisplay();
                ed.updateDisplay();
                ed.updateLocation();
            }
        }
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_CHANGED);
        Sidebar.repaintBufferListInAllFrames();
        Editor.currentEditor().status("Loading complete");
    }

    public boolean isTransient()
    {
        if (super.isTransient())
            return true;
        File file = getFile();
        if (file != null && file.isLocal()) {
            if (file.equals(Help.getBindingsFile()))
                return true;
            File dir = file.getParentFile();
            if (dir != null && dir.equals(Help.getDocumentationDirectory()))
                return true;
        }
        return false;
    }

    public boolean save()
    {
        // We shouldn't be trying to save a WebBuffer.
        Debug.bug();
        return true;
    }

    public static void copyLink()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof WebBuffer) {
            Position pos = editor.getDot();
            if (pos != null && pos.getLine() instanceof WebLine) {
                HtmlLineSegment segment =
                    ((WebLine)pos.getLine()).findSegment(pos.getOffset());
                if (segment != null) {
                    Link link = segment.getLink();
                    if (link == null)
                        return;
                    String copy = null;
                    String target = link.getTarget();
                    if (target == null)
                        return;
                    if (target.startsWith("mailto:")) {
                        copy = target;
                    } else {
                        int index = target.indexOf('#');
                        String filename;
                        String ref = null;
                        if (index == 0) {
                            filename = buffer.getFile().netPath();
                            ref = target;
                        } else if (index > 0) {
                            filename = target.substring(0, index);
                            ref = target.substring(index);
                        } else
                            filename = target;
                        final File destination =
                            resolve(buffer.getFile().getParentFile(),
                                filename);
                        if (destination != null) {
                            copy = destination.netPath();
                            if (ref != null)
                                copy = copy.concat(ref);
                        }
                    }
                    if (copy != null) {
                        KillRing killRing = editor.getKillRing();
                        killRing.appendNew(copy);
                        killRing.copyLastKillToSystemClipboard();
                        editor.status("Link copied to clipboard");
                    }
                }
            }
        }
    }
}
