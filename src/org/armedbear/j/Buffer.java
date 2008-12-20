/*
 * Buffer.java
 *
 * Copyright (C) 1998-2007 Peter Graves
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

package org.armedbear.j;

import java.awt.Cursor;
import java.io.BufferedInputStream;
import java.io.BufferedOutputStream;
import java.io.BufferedReader;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.lang.ref.SoftReference;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.zip.GZIPInputStream;
import java.util.zip.GZIPOutputStream;
import java.util.zip.ZipEntry;
import javax.swing.Icon;
import javax.swing.SwingUtilities;
import javax.swing.undo.CannotRedoException;
import javax.swing.undo.CannotUndoException;
import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;

public class Buffer extends SystemBuffer
{
    private static int untitledCount;

    protected boolean isUntitled;

    protected Formatter formatter;

    protected String title;

    private boolean needsParsing;

    boolean needsRenumbering;

    public final boolean needsRenumbering()
    {
        return needsRenumbering;
    }

    public final void needsRenumbering(boolean b)
    {
        needsRenumbering = b;
    }

    private int visibleLineCount;

    protected boolean supportsUndo = true;

    public final boolean supportsUndo()
    {
        return supportsUndo;
    }

    private int modCount;
    private int saveModCount; // Value of modCount when last saved.

    // Autosave.
    protected boolean autosaveEnabled;
    private File autosaveFile;
    private int autosaveModCount; // Value of modCount when last autosaved.

    private File cache;
    private String listing;
    public final String getListing()
    {
        return listing;
    }
    public final void setListing(String s)
    {
        this.listing = s;
    }

    private View lastView;

    private boolean backedUp = false; // Ignored for local buffers.

    private int fileType = FILETYPE_UNKNOWN;

    private Compression compression;

    public final Compression getCompression()
    {
        return compression;
    }

    public final void setCompression(Compression compression)
    {
        this.compression = compression;
    }

    protected PropertyList properties = new PropertyList();

    private BackgroundProcess backgroundProcess;

    private Mutex mutex = new Mutex();
    private final ReadWriteLock rwlock = new ReadWriteLock();

    private boolean isNewFile;

    protected Buffer parentBuffer;

    public final Buffer getParentBuffer()
    {
        return parentBuffer;
    }

    public final void setParentBuffer(Buffer b)
    {
        this.parentBuffer = b;
    }

    private Position mark;

    public final Position getMark()
    {
        return mark;
    }

    public final void setMark(Position pos)
    {
        mark = pos;
    }

    public boolean isPrimary()
    {
        return true;
    }

    public boolean isSecondary()
    {
        return false;
    }

    public boolean isPaired()
    {
        return isSecondary() || getSecondary() != null;
    }

    public Buffer getPrimary()
    {
        return null;
    }

    public Buffer getSecondary()
    {
        return null;
    }

    public float getSplit()
    {
        return 0.5F;
    }

    public void promote()
    {
    }

    public final boolean isNewFile()
    {
        return isNewFile;
    }

    private final void setNewFile(boolean b)
    {
        isNewFile = b;
    }

    protected Buffer()
    {
        // Add new buffer to global buffer list.
        Editor.getBufferList().add(this);
    }

    // Called only by Editor.newBuffer().
    public Buffer(int i /*ignored*/)
    {
        this();
        Debug.assertTrue(Editor.getBufferList().contains(this));
        initializeUndo();
        type = TYPE_NORMAL;
        isUntitled = true;
        ++untitledCount;
        String name = "Untitled-" + untitledCount;
        File directory = Editor.currentEditor().getCurrentDirectory();
        if (directory == null || directory.isRemote())
            directory = Directories.getUserHomeDirectory();
        setFile(File.getInstance(directory, name));
        autosaveEnabled = true;
        lineSeparator = System.getProperty("line.separator");
        mode = PlainTextMode.getMode();
        formatter = mode.getFormatter(this);
        setNewFile(true);
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return; // Shouldn't happen.
        }
        try {
            appendLine("");
            renumber();
            setLoaded(true);
        }
        finally {
            unlockWrite();
        }
    }

    public Buffer(File file)
    {
        this();
        Debug.assertTrue(Editor.getBufferList().contains(this));
        initializeUndo();
        setFile(file);
        type = TYPE_NORMAL;
        autosaveEnabled = true;
    }

    public static Buffer createBuffer(File file)
    {
        if (file instanceof FtpFile) {
            FtpSession session = FtpSession.getSession((FtpFile)file);
            if (session == null)
                return null;
            return new RemoteBuffer((FtpFile)file, session);
        }
        if (file instanceof HttpFile) {
            if (Editor.getModeList().modeAccepts(IMAGE_MODE, file.getName()))
                return new RemoteBuffer(file);
            if (Editor.preferences().getBooleanProperty(Property.ENABLE_WEB)) {
                int modeId =
                    Editor.getModeList().getModeIdForFileName(file.getName());
                if (modeId < 0 || modeId == HTML_MODE)
                    return WebBuffer.createWebBuffer(file, null, null);
            }
            return new RemoteBuffer(file);
        }
        if (file instanceof SshFile) {
            SshFile sshFile = (SshFile) file;
            SshSession session = SshSession.getSession(sshFile);
            if (session == null)
                return null;
            if (!session.isLocked()) {
                Debug.bug();
                return null;
            }
            session.checkLogin();
            if (sshFile.getUserName() == null) {
                Debug.bug();
                sshFile.setUserName(session.getUserName());
            }
            if (!sshFile.getUserName().equals(session.getUserName())) {
                Debug.bug();
                session.unlock();
                return null;
            }
            session.unlock();
            return new RemoteBuffer(file);
        }
        // Special case for unsent messages.
        File dir = file.getParentFile();
        if (dir != null && dir.equals(Directories.getDraftsFolder())) {
            Mode sendMailMode = Editor.getModeList().getMode(SEND_MAIL_MODE);
            if (sendMailMode != null)
                return sendMailMode.createBuffer(file);
        }
        // Local file.
        return createBuffer(file, null, null);
    }

    protected static Buffer createBuffer(File file, File cache, String listing)
    {
        Compression compression = null;
        int fileType = Utilities.getFileType(cache != null ? cache : file);
        if (fileType == FILETYPE_GZIP) {
            // If we're looking at a remote file, gunzip the cached copy
            // of it; otherwise, gunzip the file itself into the cache.
            File uncompressed = cacheGZIP(cache != null ? cache : file);
            if (uncompressed != null) {
                cache = uncompressed;
                fileType = Utilities.getFileType(cache);
                compression = new Compression(COMPRESSION_GZIP);
            } else
                fileType = FILETYPE_BINARY; // Something went wrong.
        }
        if (fileType == FILETYPE_JPEG ||
            Editor.getModeList().modeAccepts(IMAGE_MODE, file.getName())) {
            Buffer buffer =
                ImageBuffer.createImageBuffer(file, cache, listing);
            if (buffer != null) {
                buffer.setFileType(fileType);
                return buffer;
            }
        }
        // Normal case.
        Buffer buffer = new Buffer(file);
        Debug.assertTrue(Editor.getBufferList().contains(buffer));
        buffer.setFileType(fileType);
        buffer.setCache(cache);
        buffer.setListing(listing);
        buffer.setCompression(compression);
        if (file.isLocal() && !file.isFile())
            buffer.setNewFile(true);
        return buffer;
    }

    // For Session.createBuffers().
    public static Buffer precreateBuffer(File file)
    {
        if (file == null) {
            Debug.bug();
            return null;
        }
        if (file.isRemote()) {
            Debug.bug();
            return null;
        }
        // Special case for unsent messages.
        File dir = file.getParentFile();
        if (dir != null && dir.equals(Directories.getDraftsFolder())) {
            Mode sendMailMode = Editor.getModeList().getMode(SEND_MAIL_MODE);
            if (sendMailMode != null)
                return sendMailMode.createBuffer(file);
        }
        // Normal case.
        return new Buffer(file);
    }

    private boolean initialized;

    public synchronized boolean initialized()
    {
        return initialized;
    }

    public synchronized void setInitialized(boolean b)
    {
        initialized = b;
    }

    public synchronized void initialize()
    {
        Debug.assertTrue(!initialized);
        final File file = getFile();
        if (fileType == FILETYPE_UNKNOWN) {
            fileType = Utilities.getFileType(cache != null ? cache : file);
            if (fileType == FILETYPE_GZIP) {
                // If we're looking at a remote file, gunzip the cached copy
                // of it; otherwise, gunzip the file itself into the cache.
                File uncompressed = cacheGZIP(cache != null ? cache : file);
                if (uncompressed != null) {
                    cache = uncompressed;
                    fileType = Utilities.getFileType(cache);
                    compression = new Compression(COMPRESSION_GZIP);
                } else
                    fileType = FILETYPE_BINARY; // Something went wrong.
            }
        }
        mode = getDefaultMode();
        formatter = mode.getFormatter(this);
        if (fileType == FILETYPE_ZIP) {
            supportsUndo = false;
            type = TYPE_ARCHIVE;
            readOnly = true;
        } else if (fileType == FILETYPE_BINARY) {
            readOnly = true;
        } else if (fileType == FILETYPE_WORD) {
            readOnly = true;
        } else if (file != null) {
            FileHistoryEntry entry =
                FileHistory.getFileHistory().findEntry(file.netPath());
            if (entry != null) {
                // Set encoding.
                final String encoding = entry.getEncoding();
                if (encoding != null && Utilities.isSupportedEncoding(encoding))
                    file.setEncoding(encoding);
                // Set mode.
                mode =
                    Editor.getModeList().getModeFromModeName(entry.getMode());
                if (mode == null)
                    mode = Editor.getModeList().getMode(PLAIN_TEXT_MODE);
                else if (mode.getId() == BINARY_MODE)
                    readOnly = true;
                formatter = mode.getFormatter(this);
                // Properties from FileHistoryEntry override defaults set by
                // mode.
                properties.putAll(entry.getProperties());
            }
        }
        if (file != null) {
            if (file.getProtocol() == File.PROTOCOL_HTTP ||
                file.getProtocol() == File.PROTOCOL_HTTPS)
                readOnly = true;
        }
        initialized = true;
    }

    public Mode getDefaultMode()
    {
        final File file = getFile();
        final ModeList modeList = Editor.getModeList();
        switch (fileType) {
            case FILETYPE_XML:
                return modeList.getMode(XML_MODE);
            case FILETYPE_SHELLSCRIPT: {
                Mode m = grovelModeFromFile(file);
                if (m != null)
                    return m;
                return modeList.getMode(SHELL_SCRIPT_MODE);
            }
            case FILETYPE_PERL:
                return modeList.getMode(PERL_MODE);
            case FILETYPE_PHP:
                return modeList.getMode(PHP_MODE);
            case FILETYPE_ZIP:
                return modeList.getMode(ARCHIVE_MODE);
            case FILETYPE_GZIP:
            case FILETYPE_BINARY:
                return modeList.getMode(BINARY_MODE);
            case FILETYPE_JPEG:
                return modeList.getMode(IMAGE_MODE);
            case FILETYPE_WORD:
                return modeList.getMode(WORD_MODE);
            case FILETYPE_TEXT:
            default: {
                Mode m = grovelModeFromFile(file);
                if (m == null) {
                    if (compression != null && compression.getType() == COMPRESSION_ZIP) {
                        String entryName = compression.getEntryName();
                        if (entryName != null)
                            m = getModeForFileName(entryName);
                    } else if (file != null) {
                        m = getModeForFileName(file.getName());
                    }
                    if (m != null && m.getId() == IMAGE_MODE) {
                        if (fileType == FILETYPE_TEXT)
                            m = modeList.getMode(PLAIN_TEXT_MODE);
                        else
                            m = modeList.getMode(BINARY_MODE);
                    } else if (m == null) {
                        if (file != null) {
                            if (file.getProtocol() == File.PROTOCOL_HTTP ||
                                file.getProtocol() == File.PROTOCOL_HTTPS)
                                m = modeList.getMode(HTML_MODE);
                        }
                        if (m == null)
                            m = modeList.getMode(PLAIN_TEXT_MODE);
                    }
                }
                return m;
            }
        }
    }

    private static final Mode grovelModeFromFile(File file)
    {
        if (file == null)
            return null;
        if (!file.isLocal())
            return null;
        if (!file.isFile())
            return null;
        Mode mode = null;
        try {
            BufferedReader reader =
                new BufferedReader(new InputStreamReader(file.getInputStream()));
            String s = reader.readLine();
            if (s != null) {
                mode = grovelModeFromString(s);
                if (mode == null && s.startsWith("#!")) {
                    // Consider second line too.
                    s = reader.readLine();
                    if (s != null)
                        mode = grovelModeFromString(s);
                }
            }
            reader.close();
        }
        catch (IOException e) {
            Log.error(e);
        }
        return mode;
    }

    private static final Mode grovelModeFromString(String s)
    {
        if (s != null) {
            int begin = s.indexOf("-*-");
            if (begin >= 0) {
                s = s.substring(begin + 3);
                int end = s.indexOf("-*-");
                if (end >= 0) {
                    s = s.substring(0, end).trim().toLowerCase();
                    int index = s.indexOf("mode:");
                    String modeName;
                    if (index < 0) {
                        // "-*- Lisp -*-"
                        modeName = s;
                    } else {
                        // "-*- Mode: LISP; Syntax: ANSI-Common-Lisp; Base: 10 -*-"
                        s = s.substring(5).trim();
                        for (end = 0; end < s.length(); end++) {
                            char c = s.charAt(end);
                            if (c == ' ' || c == '\t' || c == ';')
                                break;
                        }
                        modeName = s.substring(0, end);
                    }
                    return Editor.getModeList().getModeFromModeName(modeName);
                }
            }
        }
        return null;
    }

    // Locking.
    public final boolean isInUse()
    {
        return mutex.isInUse();
    }

    public void acquire() throws InterruptedException
    {
        mutex.acquire();
    }

    public synchronized void release()
    {
        mutex.release();
    }

    public boolean attempt() throws InterruptedException
    {
        return mutex.attempt();
    }

    public boolean attempt(long msecs) throws InterruptedException
    {
        return mutex.attempt(msecs);
    }

    public final boolean isLocked()
    {
        return isInUse();
    }

    public synchronized boolean lock()
    {
        try {
            return attempt();
        }
        catch (InterruptedException e) {
            return false;
        }
    }

    public synchronized void unlock()
    {
        release();
    }

    public final void lockRead() throws InterruptedException
    {
        rwlock.lockRead();
    }

    public final void unlockRead()
    {
        rwlock.unlockRead();
    }

    public final void lockWrite() throws InterruptedException
    {
        rwlock.lockWrite();
    }

    public final void unlockWrite()
    {
        rwlock.unlockWrite();
    }

    public final boolean isWriteLocked()
    {
        return rwlock.isWriteLocked();
    }

    public boolean isVisible()
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();)
            if (it.nextEditor().getBuffer() == this)
                return true;

        return false;
    }

    public void setWaitCursor()
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this)
                ed.setWaitCursor();
        }
    }

    public void setDefaultCursor()
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this)
                ed.setDefaultCursor();
        }
    }

    public final PropertyList getProperties()
    {
        return properties;
    }

    public final void setCache(File file)
    {
        cache = file;
    }

    public final File getCache()
    {
        return cache;
    }

    public final Formatter getFormatter()
    {
        return formatter;
    }

    public final void setFormatter(Formatter formatter)
    {
        this.formatter = formatter;
    }

    public boolean isReadOnly()
    {
        return readOnly || forceReadOnly;
    }

    private boolean busy;

    public synchronized final boolean isBusy()
    {
        return busy;
    }

    public synchronized final void setBusy(boolean b)
    {
        busy = b;
    }

    public final BackgroundProcess getBackgroundProcess()
    {
        return backgroundProcess;
    }

    public final void setBackgroundProcess(BackgroundProcess backgroundProcess)
    {
        this.backgroundProcess = backgroundProcess;
    }

    public File getCurrentDirectory()
    {
        return getFile() != null ? getFile().getParentFile() : Directories.getUserHomeDirectory();
    }

    // Subclasses should override this method if appropriate!
    public File getCompletionDirectory()
    {
        final File file = getFile();
        if (file != null) {
            if (file.isLocal() || file instanceof SshFile)
                return file.isDirectory() ? file : file.getParentFile();
        }
        return Directories.getUserHomeDirectory();
    }

    public View getInitialView()
    {
        View view = new View();
        view.setDot(getInitialDotPos());
        return view;
    }

    private int initialLineNumber;
    private int initialOffset;

    public Position getInitialDotPos()
    {
        Line line = getLine(initialLineNumber);
        if (line == null) {
            line = getFirstLine();
            return line != null ? new Position(line, 0) : null;
        }
        return new Position(line, Math.min(initialOffset, line.length()));
    }

    public void setInitialDotPos(int lineNumber, int offset)
    {
        initialLineNumber = lineNumber;
        initialOffset = offset;
    }

    private long lastActivated;

    public final long getLastActivated()
    {
        return lastActivated;
    }

    public final void setLastActivated(long l)
    {
        lastActivated = l;
    }

    public final int getLineCount()
    {
        return lineCount;
    }

    public final int getModCount()
    {
        return modCount;
    }

    public final synchronized void setModCount(int count)
    {
        if (count != modCount) {
            modCount = count;
            srText = null;
        }
    }

    public final synchronized void incrementModCount()
    {
        ++modCount;
        srText = null;
    }

    public final void setModCountWhenLastSaved(int count)
    {
        autosaveModCount = count;
    }

    public final KeyMap getKeyMapForMode()
    {
        // Should never return null.
        return mode.getKeyMap();
    }

    public final int getFileType()
    {
        return fileType;
    }

    private final void setFileType(int fileType)
    {
        this.fileType = fileType;
    }

    private static File cacheGZIP(File f)
    {
        try {
            File tempFile = Utilities.getTempFile();
            if (tempFile != null) {
                InputStream in = new GZIPInputStream(f.getInputStream());
                if (in != null) {
                    OutputStream out = tempFile.getOutputStream();
                    byte[] buf = new byte[4096];
                    int bytesRead;
                    while ((bytesRead = in.read(buf)) > 0)
                        out.write(buf, 0, bytesRead);
                    out.close();
                    in.close();
                    return tempFile;
                }
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        return null;
    }

    // Handles all the paperwork when we rename a buffer.
    public void changeFile(File f)
    {
        if (f == null)
            return;
        String newName = f.canonicalPath();
        if (newName == null)
            return;
        File oldFile = getFile();
        String oldName = oldFile != null ? oldFile.canonicalPath() : null;
        setFile(f);
        setCompression(null);
        type = TYPE_NORMAL;
        title = null;
        Mode oldMode = mode;
        if (oldMode == PlainTextMode.getMode()) {
            setModeFromFilename(newName);
            if (mode != oldMode) {
                // Mode has changed.
                needsParsing = true;

                // Make sure we parse the buffer before we display it.
                if (formatter != null)
                    formatter.parseBuffer();
            }
        }
        readOnly = false;
        isUntitled = false;
        if (oldName != null)
            Autosave.rename(oldName, newName);
        Editor.getBufferList().modified();
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_CHANGED);
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor editor = it.nextEditor();
            if (editor.getBuffer() == this) {
                editor.updateLocation();
                editor.getDisplay().repaint();
            }
        }
    }

    protected void setModeFromFilename(String filename)
    {
        mode = Editor.getModeList().getModeForFileName(filename);
        if (mode == null)
            mode = Editor.getModeList().getMode(PLAIN_TEXT_MODE);
        formatter = mode.getFormatter(this);
    }

    private static Mode getModeForFileName(String filename)
    {
        if (filename.endsWith(".gz"))
            filename = filename.substring(0, filename.length() - 3);

        // Strip path prefix (if any).
        // Look for '/' on both Windows and Unix (filename might be a ZipEntry name).
        int index = filename.lastIndexOf('/');
        if (index >= 0)
            filename = filename.substring(index + 1);

        // Look for '\' on Windows only.
        if (Platform.isPlatformWindows()) {
            index = filename.lastIndexOf('\\');
            if (index >= 0)
                filename = filename.substring(index + 1);
        }

        return Editor.getModeList().getModeForFileName(filename);
    }

    public final void setMode(Mode mode)
    {
        this.mode = mode;
        formatter = mode.getFormatter(this);
    }

    public void changeMode(Mode newMode)
    {
        final int oldModeId = mode.getId();
        if (oldModeId == DIRECTORY_MODE)
            return;
        final int newModeId = newMode.getId();
        if (newModeId != oldModeId) {
            // Must reload buffer if changing into or out of binary mode.
            boolean reloading = newModeId == BINARY_MODE || oldModeId == BINARY_MODE;
            mode = newMode;
            formatter = mode.getFormatter(this);
            if (reloading) {
                reload();
                if (newModeId == IMAGE_MODE)
                    return;
            }

            if (newModeId == BINARY_MODE) {
                readOnly = true;
            } else {
                final File file = getFile();
                if (file != null && !file.isRemote() && file.isFile())
                    readOnly = !file.canWrite();
            }

            formatter.parseBuffer();

            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this) {
                    if (reloading) {
                        ed.setDot(getFirstLine(), 0);
                        ed.setMark(null);
                        ed.setTopLine(getFirstLine());
                        ed.moveCaretToDotCol();
                        ed.updateLocation();
                    }
                    ed.setUpdateFlag(REPAINT);
                    ed.updateDisplay();
                }
            }
        }
    }

    public final String getCommentStart()
    {
        return mode.getCommentStart();
    }

    public final String getCommentEnd()
    {
        return mode.getCommentEnd();
    }

    private long lastModified;

    public final long getLastModified()
    {
        return lastModified;
    }

    public final void setLastModified(long lastModified)
    {
        this.lastModified = lastModified;
    }

    protected void loadFile(File toBeLoaded)
    {
        try {
            int modeId = getModeId();
            if (modeId == ARCHIVE_MODE || modeId == WORD_MODE || modeId == XML_MODE)
                mode.loadFile(this, toBeLoaded);
            else {
                final String encoding = toBeLoaded.getEncoding();
                load(toBeLoaded.getInputStream(), encoding);
                if (encoding != null)
                    saveProperties(); // Remember encoding for next time.
            }
        }
        catch (IOException e) {
            Log.error(e);
        }
        // Handle zero length files.
        if (getFirstLine() == null) {
            appendLine("");
            lineSeparator = System.getProperty("line.separator");
        }
        final File file = getFile();
        if (file != null && file.getProtocol() != File.PROTOCOL_HTTP)
            lastModified = toBeLoaded.lastModified();
        renumberOriginal();
    }

    public int load()
    {
        if (!isLoaded()) {
            try {
                lockWrite();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return LOAD_FAILED;
            }
            try {
                final File file = getFile();
                final File toBeLoaded = cache != null ? cache : file;
                if (toBeLoaded.isFile()) {
                    Editor editor = Editor.currentEditor();
                    FastStringBuffer sb = new FastStringBuffer("Loading");
                    if (compression != null) {
                        if (compression.getType() == COMPRESSION_ZIP) {
                            String entryName = compression.getEntryName();
                            if (entryName != null) {
                                sb.append(' ');
                                sb.append(entryName);
                            }
                        }
                    } else if (file != null) {
                        sb.append(' ');
                        sb.append(file.getName());
                    }
                    sb.append("...");
                    editor.status(sb.toString());
                    Debug.assertTrue(mode != null);
                    Debug.assertTrue(toBeLoaded != null);
                    loadFile(toBeLoaded);
                    // At this point, if we loaded from a cache, lastModified will
                    // be set based on the cache file, which is not what we want
                    // in the case of a local file.
                    if (toBeLoaded == cache && file != null && !file.isRemote())
                        lastModified = file.lastModified();
                    formatter.parseBuffer();
                    checkCVS();
                    sb.append("done");
                    editor.status(sb.toString());
                } else {
                    // File doesn't exist.
                    if (getFirstLine() == null) {
                        appendLine("");
                        lineSeparator = System.getProperty("line.separator");
                    }
                    renumberOriginal();
                    setModeFromFilename(file.canonicalPath());
                    setLoaded(true);
                }
            }
            catch (OutOfMemoryError e) {
                _empty();
                throw e;
            }
            finally {
                unlockWrite();
            }
        }
        return LOAD_COMPLETED;
    }

    private void reloadSucceeded()
    {
        Debug.assertTrue(!rwlock.isWriteLocked());
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() != this)
                continue;
            View view = ed.getView(this);
            if (view != null) {
                Line dotLine = getLine(view.getDotLineNumber());
                if (dotLine == null)
                    dotLine = getFirstLine();
                if (dotLine != null) {
                    ed.setDot(dotLine, 0);
                    ed.moveCaretToDotCol();
                }
                Line topLine = getLine(view.getTopLineNumber());
                if (topLine == null)
                    topLine = dotLine;
                ed.setTopLine(topLine);
            } else {
                ed.setDot(getFirstLine(), 0);
                ed.moveCaretToDotCol();
                ed.setTopLine(getFirstLine());
            }
            ed.setUpdateFlag(REPAINT);
            ed.updateDisplay();
        }
        // The buffer is unmodified now, so we need to update its icon in the
        // buffer list(s).
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_REPAINT_BUFFER_LIST);
    }

    private void reloadFailed()
    {
        MessageDialog.showMessageDialog("Reload failed", "Error");
    }

    public void reload()
    {
        final File file = getFile();
        if (file == null)
            return;
        switch (file.getProtocol()) {
            case File.PROTOCOL_FTP:
                reloadFtp((FtpFile) file);
                return;
            case File.PROTOCOL_HTTP:
            case File.PROTOCOL_HTTPS:
                reloadHttp((HttpFile) file);
                return;
            case File.PROTOCOL_FILE:
                if (file.isFile()) {
                    // Local file.
                    if (getModeId() == ARCHIVE_MODE) {
                        empty();
                        resetUndo();
                        mode.loadFile(this, file);
                    } else
                        reloadLocal(file);
                }
                reloadSucceeded();
                return;
            default:
                Debug.assertTrue(false);
                break;
        }
    }

    // Asynchronous.
    private void reloadFtp(FtpFile file)
    {
        Log.debug("reloadFtp");
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        FtpSession session = FtpSession.getSession(file);
        final FtpLoadProcess ftpLoadProcess = new FtpLoadProcess(this, file, session);
        Runnable successRunnable = new Runnable() {
            public void run()
            {
                File newCache = ftpLoadProcess.getCache();
                if (newCache != null) {
                    Log.debug("newCache != null");
                    if (cache != null && cache.isFile())
                        cache.delete();
                    cache = newCache;
                    reloadLocal(cache);
                } else {
                    // User cancelled.
                    setLoaded(true);
                }
                setBusy(false);
                reloadSucceeded();
            }
        };
        ErrorRunnable errorRunnable = new ErrorRunnable("Reload failed") {
            public void run()
            {
                setBusy(false);
                reloadFailed();
            }
        };
        ftpLoadProcess.setProgressNotifier(new StatusBarProgressNotifier(this));
        ftpLoadProcess.setSuccessRunnable(successRunnable);
        ftpLoadProcess.setErrorRunnable(errorRunnable);
        ftpLoadProcess.start();
    }

    private void reloadHttp(HttpFile file)
    {
        Log.debug("reloadHttp");
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        final HttpLoadProcess httpLoadProcess = new HttpLoadProcess(this, file);
        Runnable successRunnable = new Runnable() {
            public void run()
            {
                File newCache = httpLoadProcess.getCache();
                if (newCache != null) {
                    Log.debug("newCache != null");
                    if (cache != null && cache.isFile())
                        cache.delete();
                    cache = newCache;
                    reloadLocal(cache);
                } else {
                    // User cancelled.
                    setLoaded(true);
                }
                setBusy(false);
                reloadSucceeded();
            }
        };
        ErrorRunnable errorRunnable = new ErrorRunnable("Reload failed") {
            public void run()
            {
                setBusy(false);
                reloadFailed();
            }
        };
        httpLoadProcess.setProgressNotifier(new StatusBarProgressNotifier(this));
        httpLoadProcess.setSuccessRunnable(successRunnable);
        httpLoadProcess.setErrorRunnable(errorRunnable);
        setBusy(true);
        new Thread(httpLoadProcess).start();
    }

    private void reloadLocal(File file)
    {
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            empty();
            loadFile(file);
            formatter.parseBuffer();
        }
        finally {
            unlockWrite();
        }
        unmodified();
        deleteAutosaveFile();
        resetUndo();
    }

    public synchronized void kill()
    {
        BufferList bufferList = Editor.getBufferList();
        if (!bufferList.contains(this)) {
            Debug.bug("buffer.kill() buffer not in list");
            return;
        }

        Marker.invalidateMarkers(this);

        Buffer buf = bufferList.getPreviousPrimaryBuffer(this);
        if (buf != null && buf.isPaired()) {
            Buffer secondary = buf.getSecondary();
            if (secondary != null) {
                if (secondary.getLastActivated() > buf.getLastActivated())
                    buf = secondary;
            }
        }
        if (buf == null)
            buf = new Directory(getCurrentDirectory());
        // Copy editor list since switchToBuffer() may close an editor.
        ArrayList editors = new ArrayList();
        for (EditorIterator it = new EditorIterator(); it.hasNext();)
            editors.add(it.next());
        for (Iterator it = editors.iterator(); it.hasNext();) {
            Editor ed = (Editor) it.next();
            // Skip editor if it has been closed.
            if (Editor.getEditorList().contains(ed)) {
                if (ed.getBuffer() == this)
                    ed.switchToBuffer(buf);
                ed.removeView(this);
            }
        }
        deleteAutosaveFile();
        bufferList.remove(this);
        dispose();
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_BUFFER_LIST_CHANGED |
            SIDEBAR_MODIFIED_BUFFER_COUNT);
    }

    public synchronized void relink()
    {
        BufferList bufferList = Editor.getBufferList();
        if (bufferList.contains(this)) {
            Debug.bug();
            return;
        }
        bufferList.add(this);
    }

    private Thread startTaggerThread(int priority)
    {
        if (mode != null) {
            Tagger tagger = mode.getTagger(this);
            if (tagger != null) {
                Thread thread = new Thread(tagger);
                thread.setPriority(priority);
                thread.setDaemon(true);
                thread.setName("tagger " + getFile().getName());
                thread.start();
                return thread;
            }
        }
        return null;
    }

    public boolean isTaggable()
    {
        final File file = getFile();
        if (file == null)
            return false;
        if (file.isRemote())
            return false;
        return mode.isTaggable();
    }

    // Runs tagger if tags == null.
    public List getTags(boolean update)
    {
        List tags = getTags();
        if (tags != null)
            return tags;
        if (mode != null) {
            Tagger tagger = mode.getTagger(this);
            if (tagger != null)
                tagger.run();
        }
        return getTags();
    }

    public boolean saveToCache()
    {
        if (lineSeparator == null)
            lineSeparator = System.getProperty("line.separator");
        final File tempFile = Utilities.getTempFile();
        if (tempFile != null) {
            File file = getFile();
            if (file != null)
                tempFile.setEncoding(file.getEncoding());
            if (writeFile(tempFile)) {
                final File oldCache = cache;
                cache = tempFile;
                if (oldCache != null && oldCache.isFile())
                    oldCache.delete();
                return true;
            }
        }
        return false;
    }

    private boolean maybeWriteBackupFromCache()
    {
        if (cache == null) {
            Log.error("maybeWriteBackupFromCache cache is null");
            return false;
        }
        if (!cache.isFile()) {
            Log.error("maybeWriteBackupFromCache cache is not a file");
            return false;
        }
        if (!cache.canRead()) {
            Log.error("maybeWriteBackupFromCache cache is not readable");
            return false;
        }
        if (backedUp)
            return true; // We only want to do the backup once!
        File file = getFile();
        if (file == null) {
            Debug.bug();
            return false;
        }
        String name = file.getName();
        if (compression != null && compression.getType() == COMPRESSION_GZIP) {
            if (name.endsWith(".gz"))
                name = name.substring(0, name.length() - 3);
        }
        return backedUp =
            Utilities.makeBackup(cache, name, false);
    }

    public boolean save()
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        if (!isModified())
            return true;
        addUndoBoundary();
        boolean succeeded = false;
        final File file = getFile();
        if (file != null) {
            if (file.isLocal())
                succeeded = saveLocal(file);
            if (file instanceof FtpFile)
                succeeded = saveFtp();
            if (file instanceof SshFile)
                succeeded = saveSsh((SshFile)file);
        }
        if (succeeded && Editor.isLispInitialized())
            LispAPI.invokeAfterSaveHook(this);
        return succeeded;
    }

    private boolean saveLocal(final File file)
    {
        Debug.assertTrue(file.isLocal());

        if (compression != null && compression.getType() == COMPRESSION_GZIP)
            return saveLocalCompressed(file);

        try {
            writeBuffer();
            renumberOriginal();
            saved();
            lastModified = file.lastModified();
        }
        catch (SaveException e) {
            final String where = "Save";
            String message = e.getMessage();
            // Tell user exactly what error occurred.
            if (message != null)
                MessageDialog.showMessageDialog(message, where);
            // Display summary message.
            message = "Unable to save " + file.canonicalPath();
            MessageDialog.showMessageDialog(message, where);
            return false;
        }

        if (isTaggable())
            Editor.getTagFileManager().addToQueue(file.getParentFile(), mode);
        startTaggerThread(Thread.MIN_PRIORITY);
        boolean repaint = false;
        final ModeList modeList = Editor.getModeList();
        if (file.equals(Preferences.getPreferencesFile())) {
            // Reload preferences.
            Editor.loadPreferences();
            if (Editor.preferences().getBooleanProperty(Property.AUTO_RELOAD_KEY_MAPS)) {
                // Reload keymaps.
                KeyMap.reloadKeyMaps();
            }
            repaint = true;
        } else {
            if (Editor.preferences().getBooleanProperty(Property.AUTO_RELOAD_KEY_MAPS)) {
                // Reload mode-specific keymap(s) if modified.
                synchronized (modeList) {
                    for (Iterator it = modeList.iterator(); it.hasNext();) {
                        ModeListEntry entry = (ModeListEntry) it.next();
                        Mode mode = entry.getMode(false);
                        if (mode != null) {
                            if (file.equals(mode.getKeyMapFile()))
                                mode.deleteKeyMap();
                        }
                    }
                }
                // Global keymap.
                if (file.equals(KeyMap.getGlobalKeyMapFile()))
                    KeyMap.deleteGlobalKeyMap();
            }
        }
        // Reload theme if modified.
        String theme = Editor.preferences().getStringProperty(Property.THEME);
        if (theme != null) {
            if (Utilities.isFilenameAbsolute(theme)) {
                if (file.canonicalPath().equals(File.getInstance(theme).canonicalPath())) {
                    Editor.loadPreferences();
                    repaint = true;
                }
            } else if (file.getName().equals(theme)) {
                Editor.loadPreferences();
                repaint = true;
            }
        }

        // Reload aliases if modified.
        if (file.equals(Editor.getAliasesFile()))
            Editor.reloadAliases();

        // Update listRegisters buffer (if any).
        File parent = file.getParentFile();
        if (parent != null && parent.equals(Directories.getRegistersDirectory())) {
            Buffer buf = Registers.findListRegistersBuffer();
            if (buf != null)
                buf.reload();
        }

        checkCVS();

        if (repaint) {
            // Force formatters to be re-initialized.
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf.getFormatter() != null)
                    buf.getFormatter().reset();
            }
            Display.initializeStaticValues();
            for (int i = 0; i < Editor.getFrameCount(); i++) {
                Frame f = Editor.getFrame(i);
                if (f != null) {
                    f.resetDisplay();
                    f.repaint();
                }
            }
            Editor.restoreFocus();
        }
        return true;
    }

    private boolean saveLocalCompressed(File file)
    {
        final String dialogTitle = "Save";
        final Editor editor = Editor.currentEditor();
        // Do this before saving changes to cache!
        if (!maybeWriteBackupFromCache()) {
            FastStringBuffer sb =
                new FastStringBuffer("Unable to write backup file for ");
            sb.append(file.getName());
            sb.append(". Save anyway?");
            if (!editor.confirm(dialogTitle, sb.toString()))
                return false;
        }
        if (!saveToCache()) {
            FastStringBuffer sb =
                new FastStringBuffer("Unable to write temporary file for ");
            sb.append(file.getName());
            MessageDialog.showMessageDialog(sb.toString(), dialogTitle);
            return false;
        }
        if (!compress(cache, file))
            return false;
        saved();
        lastModified = file.lastModified();
        return true;
    }

    private boolean compress(File source, File destination)
    {
        if (source == null) {
            Debug.bug();
            return false;
        }
        if (destination == null) {
            Debug.bug();
            return false;
        }
        if (!source.isFile()) {
            Debug.bug();
            return false;
        }
        final File tempFile = Utilities.getTempFile(destination.getParentFile());
        try {
            final int bufSize = 4096;
            BufferedInputStream in =
                new BufferedInputStream(source.getInputStream());
            GZIPOutputStream out =
                new GZIPOutputStream(new BufferedOutputStream(tempFile.getOutputStream()),
                                     bufSize);
            byte[] buffer = new byte[bufSize];
            while (true) {
                int bytesRead = in.read(buffer, 0, bufSize);
                if (bytesRead > 0)
                    out.write(buffer, 0, bytesRead);
                else
                    break;
            }
            in.close();
            out.flush();
            out.close();
            return Utilities.deleteRename(tempFile, destination);
        }
        catch (IOException e) {
            Log.error(e);
            return false;
        }
    }

    private boolean saveFtp()
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        Debug.assertTrue(getFile() instanceof FtpFile);
        final FtpFile file = (FtpFile) getFile();
        final FtpSession session = FtpSession.getSession(file);
        if (session == null)
            return false;
        Debug.assertTrue(session.isLocked());
        if (!lock()) {
            session.unlock();
            MessageDialog.showMessageDialog("Buffer is busy", file.netPath());
            return false;
        }
        Debug.assertTrue(isLocked());
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        // Do this before saving changes to cache!
        if (!maybeWriteBackupFromCache()) {
            editor.setDefaultCursor();
            String message = "Unable to write backup file for " +
                file.getName() + ". Save anyway?";
            if (!editor.confirm("Save", message)) {
                unlock();
                session.unlock();
                return false;
            }
            editor.setWaitCursor();
        }
        if (!saveToCache()) {
            unlock();
            session.unlock();
            editor.setDefaultCursor();
            String message = "Unable to write temporary file for " +
                file.getName();
            MessageDialog.showMessageDialog(message, "Save");
            return false;
        }
        final FtpSaveProcess saveProcess;
        if (compression != null && compression.getType() == COMPRESSION_GZIP) {
            final File tempFile = Utilities.getTempFile();
            if (!compress(cache, tempFile)) {
                String message = "Unable to compress temporary file for " +
                    file.getName();
                MessageDialog.showMessageDialog(message, "Save");
                return false;
            }
            saveProcess = new FtpSaveProcess(this, tempFile, file, session);
        } else
            saveProcess = new FtpSaveProcess(this, cache, file, session);
        saveProcess.setConfirmIfDestinationChanged(true);
        saveProcess.setTitle("Save");
        final Runnable successRunnable = new Runnable() {
            public void run()
            {
                saved();
                setListing(saveProcess.getListing());
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == Buffer.this)
                        ed.setDefaultCursor();
                }
                Sidebar.repaintBufferListInAllFrames();
            }
        };
        saveProcess.setSuccessRunnable(successRunnable);
        Debug.assertTrue(isLocked());
        saveProcess.start();
        return true;
    }

    private boolean saveSsh(final SshFile file)
    {
        final String title = "Save";
        final Editor editor = Editor.currentEditor();
        boolean succeeded = false;
        String message = null;

        // Do this before saving changes to cache!
        if (!maybeWriteBackupFromCache()) {
            message = "Unable to write backup file for " + file.getName() +
                ". Save anyway?";
            if (!editor.confirm(title, message))
                return false;
        }

        if (!saveToCache()) {
            message = "Unable to write temporary file for " + file.getName();
            MessageDialog.showMessageDialog(message, title);
            return false;
        }

        Ssh ssh = new Ssh();

        if (compression != null && compression.getType() == COMPRESSION_GZIP) {
            final File tempFile = Utilities.getTempFile();
            if (!compress(cache, tempFile)) {
                message = "Unable to compress temporary file for " +
                    file.getName();
                MessageDialog.showMessageDialog(message, "Save");
                return false;
            }
            succeeded = ssh.copy(tempFile, file);
        } else
            succeeded = ssh.copy(cache, file);

        if (!succeeded)
            message = ssh.getErrorText();

        if (succeeded) {
            saved();
        } else {
            // Tell user exactly what error occurred.
            if (message != null)
                MessageDialog.showMessageDialog(message, title);

            // Display summary message.
            message = "Unable to save " + file.canonicalPath();
            MessageDialog.showMessageDialog(message, title);
        }

        return succeeded;
    }

    public void saveAs(File destination)
    {
        File file = getFile();
        if (file != null)
            destination.setEncoding(file.getEncoding());
        if (destination.isLocal())
            saveAsLocal(destination);
        else if (destination instanceof FtpFile)
            saveAsFtp((FtpFile)destination);
        else
            MessageDialog.showMessageDialog("Invalid destination", "Save As");
    }

    private boolean saveAsLocal(File destination)
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        if (destination == null)
            return false;
        if (destination.isDirectory()) {
            final String prompt =
                destination.canonicalPath().concat(" is a directory");
            MessageDialog.showMessageDialog(prompt, "Save As");
            return false;
        }
        setBusy(true);
        Editor.currentEditor().setWaitCursor();
        boolean succeeded = false;
        String message = null;
        if (lineSeparator == null)
            lineSeparator = System.getProperty("line.separator");
        final File tempFile = Utilities.getTempFile(destination.getParent());
        if (tempFile == null) {
            message = "Unable to create temporary file for " +
                destination.canonicalPath();
        } else {
            tempFile.setEncoding(destination.getEncoding());
            if (writeFile(tempFile)) {
                Log.debug("buffer written to " + tempFile.canonicalPath());
                if (Utilities.makeBackup(destination, false)) {
                    destination.delete();
                    if (tempFile.renameTo(destination)) {
                        succeeded = true;
                    } else {
                        Log.error("unable to rename " +
                            tempFile.canonicalPath() + " to " +
                            destination.canonicalPath());
                        message = "Unable to rename temporary file";
                    }
                } else {
                    Log.error("backup failed");
                    message = "Unable to write backup file for " +
                        destination.canonicalPath();
                }
            } else {
                Log.error("writeFile failed");
                message = "Unable to create temporary file in " +
                    tempFile.getParent();
            }
        }
        setBusy(false);
        if (succeeded) {
            saved();
            changeFile(destination);
            setLastModified(getFile().lastModified());
            checkCVS();
            final String encoding = destination.getEncoding();
            if (encoding != null)
                saveProperties(); // Remember encoding for next time.
            if (isTaggable())
                Editor.getTagFileManager().addToQueue(
                    destination.getParentFile(),
                    mode);
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_REPAINT_BUFFER_LIST);
        } else {
            // Tell user exactly what error occurred.
            if (message != null)
                MessageDialog.showMessageDialog(message, "Save As");
            MessageDialog.showMessageDialog("Save failed", "Save As");
        }
        return succeeded;
    }

    private void saveAsFtp(final FtpFile destination)
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        FtpSession session = FtpSession.getSession(destination);
        if (session == null)
            return;
        Debug.assertTrue(session.isLocked());
        if (!lock()) {
            session.unlock();
            MessageDialog.showMessageDialog("Buffer is busy",
                getFile().netPath());
            return;
        }
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        if (!saveToCache()) {
            unlock();
            session.unlock();
            editor.setDefaultCursor();
            String message = "Unable to write temporary file for " +
                destination.netPath();
            MessageDialog.showMessageDialog(message, "Save As");
            return;
        }
        final FtpSaveProcess saveProcess =
            new FtpSaveProcess(this, cache, destination, session);
        saveProcess.setConfirmOverwrite(true);
        saveProcess.setTitle("Save As");
        final Runnable successRunnable = new Runnable() {
            public void run()
            {
                saved();
                changeFile(destination);
                setListing(saveProcess.getListing());
                Sidebar.setUpdateFlagInAllFrames(SIDEBAR_REPAINT_BUFFER_LIST);
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == Buffer.this)
                        ed.setDefaultCursor();
                }
                Sidebar.repaintBufferListInAllFrames();
            }
        };
        saveProcess.setSuccessRunnable(successRunnable);
        Debug.assertTrue(isLocked());
        saveProcess.start();
    }

    public void saveCopy(File destination)
    {
        if (destination.isLocal())
            saveCopyLocal(destination);
        else if (destination instanceof FtpFile)
            saveCopyFtp((FtpFile)destination);
        else
            MessageDialog.showMessageDialog("Invalid destination", "Save Copy");
    }

    private void saveCopyLocal(File destination)
    {
        boolean succeeded = false;
        String message = null;
        if (lineSeparator == null)
            lineSeparator = System.getProperty("line.separator");
        File tempFile = Utilities.getTempFile(destination.getParent());
        if (tempFile == null) {
            message = "Unable to create temporary file for " +
                destination.canonicalPath();
        } else {
            tempFile.setEncoding(destination.getEncoding());
            if (writeFile(tempFile)) {
                Log.debug("buffer written to " + tempFile.canonicalPath());
                if (Utilities.makeBackup(destination, false)) {
                    destination.delete();
                    if (tempFile.renameTo(destination)) {
                        succeeded = true;
                    } else {
                        Log.error("unable to rename " +
                            tempFile.canonicalPath() + " to " +
                            destination.canonicalPath());
                        message = "Unable to rename temporary file";
                    }
                } else {
                    Log.error("backup failed");
                    message = "Unable to write backup file for " +
                        destination.canonicalPath();
                }
            } else {
                Log.error("writeFile failed");
                message = "Unable to create temporary file in " +
                    tempFile.getParent();
            }
        }
        if (succeeded) {
            if (isTaggable())
                Editor.getTagFileManager().addToQueue(destination.getParentFile(), mode);
        } else {
            // Tell user exactly what error occurred.
            if (message != null)
                MessageDialog.showMessageDialog(message, "Save Copy");
            MessageDialog.showMessageDialog("Save failed", "Save Copy");
        }
    }

    private void saveCopyFtp(FtpFile destination)
    {
        Debug.assertTrue(SwingUtilities.isEventDispatchThread());
        FtpSession session = FtpSession.getSession(destination);
        if (session == null)
            return;
        Debug.assertTrue(session.isLocked());
        if (!lock()) {
            session.unlock();
            MessageDialog.showMessageDialog("Buffer is busy", getFile().netPath());
            return;
        }
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        if (!saveToCache()) {
            unlock();
            session.unlock();
            editor.setDefaultCursor();
            String message = "Unable to write temporary file for " + destination.netPath();
            MessageDialog.showMessageDialog(message, "Save Copy");
            return;
        }
        final Runnable successRunnable = new Runnable() {
            public void run()
            {
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == Buffer.this)
                        ed.setDefaultCursor();
                }
                Sidebar.repaintBufferListInAllFrames();
            }
        };
        final FtpSaveProcess saveProcess =
            new FtpSaveProcess(this, cache, destination, session);
        saveProcess.setConfirmOverwrite(true);
        saveProcess.setTitle("Save Copy");
        saveProcess.setSuccessRunnable(successRunnable);
        Debug.assertTrue(isLocked());
        saveProcess.start();
    }

    // Removes tabs and spaces only.
    public void removeTrailingWhitespace()
    {
        boolean bufferChanged = false;
        CompoundEdit compoundEdit = null;
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                final String text = line.getText();
                final int originalLength = text.length();
                int length = originalLength;
                for (int i = originalLength - 1; i >= 0; i--) {
                    char c = text.charAt(i);
                    if (c == ' ' || c == '\t')
                        --length;
                    else
                        break;
                }
                if (length != originalLength) {
                    if (!bufferChanged) {
                        // First change.
                        compoundEdit = new CompoundEdit();
                        bufferChanged = true;
                    }
                    compoundEdit.addEdit(new UndoLineEdit(this, line));
                    line.setText(text.substring(0, length));
                }
            }
            if (bufferChanged)
                modified();
        }
        finally {
            unlockWrite();
        }
        if (compoundEdit != null && undoManager != null) {
            compoundEdit.end();
            undoManager.addEdit(compoundEdit);
        }
        if (bufferChanged) {
            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getBuffer() == this) {
                    if (ed.getDotOffset() > ed.getDotLine().length()) {
                        ed.getDot().setOffset(ed.getDotLine().length());
                        if (getBooleanProperty(Property.RESTRICT_CARET)) {
                            ed.moveCaretToDotCol();
                            ed.updateDotLine();
                        }
                    }
                    if (ed.getMark() != null) {
                        if (ed.getMarkOffset() > ed.getMarkLine().length())
                            ed.getMark().setOffset(ed.getMarkLine().length());
                    }
                    ed.repaintDisplay();
                }
            }
        }
    }

    public synchronized void autosave()
    {
        if (autosaveEnabled && Autosave.isAutosaveEnabled())
            if (modCount != autosaveModCount)
                new Thread(autosaveRunnable, "autosave").start();
    }

    private final Runnable autosaveRunnable = new Runnable() {
        public void run()
        {
            try {
                lockRead();
            }
            catch (InterruptedException e) {
                Log.error(e);
                return;
            }
            try {
                autosaveInternal();
            }
            finally {
                unlockRead();
            }
        }
    };

    private void autosaveInternal()
    {
        if (autosaveFile == null) {
            final File autosaveDirectory = Autosave.getAutosaveDirectory();
            if (autosaveDirectory == null)
                return;
            autosaveFile = Utilities.getTempFile(autosaveDirectory);
            if (autosaveFile == null)
                return;
            // Update autosave catalog file.
            Autosave.put(getFile().netPath(), autosaveFile.getName());
            Autosave.flush();
        }
        autosaveFile.setEncoding(getFile().getEncoding());
        if (writeFile(autosaveFile))
            autosaveModCount = modCount;
        else
            Log.error("autosave writeFile failed");
    }

    public void deleteAutosaveFile()
    {
        if (autosaveFile != null)
            autosaveFile.delete();
    }

    public void setFirstLine(Line line)
    {
        if (!rwlock.isWriteLocked()) {
            Log.error("----- setFirstLine() called without write lock -----");
            Debug.dumpStack();
        }
        super.setFirstLine(line);
    }

    public void modified()
    {
        if (!rwlock.isWriteLocked()) {
            Log.error("----- modified() called without write lock -----");
            Debug.dumpStack();
        }
        if (modCount > saveModCount) {
            // Already modified.
            incrementModCount();
        } else {
            // First change.
            setModCount(saveModCount + 1);
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_MODIFIED_BUFFER_COUNT);
            Sidebar.repaintBufferListInAllFrames();
        }
        invalidate();
    }

    public void unmodified()
    {
        setModCount(0);
        saveModCount = 0;
        autosaveModCount = 0;
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_MODIFIED_BUFFER_COUNT);
        Sidebar.repaintBufferListInAllFrames();
    }

    public void saved()
    {
        saveModCount = modCount;
        autosaveModCount = modCount;
        if (isNewFile()) {
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                line.setOriginalText(null);
                line.setNew(false);
            }
            setNewFile(false);
        } else {
            for (Line line = getFirstLine(); line != null; line = line.next()) {
                if (line.isModified())
                    line.setSaved(true);
            }
            if (getBooleanProperty(Property.SHOW_CHANGE_MARKS))
                repaint();
        }
        backedUp = false; // Ignored for local buffers.
        Sidebar.setUpdateFlagInAllFrames(SIDEBAR_MODIFIED_BUFFER_COUNT);
        Sidebar.repaintBufferListInAllFrames();
        deleteAutosaveFile();
    }

    public void empty()
    {
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.debug(e);
            return;
        }
        try {
            _empty();
        }
        finally {
            unlockWrite();
        }
        setTags(null);
        // Invalidate any stored views that are referencing the old contents
        // of this buffer.
        if (lastView != null)
            lastView.invalidate();
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            View view = ed.getView(this);
            if (view != null)
                view.invalidate();
            if (ed.getBuffer() == this) {
                ed.setDot(null);
                ed.setMark(null);
                ed.setTopLine(null);
            }
        }
    }

    public void invalidate()
    {
        needsParsing = true;
        maxColsValid = false;
        setTags(null);
        setMark(null);
    }

    public final View getLastView()
    {
        if (lastView != null)
            return (View) lastView.clone();
        else
            return null;
    }

    public final void setLastView(View view)
    {
        lastView = (View) view.clone();
    }

    public void saveView(Editor editor)
    {
        final View view = saveViewInternal(editor);
        editor.setView(this, view);
        setLastView(view);
    }

    protected View saveViewInternal(Editor editor)
    {
        final Display display = editor.getDisplay();
        View view = editor.getView(this);
        if (view == null)
            view = new View();
        final Position dot = editor.getDot();
        view.dot = dot == null ? null : new Position(dot);
        final Position mark = editor.getMark();
        view.mark = mark == null ? null : new Position(mark);
        view.selection = editor.getSelection();
        view.setColumnSelection(editor.isColumnSelection());
        view.topLine = editor.getTopLine();
        if (view.topLine != null)
            view.topLineNumber = view.topLine.lineNumber();
        view.pixelsAboveTopLine = display.getPixelsAboveTopLine();
        view.shift = display.shift;
        view.caretCol = display.caretCol;
        view.timestamp = System.currentTimeMillis();
        if (view.dot == null) {
            view.lineNumber = 0;
            view.offs = 0;
        } else {
            view.lineNumber = view.dot.lineNumber();
            view.offs = view.dot.getOffset();
        }
        return view;
    }

    public boolean needsParsing()
    {
        return needsParsing;
    }

    public final void setNeedsParsing(boolean b)
    {
        needsParsing = b;
    }

    public boolean isModified()
    {
        return modCount != saveModCount;
    }

    public Line getLine(int lineNumber)
    {
        if (lineNumber < 0)
            return null;
        int n = 0;
        Line line = getFirstLine();
        while (line != null && n != lineNumber) {
            line = line.next();
            ++n;
        }
        return line;
    }

    /**
     * Returns position in buffer based on original line number. Never returns
     * null.
     *
     * @param lineNumber        the original line number to look for
     * @param offset            the offset within the line
     * @return                  the position.
     */
    public Position findOriginal(int lineNumber, int offset)
    {
        if (offset < 0)
            offset = 0;
        Line line = getFirstLine();
        while (line != null && line.originalLineNumber() != lineNumber)
            line = line.next();
        if (line != null)
            return new Position(line, Math.min(offset, line.length()));
        if (line == null) {
            // We didn't find the exact line we were looking for. Find the
            // line with the next highest original line number.
            line = getFirstLine();
            while (line != null && line.originalLineNumber() < lineNumber)
                line = line.next();
        }
        if (line != null)
            return new Position(line, 0);
        return getEnd();
    }

    // Convert position into absolute character offset from start of buffer.
    public int getAbsoluteOffset(Position pos)
    {
        Line targetLine = pos.getLine();
        int offset = 0;
        Line line = getFirstLine();
        while (line != null && line != targetLine) {
            offset += line.length() + 1;
            line = line.next();
        }
        if (line == null)
            return -1; // Line not in buffer.
        else
            return offset + pos.getOffset();
    }

    // Convert absolute character offset from start of buffer into position.
    public Position getPosition(int goal)
    {
        int offset = 0;
        Line line = getFirstLine();
        while (line != null) {
            // Line separator always counts as 1.
            offset += line.length() + 1;
            if (offset >= goal) {
                if (offset == goal)
                    line = line.next();
                break;
            }
            line = line.next();
        }
        if (line == null)
            return null; // We hit the end of the buffer without reaching our goal.
        if (offset == goal)
            return new Position(line, 0);
        offset -= line.length() + 1;
        return new Position(line, goal - offset);
    }

    private UndoManager undoManager;

    protected void initializeUndo()
    {
        undoManager = new UndoManager();
    }

    public void resetUndo()
    {
        if (supportsUndo)
            undoManager.discardAllEdits();
    }

    public void resetRedo()
    {
    }

    public final UndoManager getUndoManager()
    {
        return undoManager;
    }

    public final void addEdit(UndoableEdit edit)
    {
        if (undoManager != null)
            undoManager.addEdit(edit);
    }

    public final void addUndoBoundary()
    {
        if (undoManager != null)
            undoManager.addEdit(UndoBoundary.getInstance());
    }

    public final void appendUndoFold(Editor editor)
    {
        if (undoManager != null)
            undoManager.appendUndoFold(editor);
    }

    public boolean canUndo()
    {
        return (undoManager != null && undoManager.canUndo());
    }

    public void undo()
    {
        if (undoManager != null) {
            if (needsRenumbering)
                renumber();
            try {
                undoManager.undo();
            }
            catch (CannotUndoException e) {
                Editor.currentEditor().status("Nothing to undo");
            }
        }
    }

    public boolean canRedo()
    {
        return (undoManager != null && undoManager.canRedo());
    }

    public void redo()
    {
        if (undoManager != null) {
            if (needsRenumbering)
                renumber();
            try {
                undoManager.redo();
            }
            catch (CannotRedoException e) {
                Editor.currentEditor().status("Nothing to redo");
            }
        }
    }

    public CompoundEdit beginCompoundEdit()
    {
        if (supportsUndo) {
            CompoundEdit compoundEdit = new CompoundEdit();
            undoManager.addEdit(compoundEdit);
            return compoundEdit;
        } else
            return null;
    }

    public void endCompoundEdit(CompoundEdit compoundEdit)
    {
        if (compoundEdit != null)
            compoundEdit.end();
    }

    protected void setText(String text)
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
            if (text != null) {
                FastStringReader reader = new FastStringReader(text);
                String s;
                while ((s = reader.readLine()) != null)
                    appendLine(s);
            }
            if (getFirstLine() == null)
                appendLine("");
            renumber();
            invalidate();
            setLoaded(true);
        }
        finally {
            unlockWrite();
        }
    }

    // Inserts s at pos, moves pos past s.
    public void insertString(Position pos, String s)
    {
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            FastStringBuffer sb = new FastStringBuffer();
            final int limit = s.length();
            boolean skipLF = false;
            for (int i = 0; i < limit; i++) {
                char c = s.charAt(i);
                if (c == '\r') {
                    if (sb.length() > 0) {
                        insertChars(pos, sb.toString());
                        sb.setLength(0);
                    }
                    insertLineSeparator(pos);
                    skipLF = true;
                } else if (c == '\n') {
                    if (skipLF) {
                        skipLF = false;
                    } else {
                        if (sb.length() > 0) {
                            insertChars(pos, sb.toString());
                            sb.setLength(0);
                        }
                        insertLineSeparator(pos);
                    }
                } else {
                    skipLF = false;
                    sb.append(c);
                }
            }
            if (sb.length() > 0)
                insertChars(pos, sb.toString());
        }
        finally {
            unlockWrite();
        }
    }

    // Inserts s at pos, moves pos past s.
    // String must not contain a line separator!
    public void insertChars(Position pos, String s)
    {
        final int length = s.length();
        if (length > 0) {
            String text = pos.getLine().getText();
            FastStringBuffer sb = new FastStringBuffer(text.length() + length);
            sb.append(text.substring(0, pos.getOffset()));
            sb.append(s);
            sb.append(text.substring(pos.getOffset()));
            pos.getLine().setText(sb.toString());
            pos.skip(length);
            modified();
        }
    }

    public void insertLineSeparator(Position pos)
    {
        final Line line = pos.getLine();
        final int offset = pos.getOffset();
        if (offset == 0) {
            final Line newLine = new TextLine("");
            newLine.setNew(true);
            // Assume that the line flags should carry over to the new line.
            newLine.setFlags(line.flags());
            final Line prevLine = line.previous();
            newLine.setPrevious(prevLine);
            if (prevLine != null) {
                prevLine.setNext(newLine);
            } else {
                Debug.assertTrue(line == getFirstLine());
                setFirstLine(newLine);
            }
            newLine.setNext(line);
            line.setPrevious(newLine);
        } else if (offset == line.length()) {
            final Line newLine = new TextLine("");
            newLine.setNew(true);
            // Assume that the line flags should carry over to the new line.
            newLine.setFlags(line.flags());
            final Line prevLine = line;
            final Line nextLine = line.next();
            newLine.setPrevious(prevLine);
            if (prevLine != null) {
                prevLine.setNext(newLine);
            } else {
                Debug.assertTrue(line == getFirstLine());
                setFirstLine(newLine);
            }
            newLine.setNext(nextLine);
            if (nextLine != null)
                nextLine.setPrevious(newLine);
            pos.moveTo(newLine, 0);
        } else {
            final String head = line.substring(0, offset);
            final String tail = line.substring(offset);
            line.setText(head);
            final Line nextLine = line.next();
            final Line newLine = new TextLine(tail);
            newLine.setNew(true);
            // Assume that the line flags should carry over to the new line.
            newLine.setFlags(line.flags());
            line.setNext(newLine);
            newLine.setPrevious(line);
            newLine.setNext(nextLine);
            if (nextLine != null)
                nextLine.setPrevious(newLine);
            pos.moveTo(newLine, 0);
        }
        needsRenumbering = true;
        modified();
        repaint();
    }

    // Repaint all windows displaying this buffer.
    public final void repaint()
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getBuffer() == this)
                ed.repaintDisplay();
        }
    }

    public String getTitle()
    {
        if (title != null)
            return title;
        final File file = getFile();
        if (file != null)
            return file.netPath();
        else
            return "";
    }

    public final void setTitle(String s)
    {
        title = s;
    }

    public String getFileNameForDisplay()
    {
        final File file = getFile();
        if (file != null)
            return file.isRemote() ? file.netPath() : file.getAbsolutePath();
        return "";
    }

    // For the buffer list.
    public String toString()
    {
        if (title != null)
            return title;
        final File file = getFile();
        if (file == null) {
            // This case should be handled by toString() in the derived class.
            return null;
        }
        if (file instanceof HttpFile)
            return file.netPath();
        if (file.isRemote()) {
            // SSH, FTP.
            FastStringBuffer sb = new FastStringBuffer(file.getName());
            sb.append(" [");
            sb.append(file.netPath());
            sb.append(']');
            return sb.toString();
        }
        // Local file.
        if (file.isDirectory())
            return file.canonicalPath();
        return Editor.getBufferList().getUniqueName(this);
    }

    // For the buffer list.
    public Icon getIcon()
    {
        if (isModified())
            return Utilities.getIconFromFile("modified.png");
        else if (isReadOnly())
            return Utilities.getIconFromFile("read_only.png");
        else
            return Utilities.getIconFromFile("buffer.png");
    }

    public final int getCol(Position pos)
    {
        return getCol(pos.getLine(), pos.getOffset());
    }

    // Returns the absolute column for a given line and offset, based on the
    // tab width of the buffer.
    public final int getCol(Line line, int offset)
    {
        return getCol(line, offset, getTabWidth());
    }

    public final static int getCol(Line line, int offset, int tabWidth)
    {
        final int limit = Math.min(offset, line.length());
        int col = 0;
        for (int i = 0; i < limit; i++) {
            if (line.charAt(i) == '\t')
                col += tabWidth - col % tabWidth;
            else
                ++col;
        }
        return col;
    }

    // Return existing indentation of line (number of columns).
    public int getIndentation(Line line)
    {
        int indent = 0;
        final int tabWidth = getTabWidth();
        final int limit = line.length();
        for (int i = 0; i < limit; i++) {
            char c = line.charAt(i);
            if (c == '\t')
                indent += tabWidth - indent % tabWidth;
            else if (c == ' ')
                ++indent;
            else
                break;
        }
        return indent;
    }

    // No write locking, does not call modified().
    public void setIndentation(Line line, int indent)
    {
        // Skip over existing indentation.
        final int length = line.length();
        int i = 0;
        while (i < length && Character.isWhitespace(line.charAt(i)))
            ++i;
        if (i < length) {
            String remainder = line.substring(i);
            if (indent > 0) {
                // Put required indentation into stringbuffer.
                FastStringBuffer sb = getCorrectIndentationString(indent);
                sb.append(remainder);
                line.setText(sb.toString());
            } else
                line.setText(remainder);
        } else
            line.setText("");
    }

    public FastStringBuffer getCorrectIndentationString(int indent)
    {
        FastStringBuffer sb = new FastStringBuffer(256);
        if (getUseTabs()) {
            final int tabWidth = getTabWidth();
            int col = 0;
            while (col + tabWidth <= indent) {
                sb.append('\t');
                col += tabWidth;
            }
            while (col < indent) {
                sb.append(' ');
                ++col;
            }
        } else
            sb.append(Utilities.spaces(indent));
        return sb;
    }

    private boolean folded;

    public final void renumber()
    {
        folded = false;
        lineCount = 0;
        visibleLineCount = 0;
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            line.setLineNumber(lineCount++);
            if (line.isHidden())
                folded = true;
            else
                ++visibleLineCount;
        }
        needsRenumbering = false;
    }

    protected void renumberOriginal()
    {
        folded = false;
        lineCount = 0;
        visibleLineCount = 0;
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            line.setOriginalLineNumber(lineCount);
            line.setLineNumber(lineCount++);
            if (line.isHidden())
                folded = true;
            else
                ++visibleLineCount;
        }
        needsRenumbering = false;
    }

    protected void enforceOutputLimit(Property property)
    {
        Debug.assertTrue(property != null);
        final int outputLimit =
            Editor.preferences().getIntegerProperty(property);
        if (outputLimit == 0 || lineCount <= outputLimit)
            return;
        try {
            lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            Position begin = new Position(getFirstLine(), 0);
            Line line = getFirstLine();
            for (int i = lineCount - outputLimit; i > 0; i--) {
                if (line.next() == null)
                    break;
                line = line.next();
            }
            Position end = new Position(line, 0);
            Region r = new Region(this, begin, end);
            r.deleteLines();
            if (needsRenumbering)
                renumber();
            resetUndo();
        }
        finally {
            unlockWrite();
        }
    }

    public void saveProperties()
    {
        if (type != TYPE_NORMAL)
            return;
        if (isUntitled)
            return;
        final File file = getFile();
        if (file == null)
            return;
        final String netPath = file.netPath();
        // Name must not contain any characters that can't appear in an XML
        // attribute value. In particular, an HTTP query might contain '&'.
        if (netPath.indexOf('<') >= 0 || netPath.indexOf('&') >= 0)
            return;
        FileHistoryEntry entry = new FileHistoryEntry();
        entry.setName(netPath);
        entry.setEncoding(file.getEncoding());
        entry.setMode(mode.toString());
        entry.setWhen(System.currentTimeMillis());
        entry.setProperties(properties);
        FileHistory fileHistory = FileHistory.getFileHistory();
        fileHistory.store(entry);
        fileHistory.save();
    }

    public void windowClosing()
    {
    }

    public void dispose()
    {
        if (cache != null && cache.isFile()) {
            // Only delete the cache file if no other buffer is using it.
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer buf = it.nextBuffer();
                if (buf != this && buf.getCache() == cache)
                    return;
            }
            // Not in use. OK to delete.
            cache.delete();
        }
    }

    protected void finalize() throws Throwable
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
        }
        finally {
            unlockWrite();
        }
        super.finalize();
    }

    public final boolean canBeRestored()
    {
        final File file = getFile();
        if (file != null && file.isRemote())
            return false;
        if (this instanceof RemoteBuffer)
            return false;
        switch (type) {
            case TYPE_NORMAL:
            case TYPE_ARCHIVE:
            case TYPE_DIRECTORY:
                return true;
            default:
                return false;
        }
    }

    public final boolean isUntitled()
    {
        return isUntitled;
    }

    private boolean isTransient;

    public boolean isTransient()
    {
        return isTransient;
    }

    public final void setTransient(boolean b)
    {
        unsplitOnClose = isTransient = b;
    }

    private boolean unsplitOnClose;

    public boolean unsplitOnClose()
    {
        return unsplitOnClose;
    }

    public final void setUnsplitOnClose(boolean b)
    {
        unsplitOnClose = b;
    }

    // Cache for getText().
    private SoftReference srText;

    // Never returns null.
    public synchronized String getText()
    {
        if (srText != null) {
            final String text = (String) srText.get();
            if (text != null)
                return text;
        }
        FastStringBuffer sb = new FastStringBuffer(16384);
        Line line = getFirstLine();
        if (line != null) {
            String text = line.getText();
            if (text != null)
                sb.append(text);
            line = line.next();
            while (line != null) {
                text = line.getText();
                if (text != null) {
                    sb.append('\n');
                    sb.append(text);
                }
                line = line.next();
            }
        }
        final String text = sb.toString();
        srText = new SoftReference(text);
        return text;
    }

    public boolean isEmpty()
    {
        Line first = getFirstLine();
        if (first == null)
            return true;
        if (first.next() != null)
            return false;
        return first.length() == 0;
    }

    public int getDisplayHeight()
    {
        return visibleLineCount * Display.getCharHeight();
    }

    // Returns cumulative height to top of target line.
    public int getY(Line line)
    {
        Debug.assertTrue(!line.isHidden());
        if (folded) {
            final int charHeight = Display.getCharHeight();
            int y = 0;
            for (Line l = getFirstLine(); l != null; l = l.nextVisible()) {
                if (l == line)
                    break;
                else
                    y += charHeight;
            }
            return y;
        } else
            return line.lineNumber() * Display.getCharHeight();
    }

    public int getDisplayWidth()
    {
        return Display.getGutterWidth(this) +
            (getMaximumColumns()+1) * Display.getCharWidth();
    }

    private int maxCols = 0;
    private boolean maxColsValid = false;

    public int getMaximumColumns()
    {
        if (maxCols == 0) {
            maxCols = calculateMaximumColumns();
            maxColsValid = true;
        }
        return maxCols;
    }

    private int calculateMaximumColumns()
    {
        if (getModeId() == BINARY_MODE)
            return getFirstLine().length();
        final int tabWidth = getTabWidth();
        int max = 0;
        for (Line line = getFirstLine(); line != null; line = line.next()) {
            int cols;
            if ((cols = getCol(line, line.length(), tabWidth)) > max)
                max = cols;
        }
        return max;
    }

    // Returns true if there was a change.
    public boolean validateMaximumColumns()
    {
        if (maxColsValid)
            return false;
        final int oldMaxCols = maxCols;
        maxCols = calculateMaximumColumns();
        maxColsValid = true;
        return maxCols != oldMaxCols;
    }

    public void setProperty(Property property, Object value)
    {
        properties.setProperty(property, value);
    }

    public void setProperty(Property property, boolean value)
    {
        properties.setProperty(property, value);
    }

    public void setProperty(Property property, int value)
    {
        properties.setProperty(property, value);
    }

    boolean setPropertyFromString(Property property, String value)
    {
        return properties.setPropertyFromString(property, value);
    }

    boolean removeProperty(Property property)
    {
        return properties.removeProperty(property);
    }

    public String getStringProperty(Property property)
    {
        Object value = properties.getProperty(property);
        if (value instanceof String)
            return (String) value;
        if (mode != null)
            return mode.getStringProperty(property);
        return (String) property.getDefaultValue();
    }

    public int getIntegerProperty(Property property)
    {
        if (!property.isIntegerProperty())
            Debug.bug();
        Object value = properties.getProperty(property);
        if (value instanceof Integer)
            return ((Integer)value).intValue();
        if (mode != null)
            return mode.getIntegerProperty(property);
        return ((Integer)property.getDefaultValue()).intValue();
    }

    public boolean getBooleanProperty(Property property)
    {
        if (!property.isBooleanProperty())
            Debug.bug();
        Object value = properties.getProperty(property);
        if (value == Boolean.TRUE)
            return true;
        if (value == Boolean.FALSE)
            return false;
        if (mode != null)
            return mode.getBooleanProperty(property);
        return ((Boolean)property.getDefaultValue()).booleanValue();
    }

    public final int getTabWidth()
    {
        return getIntegerProperty(Property.TAB_WIDTH);
    }

    public void setTabWidth(int tabWidth)
    {
        properties.setProperty(Property.TAB_WIDTH, tabWidth);
    }

    public final boolean getUseTabs()
    {
        return getBooleanProperty(Property.USE_TABS);
    }

    public final int getIndentSize()
    {
        return getIntegerProperty(Property.INDENT_SIZE);
    }

    public final void setIndentSize(int indentSize)
    {
        properties.setProperty(Property.INDENT_SIZE, indentSize);
    }

    private static final Cursor textCursor =
        Cursor.getPredefinedCursor(Cursor.TEXT_CURSOR);

    public Cursor getDefaultCursor()
    {
        return textCursor;
    }

    public Cursor getDefaultCursor(Position pos)
    {
        if (pos == null || pos.getLine() instanceof ImageLine)
            return Cursor.getDefaultCursor();
        else
            return textCursor;
    }

    public String getStatusText(Editor editor)
    {
        Debug.assertTrue(editor.getBuffer() == this);
        Position dot = editor.getDotCopy();
        if (dot == null)
            return null;
        final FastStringBuffer sb = new FastStringBuffer();
        if (cvsEntry != null) {
            sb.append("CVS ");
            final String revision = cvsEntry.getRevision();
            if (revision.equals("0")) {
                sb.append('A');
            } else {
                sb.append(revision);
                final long checkout = cvsEntry.getCheckoutTime();
                if (lastModified != checkout) {
                    if (Math.abs(lastModified - checkout) >= 1000)
                        sb.append(" M");
                }
            }
            sb.append("   ");
        }
        if (Editor.preferences().getBooleanProperty(Property.STATUS_BAR_DISPLAY_LINE_SEPARATOR)) {
            if (lineSeparator != null) {
                if (lineSeparator.equals("\n"))
                    sb.append("LF   ");
                else if (lineSeparator.equals("\r\n"))
                    sb.append("CR+LF   ");
                else if (lineSeparator.equals("\r"))
                    sb.append("CR   ");
            }
        }
        sb.append("Line ");
        sb.append(String.valueOf(dot.lineNumber()+1));
        if (Editor.preferences().getBooleanProperty(Property.STATUS_BAR_DISPLAY_LINE_COUNT)) {
            sb.append(" of ");
            sb.append(String.valueOf(getLineCount()));
        }
        sb.append("  Col ");
        final Display display = editor.getDisplay();
        sb.append(String.valueOf(display.getAbsoluteCaretCol()+1));
        if (getBooleanProperty(Property.WRAP))
            sb.append("   Wrap");
        if (Editor.isRecordingMacro())
            sb.append("   [Recording macro...]");
        return sb.toString();
    }

    public Expansion getExpansion(Position dot)
    {
        return new Expansion(dot, mode);
    }

    public void restoreView(Editor editor)
    {
        final Display display = editor.getDisplay();
        final View view = editor.getView(this);
        Debug.assertTrue(view != null);
        if (view != null) {
            if (view.getDot() == null) {
                Line line = getLine(view.getDotLineNumber());
                if (line != null) {
                    int offset = view.getDotOffset();
                    if (offset < 0)
                        offset = 0;
                    else if (offset > line.length())
                        offset = line.length();
                    view.setDot(new Position(line, offset));
                } else {
                    line = getFirstLine();
                    if (line != null)
                        view.setDot(new Position(line, 0));
                }
            }
            editor.setDot(view.getDot() == null ? null : new Position(view.getDot()));
            editor.setMark(view.getMark() == null ? null : new Position(view.getMark()));
            editor.setSelection(view.getSelection());
            editor.setColumnSelection(view.isColumnSelection());
            if (view.getTopLine() == null){
                view.topLine = getFirstLine();
                view.pixelsAboveTopLine = 0;
            }
            display.setTopLine(view.getTopLine());
            display.setPixelsAboveTopLine(view.pixelsAboveTopLine);
            display.setShift(view.getShift());
            display.setCaretCol(view.getCaretCol());
        }
    }

    private CVSEntry cvsEntry;

    public final CVSEntry getCVSEntry()
    {
        return cvsEntry;
    }

    public final void checkCVS()
    {
        cvsEntry = CVSEntry.parseEntryForFile(getFile());
    }

    public final boolean isKeyword(String s)
    {
        return mode.isKeyword(s);
    }
}
