/*
 * ArchiveMode.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import gnu.regexp.RE;
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.awt.AWTEvent;
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import java.io.IOException;
import java.io.OutputStream;
import java.text.SimpleDateFormat;
import java.util.Date;
import java.util.zip.ZipEntry;
import java.util.zip.ZipInputStream;

public final class ArchiveMode extends AbstractMode implements Constants, Mode
{
    private static final ArchiveMode mode = new ArchiveMode();
    private static final RE moveToFilenameRegExp =
        new UncheckedRE(":[0-5][0-9] ");

    private ArchiveMode()
    {
        super(ARCHIVE_MODE, ARCHIVE_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
    }

    public static final ArchiveMode getMode()
    {
        return mode;
    }

    public final Formatter getFormatter(Buffer buffer)
    {
        return new PlainTextFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_ENTER, 0, "archiveOpenFile");
        km.mapKey(KeyEvent.VK_G, CTRL_MASK | SHIFT_MASK, "archiveOpenFile");
        km.mapKey(VK_DOUBLE_MOUSE_1, 0, "archiveOpenFile");
        km.mapKey(VK_MOUSE_2, 0, "archiveOpenFile");
    }

    private static String getName(String s)
    {
        REMatch match = moveToFilenameRegExp.getMatch(s);
        return match == null ? null : s.substring(match.getEndIndex());
    }

    public static void openFileAtDot(Editor editor)
    {
        Buffer buffer = editor.getBuffer();
        String name = getName(editor.getDotLine().getText());
        if (name == null)
            return;
        String source = null;
        if (buffer.getFile() != null) {
            source = "[from " + buffer.getFile().netPath() + "]";
        } else {
            Compression compression = buffer.getCompression();
            if (compression != null && compression.getType() == COMPRESSION_ZIP)
                source = "[from " + compression.getEntryName() + " " + compression.getSource() + "]";
        }
        String title = name + " " + source;
        for (BufferIterator it = new BufferIterator(); it.hasNext();) {
            Buffer maybe = it.nextBuffer();
            if (title.equals(maybe.getTitle())) {
                editor.makeNext(maybe);
                editor.activate(maybe);
                return;
            }
        }
        File toBeLoaded = null;
        if (buffer.getCache() != null)
            toBeLoaded = buffer.getCache();
        else
            toBeLoaded = buffer.getFile();
        ZipInputStream in = null;
        try {
            in = new ZipInputStream(toBeLoaded.getInputStream());
            ZipEntry zipEntry;
            while((zipEntry = in.getNextEntry()) != null) {
                if (zipEntry.getName().equals(name)) {
                    if (zipEntry.isDirectory()) {
                        editor.status(name + " is a directory");
                    } else {
                        Buffer buf = null;
                        File cache = cacheEntry(in);
                        // First see if it's an image...
                        if (Editor.getModeList().modeAccepts(IMAGE_MODE, name))
                            buf = ImageBuffer.createImageBuffer(null, cache, null);
                        if (buf != null) {
                            buf.setCompression(new Compression(COMPRESSION_ZIP,
                                                               zipEntry,
                                                               source));
                            buf.setTitle(title);
                        } else {
                            buf = new Buffer();
                            buf.type = Buffer.TYPE_NORMAL; // Default (may be changed later).
                            buf.initializeUndo();
                            buf.setCache(cache);
                            buf.setCompression(new Compression(COMPRESSION_ZIP,
                                                               zipEntry,
                                                               source));
                            buf.initialize(); // May change buffer type.
                            buf.setTitle(title);
                            buf.readOnly = true;
                        }
                        editor.makeNext(buf);
                        editor.activate(buf);
                    }
                    break;
                }
            }
        }
        catch (Exception e) {
            Log.error(e);
        }
        try {
            if (in != null)
                in.close();
        }
        catch (Exception e) {
            Log.error(e);
        }
    }

    private static File cacheEntry(ZipInputStream in)
    {
        File cache = Utilities.getTempFile();
        if (cache != null) {
            OutputStream out = null;
            try {
                out = cache.getOutputStream();
                byte[] bytes = new byte[16384];
                int bytesRead;
                while ((bytesRead = in.read(bytes, 0, bytes.length)) > 0)
                    out.write(bytes, 0, bytesRead);
            }
            catch (IOException e) {
                Log.error(e);
            }
            if (out != null) {
                try {
                    out.close();
                }
                catch (IOException e) {
                    Log.error(e);
                }
            }
        }
        return cache;
    }

    public void loadFile(Buffer buffer, File file)
    {
        if (!buffer.isLoaded()) {
            ZipInputStream in = null;
            try {
                in = new ZipInputStream(file.getInputStream());
                ZipEntry ze;
                while((ze = in.getNextEntry()) != null) {
                    in.closeEntry();
                    appendLine(buffer, ze);
                }
                buffer.renumber();

                // Is this right if we're loading from a local cache?
                buffer.setLastModified(file.lastModified());
                buffer.setLoaded(true);
            }
            catch (Exception e) {
                Log.error(e);
            }
            try {
                if (in != null)
                    in.close();
            }
            catch (Exception e) {
                Log.error(e);
            }
        }
    }

    private static final SimpleDateFormat zipEntryDateFormatter =
        new SimpleDateFormat ("MMM dd yyyy HH:mm");

    private static void appendLine(Buffer buffer, ZipEntry ze)
    {
        FastStringBuffer sb = new FastStringBuffer();
        String sizeString = String.valueOf(ze.getSize());
        for (int i = 9 - sizeString.length(); i >= 0; i--)
            sb.append(' ');
        sb.append(sizeString);
        sb.append(' ');
        sb.append(zipEntryDateFormatter.format(new Date(ze.getTime())));
        sb.append(' ');
        sb.append(ze.getName());
        buffer.appendLine(sb.toString());
    }

    public static void archiveOpenFile()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getModeId() == ARCHIVE_MODE) {
            // If this method is invoked via a mouse event mapping, move dot to
            // location of mouse click first.
            AWTEvent e = editor.getDispatcher().getLastEvent();
            if (e instanceof MouseEvent)
                editor.mouseMoveDotToPoint((MouseEvent)e);
            openFileAtDot(editor);
        }
    }
}
