/*
 * WordMode.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.awt.event.KeyEvent;
import java.io.IOException;
import java.io.InputStream;

public final class WordMode extends AbstractMode implements Constants, Mode
{
    private static final WordMode mode = new WordMode();

    private WordMode()
    {
        super(WORD_MODE, WORD_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
    }

    public static final WordMode getMode()
    {
        return mode;
    }

    public final Formatter getFormatter(Buffer buffer)
    {
        return new PlainTextFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_B, CTRL_MASK | ALT_MASK, "binaryMode");
    }

    public void loadFile(Buffer buffer, File toBeLoaded)
    {
        try {
            load(buffer, toBeLoaded.getInputStream());
        }
        catch (IOException e) {
            Log.debug(e);
        }
    }

    private static void load(Buffer buffer, InputStream inputStream)
    {
        byte[] bytes = new byte[4096];
        int totalBytes = 0;
        try {
            int bytesRead = inputStream.read(bytes);
            if (bytesRead < 0x400)
                return;
            int magic = getWord(bytes, 0x200);
            Log.debug("magic = 0x" + Integer.toHexString(magic));
            if (magic != 0xa5ec)
                return;
            int version = getWord(bytes, 0x202);
            Log.debug("version = " + version);
            if (version < 101)
                return;
            int status = getWord(bytes, 0x20a);
            Log.debug("status = 0x" + Integer.toHexString(status));
            if ((status & 4) != 0)
                Log.debug("document is fast saved");
            long beginText = getLong(bytes, 0x218) + 0x200;
            Log.debug("beginText = 0x" + Long.toHexString(beginText));
            if (beginText > bytesRead)
                return; // BUG! We could handle this case.
            long endText = getLong(bytes, 0x21c) + 0x200;
            long textLength = endText - beginText;
            Log.debug("textLength = " + textLength);
            final String encoding = "Cp1252";
            ByteBuffer bb = new ByteBuffer(256);
            boolean done = false;
            Debug.assertTrue(beginText < Integer.MAX_VALUE);
            int start = (int) beginText;
            while (bytesRead > 0) {
                for (int i = start; i < bytesRead; i++) {
                    byte b = bytes[i];
                    switch (b) {
                        case 13:
                            if (bb.length() > 0)
                                wrapAndAppend(buffer, new String(bb.getBytes(), 0, bb.length(), encoding));
                            buffer.appendLine("");
                            bb.setLength(0);
                            break;
                        case 0:
                            if (bb.length() > 0)
                                wrapAndAppend(buffer, new String(bb.getBytes(), 0, bb.length(), encoding));
                            bb.setLength(0);
                            done = true;
                            break;
                        default:
                            // Normal char.
                            bb.append(b);
                            break;
                    }
                    if (done)
                        break;
                }
                if (done)
                    break;
                bytesRead = inputStream.read(bytes);
                start = 0;
                if (bytesRead > 0)
                    buffer.loadProgress(totalBytes = totalBytes + bytesRead);
            }
            buffer.unmodified();
            buffer.setLoaded(true);
            buffer.setForceReadOnly(true);
        }
        catch (Exception e) {
            Log.error(e);
        }
        buffer.loadFinished(buffer.isLoaded());
    }

    private static final void wrapAndAppend(Buffer buffer, String s)
    {
        buffer.append(Utilities.wrap(s, 65, 8));
    }

    // 4 bytes
    private static long getLong(byte[] bytes, int offset)
    {
        long byte0 = bytes[offset] & 0xff;
        long byte1 = bytes[offset + 1] & 0xff;
        long byte2 = bytes[offset + 2] & 0xff;
        long byte3 = bytes[offset + 3] & 0xff;
        return (byte3 << 24) + (byte2 << 16) + (byte1 << 8) + byte0;
    }

    // 2 bytes
    private static int getWord(byte[] bytes, int offset)
    {
        int hi = bytes[offset + 1] & 0xff;
        int lo = bytes[offset] & 0xff;
        return (hi << 8) + lo;
    }
}
