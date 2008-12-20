/*
 * BinaryMode.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

public final class BinaryMode extends AbstractMode implements Constants, Mode
{
    private static final BinaryMode mode = new BinaryMode();

    private BinaryMode()
    {
        super(BINARY_MODE, BINARY_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
    }

    public static final BinaryMode getMode()
    {
        return mode;
    }

    protected final void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_B, CTRL_MASK | ALT_MASK, "defaultMode");
    }

    // For the status bar.
    public String getContextString(Editor editor, boolean verbose /*ignored*/)
    {
        if (editor.getMode() instanceof BinaryMode) {
            final Line dotLine = editor.getDotLine();
            int col = editor.getDisplay().getCaretCol();
            int offset = -1;
            int limit = editor.getDotLine().length() - 58;
            if (col >= 10 && col < 58)
                offset = (col - 10) / 3;
            else if (col >= 58 && col < dotLine.length())
                offset = col - 58;
            if (offset >= 0 && offset < limit) {
                FastStringBuffer sb = new FastStringBuffer("0x");
                sb.append(dotLine.substring(0, 7));
                sb.append(Integer.toHexString(offset));
                int index = 10 + offset * 3;
                String hex = dotLine.substring(index, index + 2);
                sb.append("  0x");
                sb.append(hex);
                char c = (char) Integer.parseInt(hex, 16);
                if (c >= ' ' && c < 0x7f) {
                    sb.append("  '");
                    if (c == '\'')
                        sb.append('\\');
                    sb.append(c);
                    sb.append('\'');
                }
                return sb.toString();
            }
        }
        return null;
    }

    public static void binaryMode()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer.isModified()) {
            String prompt = "Buffer will be reloaded in binary mode; discard changes?";
            if (!editor.confirm(buffer.getFile().getName(), prompt))
                return;
        }
        editor.setWaitCursor();
        buffer.changeMode(mode);
        editor.setDefaultCursor();
    }
}
