/*
 * ListRegistersMode.java
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

public class ListRegistersMode extends AbstractMode implements Constants, Mode
{
    private static final ListRegistersMode mode = new ListRegistersMode();

    private ListRegistersMode()
    {
        super(LIST_REGISTERS_MODE, LIST_REGISTERS_MODE_NAME);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.SHOW_CHANGE_MARKS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
    }

    public static final ListRegistersMode getMode()
    {
        return mode;
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new ListRegistersFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_ENTER, 0, "ListRegistersMode._editRegister");
        km.mapKey(KeyEvent.VK_ENTER, CTRL_MASK,
            "ListRegistersMode._insertRegister");
        km.mapKey(KeyEvent.VK_G, CTRL_MASK | SHIFT_MASK,
            "ListRegistersMode._editRegister");
        km.mapKey(KeyEvent.VK_DELETE, 0, "ListRegistersMode._clearRegister");
        km.mapKey(KeyEvent.VK_UP, 0, "ListRegistersMode.registerUp");
        km.mapKey(KeyEvent.VK_KP_UP, 0, "ListRegistersMode.registerUp");
        km.mapKey(KeyEvent.VK_DOWN, 0, "ListRegistersMode.registerDown");
        km.mapKey(KeyEvent.VK_KP_DOWN, 0, "ListRegistersMode.registerDown");
    }

    public static final void _editRegister()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getDot() == null)
            return;
        final Buffer buffer = editor.getBuffer();
        if (!(buffer instanceof ListRegistersBuffer))
            return;
        Line line = editor.getDotLine();
        while (!(line instanceof ListRegistersLine)) {
            line = line.previous();
            if (line == null)
                return;
        }
        Debug.assertTrue(line instanceof ListRegistersLine);
        String name = ((ListRegistersLine)line).getRegisterName();
        if (name != null)
            Registers.editRegister(name);
    }

    public static final void _insertRegister()
    {
        final Editor editor = Editor.currentEditor();
        final Editor other = editor.getOtherEditor();
        if (other == null)
            return;
        if (editor.getDot() == null)
            return;
        final Buffer buffer = editor.getBuffer();
        if (!(buffer instanceof ListRegistersBuffer))
            return;
        Line line = editor.getDotLine();
        while (!(line instanceof ListRegistersLine)) {
            line = line.previous();
            if (line == null)
                return;
        }
        Debug.assertTrue(line instanceof ListRegistersLine);
        String name = ((ListRegistersLine)line).getRegisterName();
        if (name != null) {
            String text = Registers.getText(name);
            if (text != null)
                other.paste(text);
        }
    }

    public static final void _clearRegister()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getDot() == null)
            return;
        final Buffer buffer = editor.getBuffer();
        if (!(buffer instanceof ListRegistersBuffer))
            return;
        Line line = editor.getDotLine();
        while (!(line instanceof ListRegistersLine)) {
            line = line.previous();
            if (line == null)
                return;
        }
        Debug.assertTrue(line instanceof ListRegistersLine);
        String name = ((ListRegistersLine)line).getRegisterName();
        if (name != null)
            Registers.clearRegister(name);
    }

    public static void registerDown()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListRegistersBuffer) {
            for (Line line = editor.getDotLine().next(); line != null; line = line.next()) {
                if (line instanceof ListRegistersLine &&
                    line.getText().startsWith("Register ")) {
                    editor.moveDotTo(line, 0);
                    break;
                }
            }
        }
    }

    public static void registerUp()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListRegistersBuffer) {
            for (Line line = editor.getDotLine().previous(); line != null; line = line.previous()) {
                if (line instanceof ListRegistersLine &&
                    line.getText().startsWith("Register ")) {
                    editor.moveDotTo(line, 0);
                    break;
                }
            }
        }
    }
}
