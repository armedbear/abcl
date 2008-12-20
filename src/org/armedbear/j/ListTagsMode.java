/*
 * ListTagsMode.java
 *
 * Copyright (C) 2002-2005 Peter Graves
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
import java.awt.event.KeyEvent;
import java.awt.event.MouseEvent;
import javax.swing.JPopupMenu;

public final class ListTagsMode extends AbstractMode implements Constants, Mode
{
    private static final ListTagsMode mode = new ListTagsMode();

    private ListTagsMode()
    {
        super(LIST_TAGS_MODE, LIST_TAGS_MODE_NAME);
        setProperty(Property.VERTICAL_RULE, 0);
        setProperty(Property.SHOW_LINE_NUMBERS, false);
        setProperty(Property.SHOW_CHANGE_MARKS, false);
        setProperty(Property.HIGHLIGHT_MATCHING_BRACKET, false);
        setProperty(Property.HIGHLIGHT_BRACKETS, false);
    }

    public static final ListTagsMode getMode()
    {
        return mode;
    }

    public JPopupMenu getContextMenu(Editor editor)
    {
        return null;
    }

    public Formatter getFormatter(Buffer buffer)
    {
        return new ListTagsFormatter(buffer);
    }

    protected void setKeyMapDefaults(KeyMap km)
    {
        km.mapKey(KeyEvent.VK_UP, 0, "tagUp");
        km.mapKey(KeyEvent.VK_KP_UP, 0, "tagUp");
        km.mapKey(KeyEvent.VK_DOWN, 0, "tagDown");
        km.mapKey(KeyEvent.VK_KP_DOWN, 0, "tagDown");
        km.mapKey(KeyEvent.VK_ENTER, 0, "jumpToTag");
        km.mapKey(KeyEvent.VK_ENTER, CTRL_MASK, "jumpToTagAndKillList");
        km.mapKey(KeyEvent.VK_G, CTRL_MASK | SHIFT_MASK, "jumpToTag");
        km.mapKey(VK_DOUBLE_MOUSE_1, 0, "mouseJumpToTag");
        km.mapKey(VK_MOUSE_2, 0, "mouseJumpToTag");
        km.mapKey('q', "tempBufferQuit");
    }

    public static void jumpToTag()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListTagsBuffer)
            ((ListTagsBuffer)buffer).jumpToTag(editor, false);
    }

    public static void jumpToTagAndKillList()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListTagsBuffer)
            ((ListTagsBuffer)buffer).jumpToTag(editor, true);
    }

    public static void mouseJumpToTag()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListTagsBuffer) {
            AWTEvent e = editor.getDispatcher().getLastEvent();
            if (e instanceof MouseEvent) {
                editor.mouseMoveDotToPoint((MouseEvent)e);
                ((ListTagsBuffer)buffer).jumpToTag(editor, false);
            }
        }
    }

    public static void tagDown()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListTagsBuffer) {
            for (Line line = editor.getDotLine().next(); line != null; line = line.next()) {
                if (line instanceof TagLine) {
                    editor.moveDotTo(line, 0);
                    break;
                }
            }
        }
    }

    public static void tagUp()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        if (buffer instanceof ListTagsBuffer) {
            for (Line line = editor.getDotLine().previous(); line != null; line = line.previous()) {
                if (line instanceof TagLine) {
                    editor.moveDotTo(line, 0);
                    break;
                }
            }
        }
    }
}
