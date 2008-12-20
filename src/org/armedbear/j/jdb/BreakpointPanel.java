/*
 * BreakpointPanel.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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

package org.armedbear.j.jdb;

import java.awt.Component;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.Vector;
import javax.swing.JList;
import javax.swing.JScrollPane;
import org.armedbear.j.Buffer;
import org.armedbear.j.Editor;
import org.armedbear.j.File;

public final class BreakpointPanel implements BreakpointListener, KeyListener
{
    private final Jdb jdb;
    private final JdbControlDialog dialog;
    private final JList list;
    private final JScrollPane scrollPane;

    public BreakpointPanel(Jdb jdb, JdbControlDialog dialog)
    {
        this.jdb = jdb;
        this.dialog = dialog;
        Vector v = new Vector(jdb.getBreakpoints());
        list = new JList(v);
        scrollPane = new JScrollPane(list);
        jdb.addBreakpointListener(this);
        list.addKeyListener(this);
    }

    public Component getComponent()
    {
        return scrollPane;
    }

    public void breakpointChanged()
    {
        list.setListData(new Vector(jdb.getBreakpoints()));
        list.setSelectedIndex(-1);
    }

    public void keyPressed(KeyEvent e)
    {
        final int keyCode = e.getKeyCode();
        // Mask off the bits we don't care about (Java 1.4).
        final int modifiers = e.getModifiers() & 0x0f;
        if (modifiers != 0)
            return;
        if (keyCode == KeyEvent.VK_DELETE) {
            int index = list.getSelectedIndex();
            if (index >= 0) {
                Object obj = jdb.getBreakpoints().get(index);
                if (obj instanceof ResolvableBreakpoint) {
                    ResolvableBreakpoint bp = (ResolvableBreakpoint) obj;
                    jdb.log("clear " + bp.getLocationString());
                    jdb.deleteBreakpoint(bp);
                    File file = bp.getFile();
                    if (file != null) {
                        Buffer buffer = Editor.getBufferList().findBuffer(file);
                        if (buffer != null)
                            buffer.repaint();
                    }
                    jdb.saveSession();
                    jdb.fireBreakpointChanged();
                    if (index >= list.getModel().getSize())
                        --index;
                    list.setSelectedIndex(index);
                }
            }
        }
    }

    public void keyTyped(KeyEvent e) {}

    public void keyReleased(KeyEvent e) {}
}
