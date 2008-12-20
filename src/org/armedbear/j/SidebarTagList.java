/*
 * SidebarTagList.java
 *
 * Copyright (C) 2000-2002 Peter Graves
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

import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import java.util.List;
import javax.swing.SwingUtilities;
import javax.swing.ToolTipManager;

public class SidebarTagList extends SidebarList implements Constants,
    NavigationComponent, KeyListener, MouseListener, MouseMotionListener
{
    private Editor editor;
    private Buffer buffer;
    private List tags;

    public SidebarTagList(Sidebar sidebar, Editor editor)
    {
        super(sidebar);
        this.editor = editor;
        addKeyListener(this);
        addMouseListener(this);
        addMouseMotionListener(this);
    }

    public final Editor getEditor()
    {
        return editor;
    }

    public synchronized final void setEditor(Editor editor)
    {
        this.editor = editor;
    }

    public final Buffer getBuffer()
    {
        return buffer;
    }

    public final void setBuffer(Buffer buffer)
    {
        this.buffer = buffer;
    }

    public final String getLabelText()
    {
        File file = editor.getBuffer().getFile();
        return file != null ? file.getName() : null;
    }

    public synchronized void refresh()
    {
        if (!SwingUtilities.isEventDispatchThread())
            Debug.bug("SidebarTagList.refresh() called from background thread!");
        final Buffer buf = editor.getBuffer();
        if (buf == null)
            return;
        if (tags != null && tags == buf.getTags())
            return;
        tags = buf.getTags();
        if (tags != null) {
            setListData(tags.toArray());
            setBuffer(buf);
            updatePosition();
            return;
        }
        // Need to run tagger.
        final Tagger tagger = buf.getMode().getTagger(buf);
        if (tagger == null)
            return;
        Runnable runTaggerRunnable = new Runnable() {
            public void run()
            {
                boolean locked = false;
                try {
                    buf.lockRead();
                    locked = true;
                }
                catch (InterruptedException e) {
                    Log.error(e);
                }
                if (locked) {
                    try {
                        tagger.run();
                        tags = buf.getTags();
                    }
                    finally {
                        buf.unlockRead();
                    }
                }
                if (tags != null) {
                    final Object[] listData = tags.toArray();
                    Runnable replaceListDataRunnable = new Runnable() {
                        public void run()
                        {
                            // Make sure user didn't change buffers while we
                            // were preparing the tag list.
                            replaceListData(buf, listData);
                        }
                    };
                    SwingUtilities.invokeLater(replaceListDataRunnable);
                }
            }
        };
        Thread thread =
            new Thread(runTaggerRunnable, "SidebarTagList.refresh()");
        thread.setDaemon(true);
        thread.start();
    }

    private synchronized void replaceListData(Buffer buf, Object[] listData)
    {
        if (editor.getBuffer() == buf) {
            setListData(listData);
            setBuffer(buf);
            updatePosition();
        }
    }

    // Set the selection to the last tag before the position of the caret in
    // the current editor.
    public synchronized void updatePosition()
    {
        if (tags == null)
            return;
        final Position dot = editor.getDot();
        if (dot == null)
            return;
        final Line dotLine = dot.getLine();
        int index = -1;
        Line lastTagLine = null;
        final int size = tags.size();
        for (int i = 0; i < size; i++) {
            LocalTag t = (LocalTag) tags.get(i);
            if (t.getPosition().isAfter(dot)) {
                if (t.getLine() == dotLine && t.getLine() != lastTagLine) {
                    index = i;
                    lastTagLine = t.getLine();
                }
                break;
            } else {
                index = i;
                lastTagLine = t.getLine();
            }
        }
        if (index != getSelectedIndex()) {
            if (index >= 0) {
                setSelectedIndex(index);
                centerIndex(index);
            } else {
                clearSelection();
                ensureIndexIsVisible(0);
            }
        }
    }

    private synchronized void gotoTag()
    {
        if (tags == null)
            return;
        int index = getSelectedIndex();
        if (index >= 0 && index < tags.size()) {
            LocalTag tag = (LocalTag) tags.get(index);
            tag.gotoTag(editor);
        }
        editor.setFocusToDisplay();
    }

    public synchronized String getToolTipText(MouseEvent e)
    {
        if (tags != null) {
            int index = locationToIndex(e.getPoint());
            if (index >= 0 && index < tags.size()) {
                LocalTag t = (LocalTag) tags.get(index);
                return t.getToolTipText();
            }
        }
        return null;
    }

    public void keyPressed(KeyEvent e)
    {
        int keyCode = e.getKeyCode();
        int modifiers = e.getModifiers();
        // Mask off the bits we don't care about (Java 1.4).
        modifiers &= 0x0f;
        switch (keyCode) {
            // Ignore modifier keystrokes.
            case KeyEvent.VK_SHIFT:
            case KeyEvent.VK_CONTROL:
            case KeyEvent.VK_ALT:
            case KeyEvent.VK_META:
                return;
            case KeyEvent.VK_ENTER:
                e.consume();
                gotoTag();
                if (modifiers == KeyEvent.ALT_MASK)
                    sidebar.getFrame().frameToggleSidebar();
                return;
            case KeyEvent.VK_TAB:
                e.consume();
                if (modifiers == 0) {
                    if (sidebar.getBufferList() != null) {
                        updatePosition();
                        editor.setFocus(sidebar.getBufferList());
                    }
                }
                return;
            case KeyEvent.VK_ESCAPE:
                e.consume();
                sidebar.setBuffer();
                updatePosition();
                editor.setFocusToDisplay();
                return;
        }
        editor.getDispatcher().setEnabled(false);
    }

    public void keyReleased(KeyEvent e)
    {
        e.consume();
        editor.getDispatcher().setEnabled(true);
    }

    public void keyTyped(KeyEvent e)
    {
        e.consume();
    }

    public void mousePressed(MouseEvent e)
    {
        LocationBar.cancelInput();
        editor.ensureActive();
        // Mask off the bits we don't care about (Java 1.4).
        int modifiers = e.getModifiers() & 0x1f;
        if (modifiers == InputEvent.BUTTON1_MASK ||
            modifiers == InputEvent.BUTTON2_MASK) {
            if (modifiers == InputEvent.BUTTON2_MASK)
                setSelectedIndex(locationToIndex(e.getPoint()));
            paintImmediately(0, 0, getWidth(), getHeight());
            Editor.setCurrentEditor(editor);
            sidebar.setUpdateFlag(SIDEBAR_SET_BUFFER);
            gotoTag();
        } else
            editor.setFocusToDisplay();
    }

    public void mouseReleased(MouseEvent e) {}

    public void mouseClicked(MouseEvent e) {}

    public void mouseMoved(MouseEvent e)
    {
        String text = getToolTipText(e);
        sidebar.getFrame().setStatusText(text != null ? text : "");
    }

    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e)
    {
        final Frame frame = sidebar.getFrame();
        final StatusBar statusBar = frame.getStatusBar();
        if (statusBar != null) {
            statusBar.setText(null);
            statusBar.repaintNow();
        }
        // Force tool tip to be hidden.
        ToolTipManager.sharedInstance().setEnabled(false);
        ToolTipManager.sharedInstance().setEnabled(true);
        if (frame.getFocusedComponent() == this) {
            updatePosition();
            editor.setFocusToDisplay();
        }
    }

    public void mouseDragged(MouseEvent e) {}
}
