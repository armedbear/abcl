/*
 * Sidebar.java
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

package org.armedbear.j;

import java.awt.Color;
import javax.swing.BorderFactory;
import javax.swing.BoxLayout;
import javax.swing.JComponent;
import javax.swing.JScrollPane;
import javax.swing.SwingUtilities;

public final class Sidebar extends JComponent implements Constants
{
    private final Frame frame;
    private final SplitPane splitPane;
    private final SidebarPanel topPanel;
    private final SidebarPanel bottomPanel;
    private final SidebarBufferTree bufferTree;

    private NavigationComponent bottomComponent;
    private int updateFlag;

    public Sidebar(Frame frame)
    {
        this.frame = frame;
        setLayout(new BoxLayout(this, BoxLayout.Y_AXIS));
        topPanel = new SidebarPanel(this);
        bufferTree = new SidebarBufferTree(this);
        JScrollPane bufferListScrollPane = new JScrollPane(bufferTree);
        bufferListScrollPane.setAlignmentX(LEFT_ALIGNMENT);
        bufferListScrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
        bufferListScrollPane.setBorder(BorderFactory.createEmptyBorder());
        topPanel.addScrollPane(bufferListScrollPane);
        bottomPanel = new SidebarPanel(this);
        splitPane = new SplitPane(SplitPane.VERTICAL_SPLIT, topPanel, bottomPanel);
        splitPane.setBorder(BorderFactory.createEmptyBorder());
        splitPane.setDividerLocation(Editor.getSessionProperties().getSidebarDividerLocation(frame));
        add(splitPane);
    }

    public final Frame getFrame()
    {
        return frame;
    }

    public final Editor getEditor()
    {
        return frame.getCurrentEditor();
    }

    public final SidebarBufferTree getBufferList()
    {
        return bufferTree;
    }

    public final SidebarBufferTree getBufferTree()
    {
        return bufferTree;
    }

    public NavigationComponent getBottomComponent()
    {
        return bottomComponent;
    }

    public void activateBufferList()
    {
        if (bufferTree != null)
            frame.setFocus(bufferTree);
    }

    public void activateNavigationComponent()
    {
        if (bottomComponent != null)
            frame.setFocus((JComponent) bottomComponent);
    }

    public int getDividerLocation()
    {
        if (splitPane == null)
            return -1;

        return splitPane.getDividerLocation();
    }

    public final void setBufferListLabelText(String s)
    {
        topPanel.setLabelText(s);
    }

    public static void repaintBufferListInAllFrames()
    {
        for (int i = 0; i < Editor.getFrameCount(); i++) {
            Frame frame = Editor.getFrame(i);
            if (frame != null) {
                Sidebar sidebar = frame.getSidebar();
                if (sidebar != null)
                    sidebar.getBufferList().repaint();
            }
        }
    }

    public void setBuffer()
    {
        if (bufferTree != null) {
            Buffer buffer = frame.getCurrentEditor().getBuffer();
            if (buffer != bufferTree.getSelectedBuffer())
                bufferTree.setSelectedBuffer(buffer);
        }
    }

    public void updatePosition()
    {
        if (bottomComponent != null)
            bottomComponent.updatePosition();
    }

    public static final boolean isTaggable(Buffer buffer)
    {
        Mode mode = buffer.getMode();
        return mode != null && mode.isTaggable();
    }

    public synchronized final void setUpdateFlag(int mask)
    {
        updateFlag |= mask;
        if (bufferTree != null)
            bufferTree.setUpdateFlag(updateFlag & SIDEBAR_BUFFER_LIST_ALL);
    }

    public static void setUpdateFlagInAllFrames(int mask)
    {
        for (int i = 0; i < Editor.getFrameCount(); i++) {
            Frame frame = Editor.getFrame(i);
            Sidebar sidebar = frame.getSidebar();
            if (sidebar != null)
                sidebar.setUpdateFlag(mask);
        }
    }

    private void setBottomComponent()
    {
        if (!SwingUtilities.isEventDispatchThread())
            Debug.bug("Sidebar.setBottomComponent() called from background thread!");
        if (bottomComponent != null) {
            Editor editor = frame.getCurrentEditor();
            Buffer buffer = editor.getBuffer();
            if (frame.getEditorCount() == 2 && buffer.isTransient()) {
                editor = frame.getOtherEditor();
                buffer = editor.getBuffer();
            }
            if (isTaggable(buffer) && bottomComponent instanceof SidebarTagList) {
                SidebarTagList list = (SidebarTagList) bottomComponent;
                list.setEditor(editor);
                if (list.getBuffer() == buffer)
                    return;
            } else {
                View view = editor.getCurrentView();
                if (view != null && bottomComponent == view.getSidebarComponent())
                    return;
            }
            // Reaching here, we need to remove the old bottom component.
            bottomComponent = null;
            bottomPanel.removeAll();
        }
        Debug.assertTrue(bottomComponent == null);
        final Editor editor = getEditor();
        bottomComponent = editor.getMode().getSidebarComponent(editor);
        if (bottomComponent != null) {
            JScrollPane scrollPane = new JScrollPane((JComponent)bottomComponent);
            if (bottomComponent instanceof SidebarList)
                scrollPane.setHorizontalScrollBarPolicy(JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
            scrollPane.setAlignmentX(LEFT_ALIGNMENT);
            scrollPane.setBorder(BorderFactory.createEmptyBorder());
            bottomPanel.addScrollPane(scrollPane);
            bottomPanel.validate();
        } else
            bottomPanel.repaint();
    }

    public synchronized void refreshSidebar()
    {
        if (updateFlag != 0) {
            if ((updateFlag & SIDEBAR_BUFFER_LIST_ALL) != 0) {
                SidebarBufferTree bufferTree = getBufferTree();
                if (bufferTree != null && bufferTree != frame.getFocusedComponent())
                    bufferTree.updateBufferList();
            }
            if (bottomComponent == null || bottomComponent != frame.getFocusedComponent()) {
                setBottomComponent();
                if (bottomComponent != null) {
                    bottomPanel.setLabelText(bottomComponent.getLabelText());
                    // This might start a thread.
                    bottomComponent.refresh();
                    if ((updateFlag & SIDEBAR_POSITION) != 0)
                        bottomComponent.updatePosition();
                }
            }
            updateFlag = 0;
        }
    }

    public static void refreshSidebarInAllFrames()
    {
        if (!SwingUtilities.isEventDispatchThread())
            Debug.bug("refreshSidebarInAllFrames() called from background thread!");
         for (int i = 0; i < Editor.getFrameCount(); i++) {
            Frame frame = Editor.getFrame(i);
            Sidebar sidebar = frame.getSidebar();
            if (sidebar != null)
                sidebar.refreshSidebar();
        }
    }
}
