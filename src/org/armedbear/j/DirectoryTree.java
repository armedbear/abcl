/*
 * DirectoryTree.java
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
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.MouseMotionListener;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeExpansionEvent;
import javax.swing.event.TreeExpansionListener;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public final class DirectoryTree extends SidebarTree implements NavigationComponent,
    TreeSelectionListener, TreeExpansionListener, MouseListener,
    MouseMotionListener, KeyListener
{
    private Editor editor;
    private DirectoryTreeModel treeModel;

    private DirectoryTree(Editor editor, TreeModel model)
    {
        super(model);
        this.editor = editor;
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        setScrollsOnExpand(false);
        addTreeSelectionListener(this);
        addTreeExpansionListener(this);
        addMouseListener(this);
        addMouseMotionListener(this);
        addKeyListener(this);
        setCellRenderer(new DirectoryTreeCellRenderer(this));
    }

    public static DirectoryTree getDirectoryTree(Editor editor)
    {
        if (editor.getCurrentDirectory().isLocal()) {
            if (editor.localDirectoryTree == null)
                editor.localDirectoryTree = new DirectoryTree(editor, null);
            return editor.localDirectoryTree;
        }
        return new DirectoryTree(editor, null);
    }

    public final String getLabelText()
    {
        return editor.getBuffer().getFile().getName();
    }

    public DirectoryTreeModel getTreeModel()
    {
        return treeModel;
    }

    public Editor getEditor()
    {
        return editor;
    }

    public void refresh()
    {
        if (!SwingUtilities.isEventDispatchThread())
            Debug.bug("DirectoryTree.refresh() called from background thread!");
        Runnable refreshRunnable = new Runnable() {
            public void run()
            {
                File file = editor.getBuffer().getFile();
                if (file == null)
                    return;
                if (treeModel == null) {
                    treeModel = DirectoryTreeModel.getTreeModel(file);
                    if (treeModel != null) {
                        final DefaultMutableTreeNode node = getNode(file);
                        Runnable completionRunnable = new Runnable() {
                            public void run()
                            {
                                setModel(treeModel);
                                if (node != null)
                                    scrollNodeToCenter(node);
                            }
                        };
                        SwingUtilities.invokeLater(completionRunnable);
                    }
                } else {
                    DefaultMutableTreeNode selectedNode = null;
                    TreePath path = getSelectionPath();
                    if (path != null) {
                        selectedNode =
                            (DefaultMutableTreeNode) path.getLastPathComponent();
                    }
                    final DefaultMutableTreeNode node = getNode(file);
                    if (node != null && node != selectedNode) {
                        Runnable completionRunnable = new Runnable() {
                            public void run()
                            {
                                scrollNodeToCenter(node);
                            }
                        };
                        SwingUtilities.invokeLater(completionRunnable);
                    }
                }
            }
        };
        new Thread(refreshRunnable).start();
    }

    private void expandNode(DefaultMutableTreeNode node)
    {
        DirectoryTreeElement treeElement =
            (DirectoryTreeElement) node.getUserObject();
        File file = treeElement.getFile();
        expandNode(node, file);
    }

    private void expandNode(DefaultMutableTreeNode node, File file)
    {
        treeModel.expandNode(node, file);
    }

    private DefaultMutableTreeNode getNode(File file)
    {
        if (treeModel == null || file == null)
            return null;
        return treeModel.getNode(file);
    }

    public void updatePosition()
    {
        int limit = getRowCount();
        int rowToBeSelected = -1;
        String path = editor.getBuffer().getFile().canonicalPath();
        for (int row = 0; row < limit; row++) {
            TreePath treepath = getPathForRow(row);
            if (treepath != null) {
                DefaultMutableTreeNode node =
                    (DefaultMutableTreeNode) treepath.getLastPathComponent();
                if (node != null) {
                    // On Windows the user object might be a string (host name).
                    if (node.getUserObject() instanceof DirectoryTreeElement) {
                        DirectoryTreeElement treeElement =
                            (DirectoryTreeElement) node.getUserObject();
                        if (path.equals(treeElement.getPath())) {
                            rowToBeSelected = row;
                            break;
                        }
                    }
                }
            }
        }
        if (rowToBeSelected >= 0) {
            int[] selectedRows = getSelectionRows();
            if (selectedRows != null && selectedRows.length == 1 &&
                selectedRows[0] == rowToBeSelected)
                ; // No change.
            else {
                setSelectionRow(rowToBeSelected);
                scrollRowToCenter(rowToBeSelected);
            }
        } else
            clearSelection();
    }

    public void valueChanged(TreeSelectionEvent e) {}

    public void treeCollapsed(TreeExpansionEvent e) {}

    public void treeExpanded(TreeExpansionEvent e) {}

    public void keyPressed(KeyEvent e)
    {
        int keyCode = e.getKeyCode();
        int modifiers = e.getModifiers();

        switch (keyCode) {
            // Ignore modifier keystrokes.
            case KeyEvent.VK_SHIFT:
            case KeyEvent.VK_CONTROL:
            case KeyEvent.VK_ALT:
            case KeyEvent.VK_META:
                return;

            case KeyEvent.VK_ENTER: {
                e.consume();
                TreePath path = getSelectionPath();
                if (path != null) {
                    DefaultMutableTreeNode node =
                        (DefaultMutableTreeNode) path.getLastPathComponent();
                    if (node.getUserObject() instanceof DirectoryTreeElement) {
                        DirectoryTreeElement treeElement =
                            (DirectoryTreeElement) node.getUserObject();
                        File file = treeElement.getFile();
                        editor.setWaitCursor();
                        expandNode(node, file);
                        editor.setDefaultCursor();
                        if (e.getModifiers() == InputEvent.BUTTON2_MASK)
                            editor.setFocusToDisplay();
                        repaint();
                        ((Directory)editor.getBuffer()).changeDirectory(file);
                        editor.updateDisplay();
                    }
                }
                editor.setFocusToDisplay();
                if (modifiers == KeyEvent.ALT_MASK)
                    editor.toggleSidebar();
                return;
            }

            case KeyEvent.VK_TAB:
                e.consume();
                if (modifiers == 0) {
                    if (editor.getSidebar().getBufferList() != null)
                        editor.setFocus(editor.getSidebar().getBufferList());
                }
                return;

            case KeyEvent.VK_ESCAPE:
                e.consume();
                editor.getSidebar().setBuffer();
                editor.getSidebar().updatePosition();
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

    private boolean ignoreMouseClicked;

    public void mousePressed(MouseEvent e)
    {
        ignoreMouseClicked = false;
        LocationBar.cancelInput();
        editor.ensureActive();
        int modifiers = e.getModifiers();
        if (modifiers == InputEvent.BUTTON1_MASK ||
            modifiers == InputEvent.BUTTON2_MASK) {
            Point point = e.getPoint();
            if (e.getModifiers() == InputEvent.BUTTON2_MASK) {
                int row = getRowForLocation(point.x, point.y);
                setSelectionRow(row);
            }
            editor.setFocus(this);
            TreePath treepath = getPathForLocation(point.x, point.y);
            if (treepath == null) {
                ignoreMouseClicked = true;
                e.consume();
            }
        } else
            editor.setFocusToDisplay();
    }

    public void mouseReleased(MouseEvent e) {}

    public void mouseClicked(MouseEvent e)
    {
        if (ignoreMouseClicked) {
            e.consume();
            editor.setFocusToDisplay();
            return;
        }
        final int modifiers = e.getModifiers();
        if (modifiers != InputEvent.BUTTON1_MASK &&
            modifiers != InputEvent.BUTTON2_MASK) {
            e.consume();
            editor.setFocusToDisplay();
            return;
        }
        Point point = e.getPoint();
        TreePath treepath = getPathForLocation(point.x, point.y);
        if (treepath != null) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) treepath.getLastPathComponent();
            DirectoryTreeElement treeElement =
                (DirectoryTreeElement) node.getUserObject();
            File file = treeElement.getFile();
            editor.setWaitCursor();
            expandNode(node, file);
            expandPath(treepath);
            editor.setDefaultCursor();
            if (modifiers == InputEvent.BUTTON1_MASK ||
                modifiers == InputEvent.BUTTON2_MASK)
                editor.setFocusToDisplay();
            repaint();
            ((Directory)editor.getBuffer()).changeDirectory(file);
            editor.updateDisplay();
        }
    }

    public void mouseMoved(MouseEvent e) {}

    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e)
    {
        editor.setFocusToDisplay();
    }

    public void mouseDragged(MouseEvent e) {}

    private static class DirectoryTreeCellRenderer extends DefaultTreeCellRenderer
    {
        private DirectoryTree tree;
        private Editor editor;

        private static Color noFocusSelectionBackground =
            new Color(208, 208, 208);

        private Color oldBackgroundSelectionColor;

        public DirectoryTreeCellRenderer(DirectoryTree tree)
        {
            super();
            this.tree = tree;
            editor = tree.getEditor();
            oldBackgroundSelectionColor = getBackgroundSelectionColor();

            setOpenIcon(Utilities.getIconFromFile("dir_open.png"));
            setClosedIcon(Utilities.getIconFromFile("dir_close.png"));
            setLeafIcon(Utilities.getIconFromFile("dir_close.png"));
        }

        public Component getTreeCellRendererComponent(
            JTree tree,
            Object value,
            boolean selected,
            boolean expanded,
            boolean leaf,
            int row,
            boolean hasFocus)
        {
            super.getTreeCellRendererComponent(tree, value, selected, expanded,
                                               leaf, row, hasFocus);
            if (selected)
                super.setForeground(getTextSelectionColor());
            else
                super.setForeground(getTextNonSelectionColor());
            if (editor.getFocusedComponent() == tree)
                setBackgroundSelectionColor(oldBackgroundSelectionColor);
            else
                setBackgroundSelectionColor(noFocusSelectionBackground);
            return this;
        }

        public void paintComponent(Graphics g)
        {
            Display.setRenderingHints(g);
            super.paintComponent(g);
        }
    }
}
