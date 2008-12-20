/*
 * FolderTree.java
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

package org.armedbear.j.mail;

import java.awt.Color;
import java.awt.Component;
import java.awt.Graphics;
import java.awt.Point;
import java.awt.event.InputEvent;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.Enumeration;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;
import org.armedbear.j.Buffer;
import org.armedbear.j.Display;
import org.armedbear.j.Editor;
import org.armedbear.j.Frame;
import org.armedbear.j.NavigationComponent;
import org.armedbear.j.Utilities;

public final class FolderTree extends JTree implements NavigationComponent,
    MouseListener
{
    private final Frame frame;

    private FolderTree(Frame frame)
    {
        super(FolderTreeModel.getDefaultModel());
        this.frame = frame;
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        setRootVisible(false);
        toggleClickCount = 1;
        setCellRenderer(new TreeCellRenderer(this));
        addMouseListener(this);
    }

    public static FolderTree getInstance(Frame frame)
    {
        Object obj = frame.getFolderTree();
        if (obj instanceof FolderTree)
            return (FolderTree) obj;
        FolderTree tree = new FolderTree(frame);
        frame.setFolderTree(tree);
        return tree;
    }

    public void refresh() {}

    public void updatePosition()
    {
        int row = -1;
        final Buffer buf = frame.getCurrentEditor().getBuffer();
        MailboxURL url = null;
        if (buf instanceof Mailbox)
            url = ((Mailbox)buf).getUrl();
        else if (buf instanceof MessageBuffer) {
            Mailbox mailbox = ((MessageBuffer)buf).getMailbox();
            if (mailbox != null)
                url = mailbox.getUrl();
        }
        if (url != null) {
            DefaultMutableTreeNode root =
                (DefaultMutableTreeNode) getModel().getRoot();
            if (root != null) {
                Enumeration nodes = root.depthFirstEnumeration();
                while (nodes.hasMoreElements()) {
                    DefaultMutableTreeNode node =
                        (DefaultMutableTreeNode) nodes.nextElement();
                    if (node.getUserObject() instanceof Folder) {
                        Folder folder = (Folder) node.getUserObject();
                        if (url.equals(folder.getUrl())) {
                            scrollPathToVisible(new TreePath(node.getPath()));
                            final int limit = getRowCount();
                            for (int i = 0; i < limit; i++) {
                                TreePath treepath = getPathForRow(i);
                                if (treepath != null &&
                                    treepath.getLastPathComponent() == node) {
                                    row = i;
                                    break;
                                }
                            }
                        }
                    }
                }
            }
        }
        if (row >= 0)
            setSelectionRow(row);
        else
            clearSelection();
        repaint();
    }

    public final String getLabelText()
    {
        return "Folders";
    }

    public void mousePressed(MouseEvent e) {}

    public void mouseReleased(MouseEvent e) {}

    public void mouseClicked(MouseEvent e)
    {
        final Editor editor = frame.getCurrentEditor();
        final int modifiers = e.getModifiers();
        if (modifiers != InputEvent.BUTTON1_MASK &&
            modifiers != InputEvent.BUTTON2_MASK) {
            e.consume();
            editor.setFocusToDisplay();
            return;
        }
        Point point = e.getPoint();
        TreePath treePath = getPathForLocation(point.x, point.y);
        if (treePath != null) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) treePath.getLastPathComponent();
            Object object = node.getUserObject();
            if (object instanceof Folder) {
                Folder folder = (Folder) object;
                MailboxURL url = folder.getUrl();
                editor.setWaitCursor();
                MailCommands.openMailbox(editor, url);
                editor.updateDisplay();
                editor.setDefaultCursor();
            }
        }
        editor.setFocusToDisplay();
    }

    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e)
    {
        updatePosition();
        frame.getCurrentEditor().setFocusToDisplay();
    }

    private static class TreeCellRenderer extends DefaultTreeCellRenderer
    {
        private static final Color noFocusSelectionBackground =
            new Color(208, 208, 208);

        private JTree tree;
        private Color oldBackgroundSelectionColor;

        TreeCellRenderer(JTree tree)
        {
            super();
            this.tree = tree;
            oldBackgroundSelectionColor = getBackgroundSelectionColor();
            setOpenIcon(Utilities.getIconFromFile("dir_open.png"));
            setClosedIcon(Utilities.getIconFromFile("dir_close.png"));
            setLeafIcon(Utilities.getIconFromFile("mailbox.png"));
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
            if (Editor.getCurrentFrame().getFocusedComponent() == tree)
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
