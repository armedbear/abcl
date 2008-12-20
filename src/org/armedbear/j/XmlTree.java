/*
 * XmlTree.java
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
import java.io.StringReader;
import java.util.Enumeration;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.event.TreeSelectionEvent;
import javax.swing.event.TreeSelectionListener;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public final class XmlTree extends JTree implements Constants, NavigationComponent,
    TreeSelectionListener, MouseListener, MouseMotionListener, KeyListener
{
    private final Editor editor;
    private final Buffer buffer;
    private String parserClassName;
    private boolean aelfred;
    private boolean xp;
    private int modificationCount = -1;
    private boolean disabled;

    public XmlTree(Editor editor, TreeModel model)
    {
        super(model);
        this.editor = editor;
        this.buffer = editor.getBuffer();
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        addTreeSelectionListener(this);
        addMouseListener(this);
        addMouseMotionListener(this);
        addKeyListener(this);
        setCellRenderer(new XmlTreeCellRenderer(this));
    }

    public final String getLabelText()
    {
        return buffer.getFile() != null ? buffer.getFile().getName() : null;
    }

    public void setParserClassName(String className)
    {
        parserClassName = className;
        aelfred = false;
        xp = false;
        if (parserClassName.equals("org.armedbear.j.aelfred.SAXDriver"))
            aelfred = true;
        else if (parserClassName.equals("com.jclark.xml.sax.Driver"))
            xp = true;
    }

    public final Editor getEditor()
    {
        return editor;
    }

    public synchronized void refresh()
    {
        if (!SwingUtilities.isEventDispatchThread())
            Debug.bug("XmlTree.refresh() called from background thread!");
        if (disabled)
            return;
        if (modificationCount == buffer.getModCount())
            return;
        final XmlParserImpl parser = new XmlParserImpl(buffer);
        if (!parser.initialize())
            return;
        modificationCount = buffer.getModCount();
        try {
            final String text = buffer.getText();
            if (text.length() < 7) // "<a></a>"
                return;
            parser.setReader(new StringReader(text));
        }
        catch (OutOfMemoryError e) {
            outOfMemory();
            return;
        }
        Runnable parseBufferRunnable = new Runnable() {
            public void run()
            {
                try {
                    parser.run();
                }
                catch (OutOfMemoryError e) {
                    outOfMemory();
                    return;
                }
                if (parser.getException() == null) {
                    final TreeModel treeModel = parser.getTreeModel();
                    if (treeModel != null) {
                        setParserClassName(parser.getParserClassName());
                        Runnable r = new Runnable() {
                            public void run()
                            {
                                setModel(treeModel);
                                if (editor.getBuffer() == buffer)
                                    XmlMode.ensureCurrentNodeIsVisible(editor,
                                        XmlTree.this);
                            }
                        };
                        SwingUtilities.invokeLater(r);
                    }
                }
            }
        };
        new Thread(parseBufferRunnable).start();
    }

    // Update the selected node in the tree, based on the position of dot in
    // the edit buffer.
    public void updatePosition()
    {
        if (disabled)
            return;
        Position dot = editor.getDot();
        if (dot == null)
            return;
        final Line dotLine = dot.getLine();
        final int dotOffset = dot.getOffset();

        // Our line numbers are zero-based.
        final int dotLineNumber = editor.getDotLineNumber();

        int rowToBeSelected = -1;
        final int limit = getRowCount();
        for (int row = 0; row < limit; row++) {
            TreePath path = getPathForRow(row);
            if (path != null) {
                DefaultMutableTreeNode node =
                    (DefaultMutableTreeNode) path.getLastPathComponent();
                if (node != null) {
                    XmlTreeElement treeElement =
                        (XmlTreeElement) node.getUserObject();
                    // Tree element line numbers are one-based, so subtract 1.
                    final int lineNumber = treeElement.getLineNumber() - 1;
                    if (lineNumber == dotLineNumber) {
                        // Tree element column numbers are one-based, so
                        // subtract 1.
                        int columnNumber = treeElement.getColumnNumber();
                        if (columnNumber > 0) {
                            // Tree element column numbers are one-based, so
                            // subtract 1.
                            --columnNumber;
                        }
                        int index;
                        if (columnNumber < 0) {
                            // Crimson always reports -1 ("maintaining column
                            // numbers hurts performance").
                            index = findStartTag(treeElement.getName(),
                                dotLine, 0);
                        } else if (xp) {
                            // Position reported by XP is '<' of start tag.
                            index = columnNumber;
                        } else {
                            // Position reported by parser is next char after
                            // '>' of start tag. Start reverse search 1 back
                            // from there.
                            index = reverseFindStartTag(treeElement.getName(),
                                dotLine, columnNumber);
                        }
                        if (aelfred && index < 0) {
                            // Aelfred's locator is very sloppy. Try forward
                            // search.
                            index = findStartTag(treeElement.getName(),
                                dotLine, columnNumber);
                        }
                        // Make sure index is sane (the tree may need refreshing).
                        if (index < 0)
                            index = 0;
                        else if (index > dotLine.length())
                            index = dotLine.length();
                        if (dotOffset == index) {
                            rowToBeSelected = row;
                            break;
                        } else if (dotOffset < index) {
                            // If dot is in whitespace at the beginning of the
                            // line, immediately to the left of the current
                            // node's start tag, we want the current node.
                            if (Utilities.isWhitespace(dotLine.substring(0, index)))
                                rowToBeSelected = row;
                            break;
                        }
                    } else if (lineNumber > dotLineNumber)
                        break;
                    rowToBeSelected = row;
                }
            }
        }

        if (rowToBeSelected >= 0) {
            setSelectionRow(rowToBeSelected);
            scrollRowToVisible(rowToBeSelected);
        } else
            clearSelection();

        repaint();
    }

    private void outOfMemory()
    {
        disabled = true;
        treeModel = null;
        MessageDialog.showMessageDialog(
            "Not enough memory to display tree",
            "XML Mode");
    }

    public void valueChanged(TreeSelectionEvent e)
    {
        if (editor.getFocusedComponent() != this)
            return;
        if (editor.getStatusBar() == null)
            return;
        String statusText = "";
        DefaultMutableTreeNode node =
            (DefaultMutableTreeNode) getLastSelectedPathComponent();
        if (node != null) {
            XmlTreeElement treeElement = (XmlTreeElement) node.getUserObject();
            statusText = treeElement.getStatusText();
        }
        editor.status(statusText);
    }

    public void keyPressed(KeyEvent e)
    {
        final int keyCode = e.getKeyCode();
        final int modifiers = e.getModifiers();
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
                    if (node != null)
                        moveDotToNode(node);
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

    public void mousePressed(MouseEvent e)
    {
        LocationBar.cancelInput();
        editor.ensureActive();
        final int modifiers = e.getModifiers();
        if (modifiers == InputEvent.BUTTON1_MASK ||
            modifiers == InputEvent.BUTTON2_MASK) {
            editor.setFocus(this);
            if (modifiers == InputEvent.BUTTON2_MASK) {
                int row = getRowForLocation(e.getX(), e.getY());
                if (row >= 0)
                    setSelectionRow(row);
            }
        } else
            editor.setFocusToDisplay();
    }

    public void mouseReleased(MouseEvent e)
    {
    }

    public void mouseClicked(MouseEvent e)
    {
        final int modifiers = e.getModifiers();
        if (modifiers == InputEvent.BUTTON1_MASK ||
            modifiers == InputEvent.BUTTON2_MASK) {
            Point point = e.getPoint();
            moveDotToNodeAtPoint(point);
        }
        editor.setFocusToDisplay();
    }

    public void mouseMoved(MouseEvent e)
    {
        if (editor.getStatusBar() == null)
            return;
        String statusText = "";
        Point point = e.getPoint();
        TreePath path = getPathForLocation(point.x, point.y);
        if (path != null) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) path.getLastPathComponent();
            if (node != null) {
                XmlTreeElement treeElement =
                    (XmlTreeElement) node.getUserObject();
                statusText = treeElement.getStatusText();
            }
            editor.status(statusText);
        }
    }

    public void mouseEntered(MouseEvent e)
    {
    }

    public void mouseExited(MouseEvent e)
    {
        editor.setFocusToDisplay();
        if (editor.getStatusBar() != null) {
            editor.getStatusBar().setText(null);
            editor.getStatusBar().repaintNow();
        }
    }

    public void mouseDragged(MouseEvent e)
    {
    }

    private void moveDotToNode(DefaultMutableTreeNode node)
    {
        if (node == null)
            return;
        XmlTreeElement treeElement = (XmlTreeElement) node.getUserObject();
        String name = treeElement.getName();

        // Subtract 1 since our line numbers are zero-based.
        int lineNumber = treeElement.getLineNumber() - 1;

        Editor editor = Editor.currentEditor();
        Line line = editor.getBuffer().getLine(lineNumber);
        if (line != null) {
            int offset;
            if (treeElement.getColumnNumber() < 0) {
                // Crimson always reports -1.
                offset = findStartTag(name, line, 0);
            } else if (xp) {
                // Position reported by XP is '<' of start tag.
                offset = treeElement.getColumnNumber()-1;
            } else {
                offset = 0;

                // The line and column numbers stored in the tree element
                // refer (in theory) to the position just past the end of the
                // start tag. Subtract 1 since our offsets are zero-based.
                int endOfStartTag = treeElement.getColumnNumber()-1;

                if (endOfStartTag >= 0) {
                    if (aelfred) {
                        // Aelfred.
                        // Look for start of start tag.
                        int startOfStartTag =
                            reverseFindStartTag(name, line, endOfStartTag - 1);
                        if (startOfStartTag < 0)
                            startOfStartTag = findStartTag(name, line, endOfStartTag);
                        if (startOfStartTag >= 0)
                            offset = startOfStartTag;
                    } else {
                        // Not Aelfred.
                        // Look for start of start tag.
                        int startOfStartTag =
                            reverseFindStartTag(name, line, endOfStartTag - 1);
                        while (startOfStartTag < 0) {
                            // Not found on current line. Look at previous line.
                            if (line.previous() == null)
                                break;
                            line = line.previous();
                            startOfStartTag = reverseFindStartTag(name, line,
                                line.length());
                        }
                        if (startOfStartTag >= 0)
                            offset = startOfStartTag;
                    }
                }
            }
            editor.addUndo(SimpleEdit.MOVE);
            if (editor.getMark() != null) {
                editor.setMark(null);
                editor.setUpdateFlag(REPAINT);
            }
            editor.update(editor.getDotLine());
            // Make sure offset is sane.
            if (offset < 0)
                offset = 0;
            else if (offset > line.length())
                offset = line.length();
            editor.setDot(line, offset);
            editor.update(editor.getDotLine());

            // Make sure end tag is visible if possible.
            Position end = findMatchingEndTagOnSameLine(name, editor.getDot());

            if (end != null)
                end.skip(name.length() + 3); // "</" + name + ">"
            else
                end = new Position(line, line.length() - 1);
            int absCol = buffer.getCol(end);
            editor.getDisplay().ensureColumnVisible(line, absCol);
            editor.moveCaretToDotCol();
            editor.updateDisplay();
        }
    }

    private void moveDotToNodeAtPoint(Point point)
    {
        TreePath path = getPathForLocation(point.x, point.y);
        if (path != null) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) path.getLastPathComponent();
            moveDotToNode(node);
        }
    }

    public DefaultMutableTreeNode getNodeAtPos(Position where)
    {
        if (treeModel == null)
            return null;
        DefaultMutableTreeNode root =
            (DefaultMutableTreeNode) treeModel.getRoot();
        if (root == null)
            return null;

        // Search backwards from starting point to find nearest '<' (start of
        // current node).
        Position pos = new Position(where);
        while (pos.getChar() != '<')
            if (!pos.prev())
                break;

        // Skip past '<'.
        pos.next();

        // One more for good measure. (Aelfred is sloppy!)
        pos.next();

        // The starting location reported by the parser and stored in the tree
        // element refers (in theory) to the position just past the end of the
        // start tag. We want to find the node whose reported starting
        // location is after pos, but closest to it.

        // Our line numbers and offsets are zero-based.
        int targetLineNumber = pos.lineNumber() + 1;
        int targetColumnNumber = pos.getOffset() + 1;

        DefaultMutableTreeNode currentNode = null;
        int currentLineDelta = Integer.MAX_VALUE;
        int currentColumnDelta = Integer.MAX_VALUE;
        Enumeration nodes = root.depthFirstEnumeration();
        while (nodes.hasMoreElements()) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) nodes.nextElement();
            XmlTreeElement treeElement = (XmlTreeElement) node.getUserObject();
            int lineDelta = treeElement.getLineNumber() - targetLineNumber;

            // We want the smallest lineDelta >= 0.
            if (lineDelta >= 0 && lineDelta < currentLineDelta) {
                currentNode = node;
                currentLineDelta = lineDelta;
            }
            if (lineDelta == 0 && currentLineDelta == 0) {
                int columnDelta =
                    treeElement.getColumnNumber() - targetColumnNumber;

                // We want the smallest columnDelta >= 0.
                if (columnDelta >= 0 && columnDelta < currentColumnDelta) {
                    currentNode = node;
                    currentColumnDelta = columnDelta;
                }
            }
        }
        return currentNode;
    }

    private int findStartTag(String name, Line line, int start)
    {
        final String lookFor = '<' + name;
        final int length = lookFor.length();
        while (start + length < line.length()) {
            int index = line.getText().indexOf(lookFor, start);
            if (index < 0)
                return index; // Not found.
            int end = index + length;
            if (end < line.length()) {
                char c = line.charAt(end);
                if (c == '/' || c == '>' || c <= ' ')
                    return index;
            }
            start = index + 1;
        }
        return -1; // Not found.
    }

    private int reverseFindStartTag(String name, Line line, int start)
    {
        final String lookFor = '<' + name;
        final int length = lookFor.length();
        while (start >= 0) {
            int index = line.getText().lastIndexOf(lookFor, start);
            if (index < 0)
                return index; // Not found.
            int end = index + length;
            if (end < line.length()) {
                char c = line.charAt(end);
                if (c == '/' || c == '>' || c <= ' ')
                    return index;
            } else
                return index;
            start = index - lookFor.length();
        }
        return -1; // Not found.
    }

    private static final String COMMENT_START = "<!--";
    private static final String COMMENT_END = "-->";

    private Position findMatchingEndTagOnSameLine(String name, Position start)
    {
        String toBeMatched = "<" + name;
        String match = "</" + name + ">";
        int count = 1;
        Position pos = new Position(start);
        pos.skip(toBeMatched.length());
        int limit = pos.getLineLength();
        while(pos.getOffset() < limit) {
            if (pos.lookingAt(COMMENT_START)) {
                pos.skip(COMMENT_START.length());
                while (pos.getOffset() < limit) {
                    if (pos.lookingAt(COMMENT_END)) {
                        pos.skip(COMMENT_END.length());
                        break;
                    }
                    pos.skip(1);
                }
            } else if (pos.lookingAtIgnoreCase(toBeMatched)) {
                pos.skip(toBeMatched.length());
                char c = pos.getChar();
                if (c <= ' ' || c == '>') {
                    ++count;
                    pos.skip(1);
                }
            } else if (pos.lookingAtIgnoreCase(match)) {
                --count;
                if (count == 0)
                    return pos;
                pos.skip(match.length());
            } else
                pos.skip(1);
        }
        return null;
    }

    private static class XmlTreeCellRenderer extends DefaultTreeCellRenderer
    {
        private XmlTree tree;
        private Editor editor;

        private static Color noFocusSelectionBackground =
            new Color(208, 208, 208);

        private Color oldBackgroundSelectionColor;

        public XmlTreeCellRenderer(XmlTree tree)
        {
            super();
            this.tree = tree;
            editor = tree.getEditor();
            oldBackgroundSelectionColor = getBackgroundSelectionColor();
            setOpenIcon(Utilities.getIconFromFile("branch.png"));
            setClosedIcon(Utilities.getIconFromFile("branch.png"));
            setLeafIcon(Utilities.getIconFromFile("leaf.png"));
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
