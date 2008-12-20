/*
 * JavaTree.java
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
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.Enumeration;
import java.util.List;
import javax.swing.Icon;
import javax.swing.JTree;
import javax.swing.SwingUtilities;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeCellRenderer;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;
import javax.swing.tree.TreeSelectionModel;

public final class JavaTree extends SidebarTree implements Constants,
    NavigationComponent, KeyListener, MouseListener
{
    private static final String CAPTION_FIELDS = "Fields";
    private static final String CAPTION_CONSTRUCTORS = "Constructors";
    private static final String CAPTION_METHODS = "Methods";
    private static final String CAPTION_NESTED_CLASSES = "Nested Classes";

    private static final String KEY_ARRANGE_BY_TYPE =
        "JavaMode.tree.arrangeByType";
    private static final String KEY_SORT = "JavaMode.tree.sort";

    private static boolean arrangeByType =
        Editor.getSessionProperties().getBooleanProperty(KEY_ARRANGE_BY_TYPE, true);
    private static boolean sort =
        Editor.getSessionProperties().getBooleanProperty(KEY_SORT, false);

    private final Editor editor;
    private final Frame frame;
    private List tags;
    private boolean arrangedByType;
    private boolean sorted;

    public static final void setArrangeByType(boolean b)
    {
        if (b != arrangeByType) {
            arrangeByType = b;
            Editor.getSessionProperties().setBooleanProperty(KEY_ARRANGE_BY_TYPE, b);
        }
    }

    public static final boolean getArrangeByType()
    {
        return arrangeByType;
    }

    public static final void setSort(boolean b)
    {
        if (b != sort) {
            sort = b;
            Editor.getSessionProperties().setBooleanProperty(KEY_SORT, b);
        }
    }

    public static final boolean getSort()
    {
        return sort;
    }

    public JavaTree(Editor editor)
    {
        super((TreeModel)null);
        this.editor = editor;
        frame = editor.getFrame();
        getSelectionModel().setSelectionMode(TreeSelectionModel.SINGLE_TREE_SELECTION);
        setRootVisible(false);
        setCellRenderer(new TreeCellRenderer());
        setFocusTraversalKeysEnabled(false);
        addKeyListener(this);
        addMouseListener(this);
        setToolTipText("");
    }

    public void refresh()
    {
        boolean force = (arrangedByType != arrangeByType) ||
            (sorted != sort);
        refresh(force);
    }

    public void refresh(boolean force)
    {
        final Buffer buffer = editor.getBuffer();
        final List bufferTags = buffer.getTags();
        if (!force)
            if (tags != null && tags == bufferTags)
                return; // Nothing to do.
        Runnable r = new Runnable() {
            public void run()
            {
                refreshInternal(buffer, bufferTags);
            }
        };
        Thread thread = new Thread(r, "JavaTree.refresh()");
        thread.setDaemon(true);
        thread.start();
    }

    private void refreshInternal(Buffer buffer, List bufferTags)
    {
        if (bufferTags == null)
            bufferTags = buffer.getTags(true); // Runs tagger synchronously.
        if (bufferTags != null) {
            final TreeModel model =
                getDefaultModel(bufferTags, arrangeByType, sort);
            final List finalBufferTags = bufferTags;
            Runnable completionRunnable = new Runnable() {
                public void run()
                {
                    setModel(model);
                    arrangedByType = arrangeByType;
                    sorted = sort;
                    tags = finalBufferTags;
                    expandRow(0);
                    updatePosition();
                }
            };
            SwingUtilities.invokeLater(completionRunnable);
        }
    }

    // Never returns null!
    private static TreeModel getDefaultModel(List bufferTags,
        boolean arrangeByType, boolean sort)
    {
        DefaultMutableTreeNode rootNode = new DefaultMutableTreeNode();
        List list;
        if (sort)
            list = sort(bufferTags);
        else
            list = bufferTags;
        final int size = list.size();
        for (int i = 0; i < size; i++) {
            JavaTag tag = (JavaTag) list.get(i);
            JavaClass parent = tag.getParent();
            if (parent == null) {
                addNode(rootNode, tag, arrangeByType);
            } else {
                DefaultMutableTreeNode parentNode =
                    findParentNodeForTag(tag, rootNode);
                addNode(parentNode, tag, arrangeByType);
            }
        }
        return new DefaultTreeModel(rootNode);
    }

    // Doesn't modify passed-in list.
    private static List sort(List list)
    {
        List methodsAndFields = new ArrayList();
        List allTags  = new ArrayList();
        for (int i = 0; i < list.size(); i++) {
            JavaTag t = (JavaTag) list.get(i);
            switch (t.getType()) {
                case TAG_METHOD:
                case TAG_FIELD:
                    methodsAndFields.add(t);
                    break;
                default:
                    allTags.add(t);
                    break;
            }
        }
        Collections.sort(methodsAndFields, new MethodComparator());
        allTags.addAll(methodsAndFields);
        return allTags;
    }

    private static class MethodComparator implements Comparator
    {
        MethodComparator() {}

        public int compare(Object o1, Object o2)
        {
            String s1 = o1.toString();
            String s2 = o2.toString();
            return s1.compareTo(s2);
        }
    }

    private static DefaultMutableTreeNode findParentNodeForTag(JavaTag tag,
        DefaultMutableTreeNode rootNode)
    {
        JavaClass parent = tag.getParent();
        if (parent == null)
            return rootNode;
        final String parentName = parent.getName();
        Enumeration nodes = rootNode.breadthFirstEnumeration();
        while (nodes.hasMoreElements()) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) nodes.nextElement();
            Object obj = node.getUserObject();
            if (obj instanceof JavaTag) {
                JavaTag t = (JavaTag) obj;
                String name = t.getName();
                switch (t.getType()) {
                    case TAG_CLASS:
                        if (name.startsWith("class "))
                            name = name.substring(6);
                        if (name.equals(parentName))
                            return node;
                        break;
                    case TAG_INTERFACE:
                        if (name.startsWith("interface "))
                            name = name.substring(10);
                        if (name.equals(parentName))
                            return node;
                        break;
                    default:
                        break;
                }
            }
        }
        return rootNode;
    }

    private static void addNode(DefaultMutableTreeNode parentNode, JavaTag tag,
        boolean arrangeByType)
    {
        if (parentNode instanceof ClassNode) {
            ((ClassNode)parentNode).addTag(tag);
        } else {
            final int type = tag.getType();
            if (type == TAG_CLASS || type == TAG_INTERFACE)
                parentNode.add(new ClassNode(tag, arrangeByType));
        }
    }

    public void updatePosition()
    {
        TreeModel model = getModel();
        if (model == null)
            return;
        DefaultMutableTreeNode root = (DefaultMutableTreeNode) model.getRoot();
        if (root == null)
            return;
        if (tags != null) {
            final Position dot = editor.getDotCopy();
            JavaTag tag = findTag(dot);
            if (tag != null) {
                DefaultMutableTreeNode node = findNode(root, tag);
                if (node != null) {
                    DefaultMutableTreeNode selectedNode = null;
                    TreePath oldPath = getSelectionPath();
                    if (oldPath != null) {
                        selectedNode =
                            (DefaultMutableTreeNode) oldPath.getLastPathComponent();
                    }
                    if (node != selectedNode)
                        scrollNodeToCenter(node);
                    return;
                }
            }
        }
        // Otherwise...
        setSelectionRow(0);
        scrollRowToVisible(0);
        if (arrangeByType)
            expandMethods();
    }

    private JavaTag findTag(Position dot)
    {
        if (dot == null)
            return null;
        final Line dotLine = dot.getLine();
        JavaTag tag = null;
        Line lastTagLine = null;
        final int size = tags.size();
        for (int i = 0; i < size; i++) {
            final JavaTag t = (JavaTag) tags.get(i);
            if (t.getPosition().isAfter(dot)) {
                if (t.getLine() == dotLine && t.getLine() != lastTagLine)
                    tag = t;
                break;
            } else {
                tag = t;
                lastTagLine = t.getLine();
            }
        }
        return tag;
    }

    private DefaultMutableTreeNode findNode(DefaultMutableTreeNode root,
        JavaTag tag)
    {
        Enumeration nodes = root.depthFirstEnumeration();
        while (nodes.hasMoreElements()) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) nodes.nextElement();
            if (node.getUserObject() instanceof JavaTag) {
                JavaTag t = (JavaTag) node.getUserObject();
                if (t == tag)
                    return node;
            }
        }
        return null;
    }

    private void expandMethods()
    {
        for (int i = 0; i < getRowCount(); i++) {
            TreePath path = getPathForRow(i);
            if (path != null) {
                DefaultMutableTreeNode node =
                    (DefaultMutableTreeNode) path.getLastPathComponent();
                Object obj = node.getUserObject();
                if (obj instanceof String && obj.equals(CAPTION_METHODS)) {
                    expandRow(i);
                    break;
                }
            }
        }
    }

    public final String getLabelText()
    {
        File file = editor.getBuffer().getFile();
        return file != null ? file.getName() : null;
    }

    public String getToolTipText(MouseEvent e)
    {
        JavaTag t = getJavaTagAtPoint(e.getPoint());
        return t != null ? t.getToolTipText() : null;
    }

    private JavaTag getJavaTagAtPoint(Point point)
    {
        TreePath treePath = getPathForLocation(point.x, point.y);
        if (treePath != null) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) treePath.getLastPathComponent();
            Object obj = node.getUserObject();
            if (obj instanceof JavaTag)
                return (JavaTag) obj;
        }
        return null;
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
                    Object obj = node.getUserObject();
                    if (obj instanceof JavaTag)
                        ((JavaTag)obj).gotoTag(editor);
                }
                editor.setFocusToDisplay();
                if (modifiers == KeyEvent.ALT_MASK)
                    editor.toggleSidebar();
                return;
            }
            case KeyEvent.VK_TAB:
                e.consume();
                if (modifiers == 0) {
                    final Sidebar sidebar = editor.getSidebar();
                    if (sidebar.getBufferList() != null) {
                        updatePosition();
                        editor.setFocus(sidebar.getBufferList());
                    }
                }
                return;
            case KeyEvent.VK_ESCAPE:
                e.consume();
                editor.getSidebar().setBuffer();
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

    protected void processMouseEvent(MouseEvent e)
    {
        if (e.isPopupTrigger()) {
            JavaTreePopupMenu popup = new JavaTreePopupMenu(this);
            popup.show(this, e.getX(), e.getY());
        } else
            super.processMouseEvent(e);
    }

    public void mousePressed(MouseEvent e) {}

    public void mouseReleased(MouseEvent e) {}

    public void mouseClicked(MouseEvent e)
    {
        LocationBar.cancelInput();
        editor.ensureActive();
        final int modifiers = e.getModifiers();
        if (modifiers != InputEvent.BUTTON1_MASK && modifiers != InputEvent.BUTTON2_MASK) {
            e.consume();
            editor.setFocusToDisplay();
            return;
        }
        JavaTag t = getJavaTagAtPoint(e.getPoint());
        if (t != null)
            t.gotoTag(editor);
        editor.setFocusToDisplay();
    }

    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e)
    {
        frame.getCurrentEditor().setFocusToDisplay();
    }

    private static class ClassNode extends DefaultMutableTreeNode
    {
        final String className;
        final boolean arrangeByType;

        DefaultMutableTreeNode fields;
        DefaultMutableTreeNode constructors;
        DefaultMutableTreeNode methods;
        DefaultMutableTreeNode nestedClasses;
        int index;

        ClassNode(JavaTag tag, boolean arrangeByType)
        {
            super(tag);
            String s = tag.getName();
            if (s.startsWith("class "))
                className = s.substring(6);
            else
                className = s;
            this.arrangeByType = arrangeByType;
            if (arrangeByType) {
                fields = new DefaultMutableTreeNode(CAPTION_FIELDS);
                add(fields);
                constructors = new DefaultMutableTreeNode(CAPTION_CONSTRUCTORS);
                add(constructors);
                methods = new DefaultMutableTreeNode(CAPTION_METHODS);
                add(methods);
            } else
                fields = constructors = methods = nestedClasses = this;
        }

        void addTag(JavaTag tag)
        {
            switch (tag.getType()) {
                case TAG_CLASS:
                case TAG_INTERFACE:
                    if (nestedClasses == null) {
                        nestedClasses =
                            new DefaultMutableTreeNode(CAPTION_NESTED_CLASSES);
                        add(nestedClasses);
                    }
                    nestedClasses.add(new ClassNode(tag, arrangeByType));
                    break;
                case TAG_EXTENDS:
                    insert(new DefaultMutableTreeNode(tag), 0);
                    ++index;
                    break;
                case TAG_IMPLEMENTS:
                    insert(new DefaultMutableTreeNode(tag), index++);
                    break;
                case TAG_FIELD:
                    addField(tag);
                    break;
                default:
                    if (tag.getMethodName().equals(className))
                        addConstructor(tag);
                    else
                        addMethod(tag);
                    break;
            }
        }

        void addField(JavaTag tag)
        {
            fields.add(new DefaultMutableTreeNode(tag));
        }

        void addConstructor(JavaTag tag)
        {
            constructors.add(new DefaultMutableTreeNode(tag));
        }

        void addMethod(JavaTag tag)
        {
            methods.add(new DefaultMutableTreeNode(tag));
        }
    }

    private static class TreeCellRenderer extends DefaultTreeCellRenderer
    {
        private static Color noFocusSelectionBackground = new Color(208, 208, 208);
        private static Icon classIcon = Utilities.getIconFromFile("class.png");
        private static Icon fieldIcon = Utilities.getIconFromFile("field.png");
        private static Icon constructorIcon = Utilities.getIconFromFile("method.png");
        private static Icon methodIcon = Utilities.getIconFromFile("method.png");

        private Color oldBackgroundSelectionColor;

        public TreeCellRenderer()
        {
            super();
            oldBackgroundSelectionColor = getBackgroundSelectionColor();
        }

        public Component getTreeCellRendererComponent(JTree tree, Object value,
            boolean selected, boolean expanded, boolean leaf, int row,
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
            if (value instanceof DefaultMutableTreeNode) {
                Object obj = ((DefaultMutableTreeNode)value).getUserObject();
                if (obj instanceof JavaTag) {
                    JavaTag t = (JavaTag) obj;
                    setIcon(t.getIcon());
                    setText(t.getSidebarText());
                } else if (obj instanceof String) {
                    if (obj.equals(CAPTION_FIELDS))
                        setIcon(fieldIcon);
                    else if (obj.equals(CAPTION_CONSTRUCTORS))
                        setIcon(constructorIcon);
                    else if (obj.equals(CAPTION_METHODS))
                        setIcon(methodIcon);
                    else if (obj.equals(CAPTION_NESTED_CLASSES))
                        setIcon(classIcon);
                }
            }
            return this;
        }

        public void paintComponent(Graphics g)
        {
            Display.setRenderingHints(g);
            super.paintComponent(g);
        }
    }
}
