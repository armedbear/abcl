/*
 * DirectoryTreeModel.java
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

import java.util.Arrays;
import java.util.Comparator;
import java.util.Vector;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;

public class DirectoryTreeModel extends DefaultTreeModel
{
    private static final boolean ignoreCase = Platform.isPlatformWindows();

    private static DirectoryTreeModel localTreeModel;
    private static Vector remoteModels;

    private File rootFile; // Will be null for local tree.

    private DirectoryTreeModel(TreeNode root)
    {
        super(root);
    }

    private DirectoryTreeModel(TreeNode root, File f)
    {
        super(root);
        rootFile = f;
    }

    private File getRootFile()
    {
        return rootFile;
    }

    private static DirectoryTreeModel getLocalDirectoryTreeModel(File file)
    {
        if (localTreeModel == null)
            setLocalRoot(scanRoot(file));
        return localTreeModel;
    }

    public static DirectoryTreeModel getTreeModel(File file)
    {
        if (file.isLocal())
            return getLocalDirectoryTreeModel(file);
        DirectoryTreeModel model;
        File rootFile = file.getRoot();
        if (remoteModels != null) {
            for (int i = 0; i < remoteModels.size(); i++) {
                model = (DirectoryTreeModel) remoteModels.get(i);
                if (rootFile.equals(model.getRootFile()))
                    return model;
            }
        }
        DefaultMutableTreeNode root =
            new DefaultMutableTreeNode(new DirectoryTreeElement(rootFile));
        model = new DirectoryTreeModel(root, rootFile);
        addChildren(rootFile, root);
        if (remoteModels == null)
            remoteModels = new Vector();
        remoteModels.add(model);
        return model;
    }

    private static synchronized void setLocalRoot(DefaultMutableTreeNode root)
    {
        if (localTreeModel != null)
            localTreeModel.setRoot(root);
        else
            localTreeModel = new DirectoryTreeModel(root);
    }

    public void rescan(File file)
    {
        setRoot(scanRoot(file));
    }

    private static DefaultMutableTreeNode scanRoot(File file)
    {
        DefaultMutableTreeNode root = null;
        if (Platform.isPlatformWindows()) {
            File[] roots = File.listRoots();
            if (roots != null) {
                root = new DefaultMutableTreeNode("Local");
                for (int i = 0; i < roots.length; i++) {
                    DefaultMutableTreeNode child =
                        new DefaultMutableTreeNode(new DirectoryTreeElement(roots[i]));
                    root.insert(child, root.getChildCount());
                    if (roots[i].equals(file.getRoot()))
                        addChildren(roots[i], child);
                }
            } else {
                root = new DefaultMutableTreeNode(new DirectoryTreeElement(file.getRoot()));
                addChildren(file.getRoot(), root);
            }
        } else {
            // Unix.
            File f = file.getRoot();
            root = new DefaultMutableTreeNode(new DirectoryTreeElement(file.getRoot()));
            addChildren(f, root);
        }
        return root;
    }

    private static void addChildren(File parent, DefaultMutableTreeNode node)
    {
        File[] list = parent.listFiles();
        if (list == null)
            return;
        Arrays.sort(list,
            ignoreCase ? ciFileNameComparator : csFileNameComparator);
        for (int i = 0; i < list.length; i++) {
            File f = list[i];
            if (f.isDirectory() && !f.isLink()) {
                DefaultMutableTreeNode child =
                    new DefaultMutableTreeNode(new DirectoryTreeElement(f));
                node.insert(child, node.getChildCount());
            }
        }
    }

    public DefaultMutableTreeNode getNode(File file)
    {
        return getNode((DefaultMutableTreeNode) getRoot(), file);
    }

    private static DefaultMutableTreeNode getNode(DefaultMutableTreeNode root, File file)
    {
        if (root == null || file == null)
            return null;
        DefaultMutableTreeNode currentNode = root;
        while (true) {
            DefaultMutableTreeNode node = findMatchingChild(currentNode, file);
            if (node == null)
                return null;
            DirectoryTreeElement treeElement =
                (DirectoryTreeElement) node.getUserObject();
            File f = treeElement.getFile();
            expandNode(node, f);
            if (file.canonicalPath().equals(f.canonicalPath()))
                return node;
            currentNode = node;
        }
    }

    // Find child that is ancestor of file (so to speak).
    private static DefaultMutableTreeNode findMatchingChild(
        DefaultMutableTreeNode parent, File file)
    {
        if (file != null) {
            for (int i = 0; i < parent.getChildCount(); i++) {
                DefaultMutableTreeNode node =
                    (DefaultMutableTreeNode) parent.getChildAt(i);
                DirectoryTreeElement treeElement =
                    (DirectoryTreeElement) node.getUserObject();
                File f = treeElement.getFile();
                if (file.canonicalPath().equals(f.canonicalPath()))
                    return node;
                String prefix = f.canonicalPath();
                if (!prefix.endsWith(f.getSeparator()))
                    prefix += f.getSeparator();
                if (file.canonicalPath().startsWith(prefix))
                    return node;
            }
        }
        return null;
    }

    public static void expandNode(DefaultMutableTreeNode node, File file)
    {
        if (node.getChildCount() == 0) {
            File[] list = file.listFiles();
            if (list == null)
                return;
            Arrays.sort(list,
                ignoreCase ? ciFileNameComparator : csFileNameComparator);
            for (int i = 0; i < list.length; i++) {
                File f = list[i];
                if (f.isLink())
                    continue;
                else if (f.isDirectory()) {
                    DefaultMutableTreeNode child =
                        new DefaultMutableTreeNode(new DirectoryTreeElement(f));
                    node.insert(child, node.getChildCount());
                }
            }
        }
    }

    // Case-sensitive filename comparator (Unix).
    private final static Comparator csFileNameComparator = new Comparator() {
        public int compare(Object o1, Object o2)
        {
            String name1 = ((File)o1).getName();
            String name2 = ((File)o2).getName();
            return name1.compareTo(name2);
        }
    };

    // Case-insensitive filename comparator (Windows).
    private final static Comparator ciFileNameComparator = new Comparator() {
        public int compare(Object o1, Object o2)
        {
            String name1 = ((File)o1).getName();
            String name2 = ((File)o2).getName();
            return name1.compareToIgnoreCase(name2);
        }
    };
}
