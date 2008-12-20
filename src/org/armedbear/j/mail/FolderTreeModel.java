/*
 * FolderTreeModel.java
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

import java.util.Enumeration;
import java.util.List;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.DefaultTreeModel;
import javax.swing.tree.TreeNode;
import org.armedbear.j.Directories;
import org.armedbear.j.Editor;
import org.armedbear.j.File;
import org.armedbear.j.Property;

public final class FolderTreeModel extends DefaultTreeModel
{
    private static FolderTreeModel model;

    private FolderTreeModel(TreeNode root)
    {
        super(root);
    }

    public static synchronized FolderTreeModel getDefaultModel()
    {
        if (model == null) {
            // Root will not be visible.
            DefaultMutableTreeNode root = new DefaultMutableTreeNode();
            File local =
                File.getInstance(Directories.getMailDirectory(), "local");
            if (local.isDirectory()) {
                DefaultMutableTreeNode localFolders =
                    new DefaultMutableTreeNode("Local Folders");
                root.add(localFolders);
                // Add the drafts folder.
                Folder drafts = new Folder("drafts",
                    new LocalMailboxURL(Directories.getDraftsFolder()));
                localFolders.add(new DefaultMutableTreeNode(drafts));
                // Enumerate directories under ~/.j/mail/local.
                String[] list = local.list();
                for (int i = 0; i < list.length; i++) {
                    final String name = list[i];
                    if (name.equals("drafts"))
                        continue;
                    File file = File.getInstance(local, name);
                    if (file.isDirectory()) {
                        File mailboxFile = File.getInstance(file, "mbox");
                        final LocalMailboxURL url =
                            new LocalMailboxURL(mailboxFile);
                        Folder folder = new Folder(file.getName(), url);
                        localFolders.add(new DefaultMutableTreeNode(folder));
                    }
                }
            }
            model = new FolderTreeModel(root);
            String inbox =
                Editor.preferences().getStringProperty(Property.INBOX);
            if (inbox != null) {
                MailboxURL url = MailboxURL.parse(inbox);
                if (url != null)
                    model.addNodeForFolder(url);
            }
        }
        return model;
    }

    public void maybeAddNodeForFolder(MailboxURL url)
    {
        if (findNodeForFolder(url) == null)
            addNodeForFolder(url);
    }

    private DefaultMutableTreeNode findNodeForFolder(MailboxURL url)
    {
        Enumeration nodes =
            ((DefaultMutableTreeNode) root).depthFirstEnumeration();
        while (nodes.hasMoreElements()) {
            DefaultMutableTreeNode node =
                (DefaultMutableTreeNode) nodes.nextElement();
            Object obj = node.getUserObject();
            if (obj instanceof Folder && ((Folder) obj).getUrl().equals(url))
                return node;
        }
        return null;
    }

    private void addNodeForFolder(MailboxURL url)
    {
        if (url instanceof ImapURL) {
            Enumeration nodes = root.children();
            DefaultMutableTreeNode parent = null;
            while (nodes.hasMoreElements()) {
                DefaultMutableTreeNode node =
                    (DefaultMutableTreeNode) nodes.nextElement();
                Object obj = node.getUserObject();
                if (obj instanceof String) {
                    String s = (String) obj;
                    if (s.equals(url.getHost())) {
                        parent = node;
                        break;
                    }
                }
            }
            if (parent == null) {
                parent = new DefaultMutableTreeNode(url.getHost());
                ((DefaultMutableTreeNode) root).add(parent);
            }
            List list = ((ImapURL)url).getFolderPathComponents();
            for (int i = 0; i < list.size()-1; i++) {
                boolean add = true;
                nodes = parent.children();
                while (nodes.hasMoreElements()) {
                    DefaultMutableTreeNode node =
                        (DefaultMutableTreeNode) nodes.nextElement();
                    Object obj = node.getUserObject();
                    if (obj instanceof String) {
                        String s = (String) obj;
                        if (s.equals(list.get(i))) {
                            parent = node;
                            add = false;
                            break;
                        }
                    }
                }
                if (add) {
                    DefaultMutableTreeNode node =
                        new DefaultMutableTreeNode(list.get(i));
                    parent.add(node);
                    parent = node;
                }
            }
            // Last component.
            parent.add(new DefaultMutableTreeNode(new Folder((String)list.get(list.size()-1), url)));
        } else
            ((DefaultMutableTreeNode)root).add(new DefaultMutableTreeNode(new Folder(url.toString(), url)));
        reload();
    }
}
