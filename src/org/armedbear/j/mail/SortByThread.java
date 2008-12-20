/*
 * SortByThread.java
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

import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.HashMap;
import java.util.Iterator;
import java.util.List;
import java.util.Map;
import javax.swing.tree.DefaultMutableTreeNode;
import org.armedbear.j.Debug;
import org.armedbear.j.Log;

public final class SortByThread
{
    private final List entries;

    private final Node root = new Node();
    private final Map idMap = new HashMap();

    public SortByThread(List entries)
    {
        this.entries = Collections.unmodifiableList(entries);
    }

    public void run()
    {
        final int count = entries.size();
        // Create all the nodes and populate the ID map.
        ArrayList nodes = new ArrayList(count);
        for (int i = 0; i < count; i++) {
            final MailboxEntry entry = (MailboxEntry) entries.get(i);
            Node node = new Node(entry);
            nodes.add(node);
            String messageId = entry.getMessageId();
            if (messageId != null && messageId.length() > 0)
                idMap.put(messageId, node);
            else
                Debug.bug();
        }

        for (int i = 0; i < count; i++) {
            final MailboxEntry entry = (MailboxEntry) entries.get(i);
            // References.
            String[] references = entry.getReferences();
            if (references != null) {
                for (int j = 0; j < references.length; j++) {
                    String reference = references[j];
                    if (reference == null || reference.length() == 0)
                        continue;
                    Node node = (Node) idMap.get(reference);
                    if (node == null) {
                        node = new Node(reference, entry.getBaseSubject());
                        idMap.put(reference, node);
                        nodes.add(node);
                    }
                }
            }
            // In-Reply-To.
            String inReplyTo = entry.getInReplyTo();
            if (inReplyTo != null && inReplyTo.length() > 0) {
                Node node = (Node) idMap.get(inReplyTo);
                if (node == null) {
                    node = new Node(inReplyTo, entry.getBaseSubject());
                    idMap.put(inReplyTo, node);
                    nodes.add(node);
                }
            }
        }

        Iterator iter = nodes.iterator();
        while (iter.hasNext()) {
            Node node = (Node) iter.next();
            if (node.getParent() == null) {
                Node parent = findParentForNode(node);
                if (parent != null)
                    parent.add(node);
                else
                    root.add(node);
            }
        }

        removeEmptyContainers(root);

        groupMessagesBySubject();

        // Walk the tree and sort the entries by date (or whatever).
        sort(root);
    }

    private void removeEmptyContainers(final Node parent)
    {
        for (int i = 0; i < parent.getChildCount(); i++) {
            Node node = (Node) parent.getChildAt(i);
            if (node.getMailboxEntry() == null && node.getChildCount() == 0) {
                // An empty container with no children.
                parent.remove(i);
                --i;
                continue;
            }
            if (node.getMailboxEntry() == null && node.getChildCount() > 0) {
                Debug.assertTrue(node.getParent() == parent);
                if (parent != root || node.getChildCount() == 1) {
                    for (int j = node.getChildCount(); j-- > 0;) {
                        Node child = (Node) node.getChildAt(j);
                        Debug.assertTrue(child.getParent() == node);
                        node.remove(j);
                        Debug.assertTrue(child.getParent() == null);
                        parent.add(child);
                        Debug.assertTrue(child.getParent() == parent);
                    }
                    Debug.assertTrue(node.getChildCount() == 0);
                    parent.remove(i);
                    --i;
                    continue;
                }
            }
            if (node.getMailboxEntry() != null) {
                removeEmptyContainers(node);
            }
        }
    }

    private void groupMessagesBySubject()
    {
        HashMap subjectMap = createSubjectMap();

        // Iterate through top-level nodes.
        for (int i = root.getChildCount(); i-- > 0; ) {
            final Node node = (Node) root.getChildAt(i);
            MailboxEntry entry = node.getMailboxEntry();
            if (entry == null) {
                if (node.getChildCount() > 0)
                    entry = ((Node)node.getChildAt(0)).getMailboxEntry();
                if (entry == null)
                    continue;
            }
            final String baseSubject = entry.getBaseSubject();
            if (baseSubject == null || baseSubject.length() == 0)
                continue;
            Node oldNode = (Node) subjectMap.get(baseSubject);
            if (oldNode == node)
                continue;

            if (oldNode.isDummy() && node.isDummy()) {
                // Add node's children to old node.
                for (int j = node.getChildCount(); j-- > 0;) {
                    Node child = (Node) node.getChildAt(j);
                    node.remove(j);
                    oldNode.add(child);
                }
                // Remove node.
                root.remove(i);
                continue;
            }

            if (oldNode.isDummy()) {
                Debug.assertTrue(!node.isDummy());
                // Make this node be a child of the other.
                root.remove(i);
                oldNode.add(node);
                continue;
            }

            Debug.assertTrue(!oldNode.isDummy());
            final MailboxEntry oldEntry = oldNode.getMailboxEntry();
            Debug.assertTrue(oldEntry != null);
            if (!node.isDummy()) {
                if (entry.subjectIsReply() && !oldEntry.subjectIsReply()) {
                    // Make this node be a child of the other.
                    root.remove(i);
                    oldNode.add(node);
                    continue;
                }
            }

            // If neither entry is a reply, leave them both at top level.
            if (!entry.subjectIsReply() && !oldEntry.subjectIsReply())
                continue;

            // Make old and new nodes be children of a new dummy node.
            // Create a new container for the old message.
            Node c = new Node(oldEntry);
            for (int j = oldNode.getChildCount(); j-- > 0;) {
                Node child = (Node) oldNode.getChildAt(j);
                oldNode.remove(j);
                c.add(child);
            }
            // Make old container into a dummy by emptying it.
            oldNode.setUserObject(null);
            oldNode.setBaseSubject(baseSubject);
            // Add old and new messages to new dummy node.
            oldNode.add(c);
            oldNode.add(node);
        }

        subjectMap.clear();
        subjectMap = null;
    }

    private HashMap createSubjectMap()
    {
        HashMap subjectMap = new HashMap();
        for (int i = 0, limit = root.getChildCount(); i < limit; i++) {
            final Node node = (Node) root.getChildAt(i);
            MailboxEntry entry = node.getMailboxEntry();
            if (entry == null) {
                // Dummy node.
                if (node.getChildCount() > 0)
                    entry = ((Node)node.getChildAt(0)).getMailboxEntry();
                else {
                    Log.debug("dummy node child count is zero");
                    continue;
                }
            }
            if (entry != null) {
                String baseSubject = entry.getBaseSubject();
                if (baseSubject != null && baseSubject.length() > 0) {
                    Node oldNode = (Node) subjectMap.get(baseSubject);
                    if (oldNode == null) {
                        subjectMap.put(baseSubject, node);
                    } else {
                        MailboxEntry oldEntry = oldNode.getMailboxEntry();
                        if (oldEntry != null && oldEntry.subjectIsReply() &&
                            !entry.subjectIsReply()) {
                            subjectMap.put(baseSubject, node);
                        } else if (node.getMailboxEntry() == null && oldEntry != null) {
                            subjectMap.put(baseSubject, node);
                        }
                    }
                }
            }
        }
        return subjectMap;
    }

    private void sort(Node node)
    {
        // Depth first!
        for (int i = 0, limit = node.getChildCount(); i < limit; i++)
            sort((Node)node.getChildAt(i));
        if (node.getChildCount() > 1)
            node.sortChildren();
    }

    private Node findParentForNode(Node node)
    {
        final MailboxEntry entry = node.getMailboxEntry();
        if (entry == null)
            return null;
        final String inReplyTo = entry.getInReplyTo();
        if (inReplyTo != null) {
            Node parent = (Node) idMap.get(inReplyTo);
            if (parent != null)
                if (!parent.isNodeAncestor(node))
                    return parent;
        }
        final String[] references = entry.getReferences();
        if (references != null) {
            for (int i = references.length; i-- > 0;) {
                Node parent = (Node) idMap.get(references[i]);
                if (parent != null)
                    if (!parent.isNodeAncestor(node))
                        return parent;
            }
        }
        return null;
    }

    private int sequenceNumber;

    public void addEntries(Mailbox mb, MailboxFilter filter)
    {
        sequenceNumber = 1;
        addEntriesForNode(root, mb, filter, 0);
    }

    private void addEntriesForNode(Node node, Mailbox mb, MailboxFilter filter,
        int depth)
    {
        if (node != root) {
            MailboxEntry entry = node.getMailboxEntry();
            if (entry != null) {
                entry.setSequenceNumber(sequenceNumber++);
                if (filter == null || filter.accept(entry))
                    mb.appendLine(entry, depth);
            } else {
                if (node.getChildCount() > 0) {
                    Node child = (Node) node.getChildAt(0);
                    MailboxEntry childEntry = child.getMailboxEntry();
                    if (childEntry != null)
                        childEntry.setOrphan(true);
                }
            }
        }
        for (int i = 0, limit = node.getChildCount(); i < limit; i++)
            addEntriesForNode((Node)node.getChildAt(i), mb, filter, depth + 1);
    }
}

class Node extends DefaultMutableTreeNode
{
    private String messageId;
    private String baseSubject;

    Node()
    {
        super();
    }

    Node(MailboxEntry entry)
    {
        super(entry);
    }

    Node(String messageId, String baseSubject)
    {
        this.messageId = messageId;
        this.baseSubject = baseSubject;
    }

    MailboxEntry getMailboxEntry()
    {
        return (MailboxEntry) getUserObject();
    }

    String getBaseSubject()
    {
        return baseSubject;
    }

    void setBaseSubject(String s)
    {
        baseSubject = s;
    }

    String getMessageId()
    {
        return messageId;
    }

    RFC822Date getDate()
    {
        MailboxEntry entry = getMailboxEntry();
        if (entry != null)
            return entry.getDate();
        if (getChildCount() > 0) {
            Node child = (Node) getChildAt(0);
            entry = child.getMailboxEntry();
            if (entry != null)
                return entry.getDate();
        }
        Log.debug("getDate no date");
        return null;
    }

    boolean isDummy()
    {
        return getUserObject() == null;
    }

    void sortChildren()
    {
        if (children != null)
            sortEntriesByDate(children);
    }

    private static final Comparator comparator = new Comparator() {
        public int compare(Object o1, Object o2)
        {
            return RFC822Date.compare(((Node)o1).getDate(),
                ((Node)o2).getDate());
        }
    };

    private static final void sortEntriesByDate(List list)
    {
        Collections.sort(list, comparator);
    }
}
