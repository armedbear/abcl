/*
 * SidebarTree.java
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

import java.awt.Rectangle;
import javax.swing.JTree;
import javax.swing.tree.DefaultMutableTreeNode;
import javax.swing.tree.TreeModel;
import javax.swing.tree.TreePath;

public class SidebarTree extends JTree
{
    public SidebarTree(TreeModel model)
    {
        super(model);
    }

    protected void scrollNodeToCenter(DefaultMutableTreeNode node)
    {
        TreePath treePath = new TreePath(node.getPath());
        TreePath parentPath = treePath.getParentPath();
        if (parentPath != null)
            expandPath(parentPath);
        int row = getRowForPath(treePath);
        scrollRowToCenter(row);
        setSelectionRow(row);
    }

    protected void scrollRowToCenter(int row)
    {
        Rectangle rect = getVisibleRect();
        int top = getClosestRowForLocation(rect.x, rect.y);
        int bottom = top + getVisibleRowCount() - 1;
        int margin = getVisibleRowCount() / 4;
        int first = row - margin;
        if (first < 0)
            first = 0;
        int last = row + margin;
        if (last > getRowCount() - 1)
            last = getRowCount() - 1;
        if (first < top || first > bottom) {
            scrollRowToVisible(first);
            rect = getVisibleRect();
            top = getClosestRowForLocation(rect.x, rect.y);
            bottom = top + getVisibleRowCount() - 1;
        }
        if (last < top || last > bottom)
            scrollRowToVisible(last);
    }
}
