/*
 * JavaTreePopupMenu.java
 *
 * Copyright (C) 2002 Peter Graves
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

import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import javax.swing.JCheckBoxMenuItem;
import javax.swing.JPopupMenu;

public final class JavaTreePopupMenu extends JPopupMenu implements ActionListener
{
    private static final String ARRANGE_BY_TYPE = "Arrange by type";
    private static final String SORT            = "Sort alphabetically";

    private final JavaTree tree;

    public JavaTreePopupMenu(JavaTree tree)
    {
        super();
        this.tree = tree;
        JCheckBoxMenuItem item = new JCheckBoxMenuItem(ARRANGE_BY_TYPE);
        item.setSelected(JavaTree.getArrangeByType());
        item.addActionListener(this);
        add(item);
        item = new JCheckBoxMenuItem(SORT);
        item.setSelected(JavaTree.getSort());
        item.addActionListener(this);
        add(item);
    }

    public void actionPerformed(ActionEvent e)
    {
        Object object = e.getSource();
        if (object instanceof JCheckBoxMenuItem) {
            JCheckBoxMenuItem item = (JCheckBoxMenuItem) object;
            boolean b = item.isSelected();
            String command = e.getActionCommand();
            if (command.equals(ARRANGE_BY_TYPE)) {
                if (b != JavaTree.getArrangeByType()) {
                    JavaTree.setArrangeByType(b);
                    tree.refresh(true);
                }
            } else if (command.equals(SORT)) {
                if (b != JavaTree.getSort()) {
                    JavaTree.setSort(b);
                    tree.refresh(true);
                }
            }
        }
    }
}
