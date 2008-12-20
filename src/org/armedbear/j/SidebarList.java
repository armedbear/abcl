/*
 * SidebarList.java
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
import javax.swing.JLabel;
import javax.swing.JList;
import javax.swing.ListCellRenderer;
import javax.swing.UIManager;
import javax.swing.border.Border;
import javax.swing.border.CompoundBorder;
import javax.swing.border.EmptyBorder;

public abstract class SidebarList extends JList implements NavigationComponent
{
    protected Sidebar sidebar;

    public SidebarList(Sidebar sidebar)
    {
        this.sidebar = sidebar;
        setCellRenderer(new SidebarListCellRenderer(sidebar));
        setToolTipText("");
        int h = Editor.preferences().getIntegerProperty(Property.JLIST_FIXED_CELL_HEIGHT);
        if (h > 0)
            setFixedCellHeight(h);
        setFocusTraversalKeysEnabled(false);
    }

    public void refresh()
    {
    }

    public void updatePosition()
    {
    }

    protected void centerIndex(int index)
    {
        int first = getFirstVisibleIndex();
        int last = getLastVisibleIndex();
        if (first == -1 || last == -1) {
            ensureIndexIsVisible(index);
            return;
        }
        if (first == 0 && last == getModel().getSize()-1)
            return;
        if (index > first + 2 && index < last-2)
            return;
        int span  = last - first;
        first = index - span / 2;
        if (first < 0)
            first = 0;
        ensureIndexIsVisible(first);
        if (getFirstVisibleIndex() == first)
            return;
        last = first + span;
        if (last > getModel().getSize()-1)
            last = getModel().getSize()-1;
        ensureIndexIsVisible(last);
    }

    private static final class SidebarListCellRenderer extends JLabel
        implements ListCellRenderer
    {
        private Sidebar sidebar;

        private static Border noFocusBorder;

        private static Color noFocusSelectionBackground = new Color(208, 208, 208);

        public SidebarListCellRenderer(Sidebar sidebar)
        {
            super();
            this.sidebar = sidebar;
            noFocusBorder = new EmptyBorder(1, 1, 1, 1);
            setOpaque(true);
        }

        public Component getListCellRendererComponent(
            JList list,
            Object value,
            int index,
            boolean isSelected,
            boolean cellHasFocus)
        {
            Frame frame = sidebar.getFrame();
            if (isSelected) {
                if (frame.isActive() && frame.getFocusedComponent() == list)
                    setBackground(list.getSelectionBackground());
                else
                    setBackground(noFocusSelectionBackground);
                setForeground(list.getSelectionForeground());
            } else {
                setBackground(list.getBackground());
                setForeground(list.getForeground());
            }
            Border innerBorder = null;
            if (value instanceof Buffer) {
                setText(value.toString());
                Buffer buffer = (Buffer) value;
                setIcon(buffer.getIcon());
                if (buffer.isSecondary())
                    innerBorder = new EmptyBorder(0, 10, 0, 0);
            } else if (value instanceof LocalTag) {
                LocalTag tag = (LocalTag) value;
                setText(tag.getSidebarText());
                setIcon(tag.getIcon());
            }
            setEnabled(list.isEnabled());
            setFont(list.getFont());
            final Border outerBorder;
            if (cellHasFocus)
                outerBorder = UIManager.getBorder("List.focusCellHighlightBorder");
            else
                outerBorder = noFocusBorder;
            setBorder(new CompoundBorder(outerBorder, innerBorder));
            return this;
        }

        public void paintComponent(java.awt.Graphics g)
        {
            Display.setRenderingHints(g);
            super.paintComponent(g);
        }
    }
}
