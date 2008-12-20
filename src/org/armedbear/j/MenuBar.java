/*
 * MenuBar.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

import java.awt.Graphics;
import javax.swing.JMenuBar;

public final class MenuBar extends JMenuBar
{
    private static MenuListener listener = new MenuListener();

    private final String menuName;

    public static final MenuListener getListener()
    {
        return listener;
    }

    public MenuBar(String menuName)
    {
        this.menuName = menuName;
    }

    public String getMenuName()
    {
        return menuName;
    }

    public void paintComponent(Graphics g)
    {
        Display.setRenderingHints(g);
        super.paintComponent(g);
    }
}
