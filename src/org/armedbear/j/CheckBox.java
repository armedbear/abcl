/*
 * CheckBox.java
 *
 * Copyright (C) 2003 Peter Graves
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
import javax.swing.JCheckBox;

public final class CheckBox extends JCheckBox
{
    public CheckBox(String text)
    {
        super(text);
    }

    public CheckBox(String text, boolean selected)
    {
        super(text, selected);
    }

    public void paintComponent(Graphics g)
    {
        Display.setRenderingHints(g);
        super.paintComponent(g);
    }
}
