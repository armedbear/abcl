/*
 * MenuItem.java
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

import java.awt.Color;
import java.awt.Dimension;
import java.awt.Font;
import java.awt.FontMetrics;
import java.awt.Graphics;
import java.awt.Insets;
import java.awt.event.KeyEvent;
import javax.swing.JMenuItem;
import javax.swing.MenuElement;
import javax.swing.MenuSelectionManager;
import javax.swing.UIManager;

public final class MenuItem extends JMenuItem
{
    private static final Font acceleratorFont =
        UIManager.getFont("MenuItem.acceleratorFont");
    private static final Color acceleratorForeground =
        UIManager.getColor("MenuItem.acceleratorForeground");
    private static final Color acceleratorSelectionForeground =
        UIManager.getColor("MenuItem.acceleratorSelectionForeground");
    private static final Color disabledForeground =
        UIManager.getColor("MenuItem.disabledForeground");

    private final String acceleratorText;

    public MenuItem(String label, String acceleratorText)
    {
        super(label);
        this.acceleratorText = acceleratorText;
    }

    public Dimension getPreferredSize()
    {
        Dimension d = super.getPreferredSize();
        if (acceleratorText != null)
            d.width += getToolkit().getFontMetrics(acceleratorFont).stringWidth(acceleratorText) + 30;
        return d;
    }

    // We paint our own menu items so the accelerator text will be consistent
    // with our key map format.
    public void paint(Graphics g)
    {
        Display.setRenderingHints(g);
        super.paint(g);
        if (acceleratorText != null) {
            g.setFont(acceleratorFont);
            Color c;
            if (isEnabled())
                c = getModel().isArmed() ? acceleratorSelectionForeground : acceleratorForeground;
            else
                c = disabledForeground;
            g.setColor(c);
            FontMetrics fm = g.getFontMetrics();
            Insets insets = getInsets();
            g.drawString(acceleratorText,
                         getWidth() - (fm.stringWidth(acceleratorText) + insets.right + insets.left),
                         getFont().getSize() + (insets.top - 1));
        }
    }

    private static final boolean consumeKeyEvent =
        Platform.isJava13() || Platform.isJava140();

    public void processKeyEvent(KeyEvent e, MenuElement path[], MenuSelectionManager manager)
    {
        super.processKeyEvent(e, path, manager);
        if (consumeKeyEvent)
            if (Character.toUpperCase(e.getKeyChar()) == getMnemonic())
                e.consume();
    }
}
