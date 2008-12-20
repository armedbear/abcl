/*
 * LabelUI.java
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
import javax.swing.JComponent;
import javax.swing.JLabel;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.metal.MetalLabelUI;

public final class LabelUI extends MetalLabelUI
{
    private static final LabelUI labelUI = new LabelUI();

    public static ComponentUI createUI(JComponent c)
    {
        return labelUI;
    }

    protected void paintEnabledText(JLabel l, Graphics g, String s,
                                    int textX, int textY)
    {
        Display.setRenderingHints(g);
        super.paintEnabledText(l, g, s, textX, textY);
    }


    protected void paintDisabledText(JLabel l, Graphics g, String s,
                                     int textX, int textY)
    {
        Display.setRenderingHints(g);
        super.paintDisabledText(l, g, s, textX, textY);
    }
}
