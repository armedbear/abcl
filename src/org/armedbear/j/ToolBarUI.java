/*
 * ToolBarUI.java
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

import javax.swing.JComponent;
import javax.swing.plaf.ComponentUI;
import javax.swing.plaf.metal.MetalToolBarUI;

public final class ToolBarUI extends MetalToolBarUI
{
    public static ComponentUI createUI(JComponent c)
    {
        if (ToolBar.isRolloverEnabled())
            c.putClientProperty("JToolBar.isRollover", Boolean.TRUE);
        return new org.armedbear.j.ToolBarUI();
    }

    // Don't install default keyboard actions!
    protected void installKeyboardActions()
    {
    }

    protected void uninstallKeyboardActions()
    {
    }
}
