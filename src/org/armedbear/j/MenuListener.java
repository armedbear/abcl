/*
 * MenuListener.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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

import javax.swing.event.MenuEvent;

public final class MenuListener implements javax.swing.event.MenuListener
{
    public void menuCanceled(MenuEvent e)
    {
//         Log.debug("menuCanceled " + e.toString());
    }

    public void menuDeselected(MenuEvent e)
    {
//         Log.debug("menuDeselected " + e.toString());
        if (e.getSource() instanceof org.armedbear.j.Menu) {
//             Log.debug("menuSelected calling removeAll...");
//             final Editor editor = Editor.currentEditor();
//             editor.getMode().populateMenu(editor, (Menu) e.getSource());
            ((Menu)e.getSource()).removeAll();
        }
        Editor.isMenuSelected = false;
    }

    public void menuSelected(MenuEvent e)
    {
//         Log.debug("menuSelected " + e.toString());
        if (e.getSource() instanceof org.armedbear.j.Menu) {
//             Log.debug("menuSelected calling populateMenu...");
            final Editor editor = Editor.currentEditor();
            editor.getMode().populateMenu(editor, (Menu) e.getSource());
        }
        Editor.isMenuSelected = true;
    }
}
