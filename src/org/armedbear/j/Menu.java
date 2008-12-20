/*
 * Menu.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import javax.swing.JMenu;

public final class Menu extends JMenu implements Constants
{
    public Menu(String s)
    {
        super(s);
    }

    public Menu(String s, char mnemonic)
    {
        super(s);
        if (Editor.preferences().getBooleanProperty(Property.USE_MENU_MNEMONICS))
            setMnemonic(mnemonic);
        addMenuListener(MenuBar.getListener());
    }

    public void setPopupMenuVisible(boolean b)
    {
        super.setPopupMenuVisible(b);
    }

    public MenuItem add(Editor editor, String label, char mnemonic,
        String command, boolean enabled)
    {
        Object[] values = editor.getKeyMapping(command);
        Debug.assertTrue(values != null);
        Debug.assertTrue(values.length == 2);
        KeyMapping mapping = (KeyMapping) values[0];
        String keyText = mapping != null ? mapping.getKeyText() : "";
        if (keyText.length() == 3) {
            if (keyText.charAt(0) == '\'' && keyText.charAt(2) == '\'')
                keyText = keyText.substring(1, 2).toUpperCase(); // 'a' => A
        }
        MenuItem menuItem = new MenuItem(label, keyText);
        if (mnemonic != '\0')
            menuItem.setMnemonic(mnemonic);
        menuItem.setActionCommand(command);
        menuItem.addActionListener(editor.getDispatcher());
        if (!enabled)
            menuItem.setEnabled(false);
        add(menuItem);
        return menuItem;
    }

    public MenuItem add(Editor editor, String label, char mnemonic,
        String command)
    {
        return add(editor, label, mnemonic, command, true);
    }
}
