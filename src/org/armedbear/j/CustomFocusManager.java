/*
 * CustomFocusManager.java
 *
 * Copyright (C) 1999-2003 Peter Graves
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

import java.awt.Component;
import java.awt.event.KeyEvent;
import javax.swing.DefaultFocusManager;
import javax.swing.JDialog;

public final class CustomFocusManager extends DefaultFocusManager
{
    public void processKeyEvent(Component focusedComponent, KeyEvent e)
    {
        if (e.getID() == KeyEvent.KEY_PRESSED) {
            if (isComponentHookable(focusedComponent)) {
                KeyMapping km;
                int keyCode = e.getKeyCode();
                if (keyCode != 0)
                    km = new KeyMapping(keyCode, e.getModifiers(), null);
                else
                    km = new KeyMapping(e.getKeyChar(), null);
                String keyText = km.toString();
                // Escape the escape character!
                if (keyText.equals("\\"))
                    keyText = "\\\\";
                Editor.invokeHook("key-pressed-hook",
                                  "\"" + keyText + "\"");
            }
        }
        super.processKeyEvent(focusedComponent, e);
    }

    private static final boolean isComponentHookable(Component c)
    {
        if (c instanceof Display)
            return false;
        if (c == null)
            return false;
        if (c instanceof HistoryTextField) {
            HistoryTextField textField = (HistoryTextField) c;
            if (textField.getHandler() instanceof IncrementalFindTextFieldHandler)
                return false;
        }
        if (!Editor.preferences().getBooleanProperty(Property.ENABLE_KEY_PRESSED_HOOK))
            return false;
        while (true) {
            if (c instanceof JDialog)
                return false;
            c = c.getParent();
            if (c == null)
                return true;
        }
    }
}
