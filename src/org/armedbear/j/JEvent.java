/*
 * JEvent.java
 *
 * Copyright (C) 2005 Peter Graves
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

import java.awt.event.KeyEvent;

// A keyboard or mouse event, from j's point of view.
public final class JEvent
{
    private  static final int ID_FIRST    = 0;

    public static final int KEY_PRESSED   = 0;
    public static final int KEY_TYPED     = 1;
    public static final int KEY_RELEASED  = 2;
    public static final int MOUSE_PRESSED = 3;

    private  static final int ID_LAST     = 3;

    private final int id;
    private final int keyCode;
    private final char keyChar;
    private final int modifiers;

    JEvent(int id, int keyCode, char keyChar, int modifiers)
    {
        if (id < ID_FIRST || id > ID_LAST) {
            Log.debug("bad JEvent id " + id);
            Debug.assertTrue(false);
        }
        this.id = id;
        this.keyCode = keyCode;
        this.keyChar = keyChar;
        this.modifiers = modifiers;
    }

    JEvent(KeyEvent e)
    {
        this(translateID(e), e.getKeyCode(), e.getKeyChar(), e.getModifiers());
    }

    private static int translateID(KeyEvent e)
    {
        int id = -1;
        switch (e.getID()) {
            case KeyEvent.KEY_PRESSED:
                id = KEY_PRESSED;
                break;
            case KeyEvent.KEY_TYPED:
                id = KEY_TYPED;
                break;
            case KeyEvent.KEY_RELEASED:
                id = KEY_RELEASED;
                break;
            default:
                Debug.assertTrue(false);
                break;
        }
        return id;
    }

    public int getID()
    {
        return id;
    }

    public int getKeyCode()
    {
        return keyCode;
    }

    public char getKeyChar()
    {
        return keyChar;
    }

    public int getModifiers()
    {
        return modifiers;
    }

    public String getKeyText()
    {
        return Utilities.getKeyText(keyChar, keyCode, modifiers);
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer();
        switch (id) {
            case KEY_PRESSED:
                sb.append("KEY_PRESSED ");
                break;
            case KEY_TYPED:
                sb.append("KEY_TYPED ");
                break;
            case KEY_RELEASED:
                sb.append("KEY_RELEASED ");
                break;
        }
        sb.append("0x");
        sb.append(Integer.toHexString(keyCode));
        sb.append(" 0x");
        sb.append(Integer.toHexString(modifiers));
        sb.append(" 0x");
        sb.append(Integer.toHexString((int)keyChar));
        sb.append(' ');
        sb.append(String.valueOf(keyChar));
        sb.append(" \"");
        sb.append(Utilities.getKeyText(keyChar, keyCode, modifiers));
        sb.append('"');
        return sb.toString();
    }
}
