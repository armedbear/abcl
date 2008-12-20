/*
 * KeyMapping.java
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

import javax.swing.KeyStroke;

public class KeyMapping implements Constants
{
    private final char keyChar;
    private final int keyCode;
    private final int modifiers;
    private final Object command;

    public KeyMapping(int keyCode, int modifiers, Object command)
    {
        this.keyChar = 0;
        this.keyCode = keyCode;
        this.modifiers = modifiers;
        if (command instanceof String)
            this.command = ((String)command).intern();
        else
            this.command = command;
    }

    public KeyMapping(char keyChar, Object command)
    {
        this.keyChar = keyChar;
        this.keyCode = 0;
        this.modifiers = 0;
        if (command instanceof String)
            this.command = ((String)command).intern();
        else
            this.command = command;
    }

    private KeyMapping(KeyStroke keyStroke, String command)
    {
        char c = keyStroke.getKeyChar();
        keyChar = c == 0xffff ?  0 : c;
        keyCode = keyStroke.getKeyCode();
        // Mask off the bits we don't care about (Java 1.4).
        modifiers = keyStroke.getModifiers() & 0x0f;
        if (command != null)
            this.command = command.intern();
        else
            this.command = null;
    }

    // Returns null if string can't be parsed.
    public static KeyMapping createKeyMapping(String s)
    {
        s = s.trim();
        String parameters = null;
        int index = s.indexOf('(');
        if (index >= 0) {
            parameters = s.substring(index);
            s = s.substring(0, index).trim();
        }
        index = s.lastIndexOf(' ');
        if (index < 0)
            return null;
        String keyText = s.substring(0, index).trim();
        String command = s.substring(index + 1).trim();
        if (parameters != null)
            command += parameters;
        return createKeyMapping(keyText, command);
    }

    private static KeyMapping createKeyMapping(String keyText, String command)
    {
        KeyStroke keyStroke = Utilities.getKeyStroke(keyText);
        if (keyStroke == null)
            return null;
        if (command != null)
            command = command.trim();
        return new KeyMapping(keyStroke, command);
    }

    public final char getKeyChar()
    {
        return keyChar;
    }

    public final int getKeyCode()
    {
        return keyCode;
    }

    public final int getModifiers()
    {
        return modifiers;
    }

    public final Object getCommand()
    {
        return command;
    }

    public String toString()
    {
        FastStringBuffer sb = new FastStringBuffer(64);
        sb.append(Utilities.getKeyText(keyChar, keyCode, modifiers));
        if (command != null) {
            while (sb.length() < 32)
                sb.append(' ');
            sb.append(command);
        }
        return sb.toString();
    }

    public final String getKeyText()
    {
        return Utilities.getKeyText(keyChar, keyCode, modifiers);
    }
}
