/*
 * TextFieldHandler.java
 *
 * Copyright (C) 2002 Peter Graves
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
import java.awt.event.KeyListener;
import java.util.List;

public interface TextFieldHandler extends KeyListener
{
    public void enter();
    public void escape();
    public boolean wantTab();
    public void tab();
    public void shiftTab();
    public List getCompletions(String prefix);
    public void resetCompletions();
    public Expansion getExpansion(String prefix);
    public void keyPressed(KeyEvent e);
    public void keyReleased(KeyEvent e);
    public void keyTyped(KeyEvent e);
}
