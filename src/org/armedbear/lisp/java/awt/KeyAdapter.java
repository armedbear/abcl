/*
 * KeyAdapter.java
 *
 * Copyright (C) 2003 Peter Graves
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

package org.armedbear.lisp.java.awt;

import org.armedbear.lisp.JHandler;
import java.awt.Component;
import java.awt.event.KeyEvent;

public class KeyAdapter extends java.awt.event.KeyAdapter {

    public static synchronized void addTo(Component component) {
        component.addKeyListener(new KeyAdapter());
    }

    private void call(String s, KeyEvent keyevent) {
        int ai[] = {
            keyevent.getModifiers(), 
	    keyevent.isActionKey() ? 1 : 0, 
	    keyevent.getKeyCode()
        };
        JHandler.callLisp(s, keyevent.getComponent(), keyevent.paramString(), ai);
    }

    public void keyPressed(KeyEvent keyevent) {
        call("KEYPRESSED", keyevent);
    }

    public void keyReleased(KeyEvent keyevent) {
        call("KEYRELEASED", keyevent);
    }

    public void keyTyped(KeyEvent keyevent) {
        call("KEYTYPED", keyevent);
    }
}
