/*
 * MouseMotionAdapter.java
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
import java.awt.event.MouseEvent;

public class MouseMotionAdapter extends java.awt.event.MouseMotionAdapter 
{
    public static synchronized void addTo(Component component) {
        component.addMouseMotionListener(new MouseMotionAdapter());
    }

    private void call(String s, MouseEvent mouseevent) {
        int ai[] = {
            mouseevent.getModifiers(), 
	    mouseevent.isPopupTrigger() ? 1 : 0, 
	    mouseevent.getClickCount(), 
	    mouseevent.getX(), 
	    mouseevent.getY()
        };
        JHandler.callLisp(s, mouseevent.getComponent(), mouseevent.paramString(), ai);
    }

    public void mouseDragged(MouseEvent mouseevent) {
        call("MOUSEDRAGGED", mouseevent);
    }

    public void mouseMoved(MouseEvent mouseevent) {
        call("MOUSEMOVED", mouseevent);
    }

    public void mouseWheel(MouseEvent mouseevent) {
        call("MOUSEWHEEL", mouseevent);
    }
}
