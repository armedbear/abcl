/*
 * WindowAdapter.java
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
import java.awt.Window;
import java.awt.event.WindowEvent;

public class WindowAdapter extends java.awt.event.WindowAdapter 
{
    private void call(String s, WindowEvent windowevent) {
        JHandler.callLisp(s, windowevent.getWindow());
    }

    public static synchronized void addTo(Window window) {
        window.addWindowListener(new WindowAdapter());
    }

    public void windowOpened(WindowEvent windowevent) {
        call("WINDOWOPENED", windowevent);
    }

    public void windowClosed(WindowEvent windowevent) {
        call("WINDOWCLOSED", windowevent);
    }

    public void windowClosing(WindowEvent windowevent) {
        call("WINDOWCLOSING", windowevent);
    }

    public void windowActivated(WindowEvent windowevent) {
        call("WINDOWACTIVATED", windowevent);
    }

    public void windowDeactivated(WindowEvent windowevent) {
        call("WINDOWDEACTIVATED", windowevent);
    }

    public void windowIconified(WindowEvent windowevent) {
        call("WINDOWICONIFIED", windowevent);
    }

    public void windowDeiconified(WindowEvent windowevent) {
        call("WINDOWDEICONIFIED", windowevent);
    }

    public void windowGainedFocus(WindowEvent windowevent) {
        call("WINDOWGAINEDFOCUS", windowevent);
    }

    public void windowLostFocus(WindowEvent windowevent) {
        call("WINDOWLOSTFOCUS", windowevent);
    }
}
