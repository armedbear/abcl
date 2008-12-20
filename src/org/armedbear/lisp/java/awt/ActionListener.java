/*
 * ActionListener.java
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
import java.awt.event.ActionEvent;
import java.awt.Button;
import java.awt.List;
import java.awt.MenuItem;
import java.awt.TextField;
import javax.swing.AbstractButton;
import javax.swing.JTextField;

public class ActionListener implements java.awt.event.ActionListener 
{
    public void actionPerformed(ActionEvent actionevent) {
        String as[] = { actionevent.paramString(), actionevent.getActionCommand() };
        int ai[] = { actionevent.getModifiers() };
	long al[] = { actionevent.getWhen() }; // not yet used
        JHandler.callLisp("ACTIONPERFORMED", handle, as, ai);
    }

    //AWT

    public static synchronized void addTo(Button button) {
        ActionListener actionlistener = new ActionListener();
        actionlistener.handle = button;
        button.addActionListener(actionlistener);
    }

    public static synchronized void addTo(List list) {
        ActionListener actionlistener = new ActionListener();
        actionlistener.handle = list;
        list.addActionListener(actionlistener);
    }

    public static synchronized void addTo(MenuItem menuitem) {
        ActionListener actionlistener = new ActionListener();
        actionlistener.handle = menuitem;
        menuitem.addActionListener(actionlistener);
    }

    public static synchronized void addTo(TextField textfield) {
        ActionListener actionlistener = new ActionListener();
        actionlistener.handle = textfield;
        textfield.addActionListener(actionlistener);
    }

    //Swing
  
    //takes care of JButton, JMenuItem, JToggleButton etc.
    public static synchronized void addTo(AbstractButton ab) {
        ActionListener actionlistener = new ActionListener();
        actionlistener.handle = ab;
        ab.addActionListener(actionlistener);
    }

    public static synchronized void addTo(JTextField textfield) {
        ActionListener actionlistener = new ActionListener();
        actionlistener.handle = textfield;
        textfield.addActionListener(actionlistener);
    }

    private Object handle;
}
