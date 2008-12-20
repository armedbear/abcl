/*
 * ErrorRunnable.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

public class ErrorRunnable implements Runnable
{
    private final String defaultMessage;

    protected String message;

    public ErrorRunnable(String defaultMessage)
    {
        this.defaultMessage = defaultMessage;
    }

    public final void setMessage(String s)
    {
        message = s;
    }

    public void run()
    {
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            ed.setDefaultCursor();
            ed.updateLocation();
            ed.status("");
        }
        String text;
        if (message != null && message.length() > 0)
            text = message;
        else if (defaultMessage != null)
            text = defaultMessage;
        else
            text = "Unknown error";
        MessageDialog.showMessageDialog(text, "Error");
    }
}
