/*
 * CloseBufferConfirmationDialog.java
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

public final class CloseBufferConfirmationDialog extends ConfirmDialog
{
    private final Editor editor;
    private final Buffer buffer;

    private boolean confirmed;

    public static boolean confirmClose(Editor editor, Buffer buffer)
    {
        CloseBufferConfirmationDialog d =
            new CloseBufferConfirmationDialog(editor, buffer);
        d.show();
        return d.confirmed();
    }

    private CloseBufferConfirmationDialog(Editor editor, Buffer buffer)
    {
        super(editor);
        this.editor = editor;
        this.buffer = buffer;
        // Show cancel button.
        cancel = true;
        FastStringBuffer sb = new FastStringBuffer(buffer.getFile().getName());
        sb.append(" is modified. Do you want to save your changes before closing the buffer?");
        initialize(Utilities.wrap(sb.toString(), 65, 8), "Close Buffer");
        editor.setDefaultCursor();
        centerDialog();
    }

    private boolean confirmed()
    {
        return confirmed;
    }

    // Save the changes.
    protected void yes()
    {
        setVisible(false);
        editor.save(buffer);
        if (!buffer.isModified()) {
            confirmed = true;
            dispose();
            return;
        }
        // Save failed.
        setVisible(true);
    }

    // Don't save the changes.
    protected void no()
    {
        confirmed = true;
        dispose();
    }

    protected void cancel()
    {
        cancelled = true;
        dispose();
    }
}
