/*
 * SystemSelection.java
 *
 * Copyright (C) 2002-2003 Peter Graves
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

import java.awt.AWTEvent;
import java.awt.Toolkit;
import java.awt.datatransfer.Clipboard;
import java.awt.datatransfer.ClipboardOwner;
import java.awt.datatransfer.DataFlavor;
import java.awt.datatransfer.StringSelection;
import java.awt.datatransfer.Transferable;
import java.awt.event.MouseEvent;
import javax.swing.undo.CompoundEdit;

public final class SystemSelection implements ClipboardOwner, Constants
{
    private static SystemSelection systemSelection =
        getSystemSelection();

    private final Clipboard clipboard;
    private String primarySelection;

    private SystemSelection(Clipboard clipboard)
    {
        this.clipboard = clipboard;
    }

    private static SystemSelection getSystemSelection()
    {
        try {
            Clipboard clipboard =
                Toolkit.getDefaultToolkit().getSystemSelection();
            return new SystemSelection(clipboard);
        }
        catch (Exception e) {
            return null;
        }
    }

    public void lostOwnership(Clipboard clipboard, Transferable contents)
    {
        primarySelection = null;
    }

    public void update(Editor editor)
    {
        try {
            if (clipboard != null) {
                StringSelection ss = null;
                if (editor.getMark() != null && !editor.isColumnSelection()) {
                    primarySelection = new Region(editor).toString();
                    ss = new StringSelection(primarySelection);
                } else if (primarySelection != null) {
                    // We own the primary selection.
                    ss = new StringSelection("");
                }
                if (ss != null)
                    clipboard.setContents(ss, this);
            }
        }
        catch (OutOfMemoryError e) {
            Log.error("SystemSelection.update() OutOfMemoryError");
        }
    }

    public String getPrimarySelection()
    {
        if (primarySelection != null) {
            // We own the primary selection.
            return primarySelection;
        }
        Transferable t = clipboard.getContents(this);
        if (t != null) {
            try {
                return (String) t.getTransferData(DataFlavor.stringFlavor);
            }
            catch (Exception e) {}
        }
        return null;
    }

    public static void updateSystemSelection(Editor editor)
    {
        if (systemSelection != null)
            systemSelection.update(editor);
    }

    public static void pastePrimarySelection()
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        if (systemSelection == null) {
            Log.debug("pastePrimarySelection systemSelection is null");
            return;
        }
        String s = systemSelection.getPrimarySelection();
        if (s == null || s.length() == 0) {
            Log.debug("pastePrimarySelection no selection");
            return;
        }
        KillRing killRing = Editor.getKillRing();
        killRing.appendNew(s);
        // We MUST call killRing.pop() here so that killRing.indexOfNextPop
        // and killRing.lastPaste are set correctly.
        killRing.pop();
        AWTEvent e = editor.getDispatcher().getLastEvent();
        if (e instanceof MouseEvent) {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            editor.mouseMoveDotToPoint((MouseEvent) e);
            editor.paste(s);
            editor.endCompoundEdit(compoundEdit);
        } else
            editor.paste(s);
        editor.setCurrentCommand(COMMAND_PASTE);
    }
}
