/*
 * UndoManager.java
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

import javax.swing.undo.CompoundEdit;
import javax.swing.undo.UndoableEdit;

public final class UndoManager extends javax.swing.undo.UndoManager
{
    public synchronized void undo()
    {
        UndoableEdit edit = editToBeUndone();
        super.undo();
        if (edit instanceof UndoBoundary)
            super.undo();
    }

    public synchronized void redo()
    {
        UndoableEdit edit = editToBeRedone();
        super.redo();
        if (edit instanceof UndoBoundary)
            super.redo();
    }

    public void appendUndoFold(Editor editor)
    {
        if (edits.size() > 0) {
            UndoFold undoFold = new UndoFold(editor);
            UndoableEdit lastEdit = (UndoableEdit) edits.remove(edits.size()-1);
            CompoundEdit compoundEdit = new CompoundEdit();
            compoundEdit.addEdit(lastEdit);
            compoundEdit.addEdit(undoFold);
            compoundEdit.end();
            edits.add(compoundEdit);
        }
    }
}
