/*
 * UndoMove.java
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

import javax.swing.undo.AbstractUndoableEdit;

public class UndoMove extends AbstractUndoableEdit implements Constants
{
    private final State preState;
    private State postState;

    public UndoMove(Editor editor)
    {
        preState = new State(editor);
    }

    public void undo()
    {
        super.undo();
        final Editor editor = Editor.currentEditor();
        postState = new State(editor);
        preState.restoreState(editor);
        editor.setUpdateFlag(REFRAME);
    }

    public void redo()
    {
        super.redo();
        final Editor editor = Editor.currentEditor();
        postState.restoreState(editor);
        editor.setUpdateFlag(REFRAME);
    }

    private static class State
    {
        final int dotLineNumber;
        final int dotOffset;
        final int markLineNumber;
        final int markOffset;
        final int absCaretCol;
        final boolean isColumnSelection;

        State(Editor editor)
        {
            dotLineNumber = editor.getDotLine().lineNumber();;
            dotOffset = editor.getDotOffset();
            Position mark = editor.getMark();
            if (mark != null) {
                markLineNumber = mark.lineNumber();
                markOffset = mark.getOffset();
            } else {
                markLineNumber = -1;
                markOffset = -1;
            }
            absCaretCol = editor.getAbsoluteCaretCol();
            isColumnSelection = editor.isColumnSelection();
        }

        void restoreState(Editor editor)
        {
            boolean wasMarked = editor.getMark() != null;
            editor.updateDotLine();
            editor.setDot(dotLineNumber, dotOffset);
            if (markLineNumber >= 0) {
                editor.setMark(markLineNumber, markOffset);
                editor.setColumnSelection(isColumnSelection);
            } else
                editor.setMark(null);
            editor.updateDotLine();
            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());
            if (wasMarked || editor.getMark() != null)
                display.setUpdateFlag(REPAINT);
        }
    }
}
