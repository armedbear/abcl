/*
 * UndoLineEdit.java
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
import javax.swing.undo.UndoableEdit;

public class UndoLineEdit extends AbstractUndoableEdit implements Constants,
    UndoableEdit
{
    private final Buffer buffer;
    private final int changeLineNumber;
    private final State preState;
    private State postState;

    public UndoLineEdit(Editor editor)
    {
        buffer = editor.getBuffer();
        changeLineNumber = editor.getDot().lineNumber();
        preState = new State(editor);
    }

    public UndoLineEdit(Buffer buffer, Line line)
    {
        this.buffer = buffer;
        changeLineNumber = line.lineNumber();
        preState = new State(buffer);
    }

    public boolean addEdit(UndoableEdit edit)
    {
        if (edit instanceof UndoLineEdit) {
            UndoLineEdit e = (UndoLineEdit) edit;
            if (e.changeLineNumber == this.changeLineNumber)
                return true;
        }
        return false;
    }

    public void undo()
    {
        super.undo();
        final Editor editor = Editor.currentEditor();
        Debug.assertTrue(editor.getBuffer() == buffer);
        postState = new State(editor);
        preState.restoreState(editor);
        editor.setUpdateFlag(REFRAME);

        if (postState.modificationCount != preState.modificationCount) {
            // Buffer was changed.
            buffer.invalidate();
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_MODIFIED_BUFFER_COUNT |
                SIDEBAR_REPAINT_BUFFER_LIST);
            Sidebar.repaintBufferListInAllFrames();
        }
    }

    public void redo()
    {
        super.redo();
        final Editor editor = Editor.currentEditor();
        Debug.assertTrue(editor.getBuffer() == buffer);
        postState.restoreState(editor);
        editor.setUpdateFlag(REFRAME);

        if (postState.modificationCount != preState.modificationCount) {
            // Buffer was changed.
            buffer.invalidate();
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_MODIFIED_BUFFER_COUNT |
                SIDEBAR_REPAINT_BUFFER_LIST);
            Sidebar.repaintBufferListInAllFrames();
        }
    }

    private class State
    {
        final int dotLineNumber;
        final int dotOffset;
        final int markLineNumber;
        final int markOffset;
        final int absCaretCol;
        final boolean isColumnSelection;
        final int modificationCount;
        final boolean modified;
        final Line line;

        State(Editor editor)
        {
            final Line dotLine = editor.getDotLine();
            dotLineNumber = dotLine.lineNumber();
            final int length = dotLine.length();
            final int offset = editor.getDotOffset();
            if (offset > length)
                Debug.bug();
            dotOffset = offset > length ? length : offset;
            absCaretCol = editor.getAbsoluteCaretCol();
            Position mark = editor.getMark();
            if (mark != null) {
                markLineNumber = mark.lineNumber();
                markOffset = mark.getOffset();
            } else {
                markLineNumber = -1;
                markOffset = -1;
            }
            isColumnSelection = editor.isColumnSelection();
            final Buffer buffer = editor.getBuffer();
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();
            line = buffer.getLine(changeLineNumber).copy();
        }

        State(Buffer buffer)
        {
            dotLineNumber = -1;
            dotOffset = -1;
            absCaretCol = -1;
            markLineNumber = -1;
            markOffset = -1;
            isColumnSelection = false;
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();
            line = buffer.getLine(changeLineNumber).copy();
        }

        void restoreState(Editor editor)
        {
            Debug.assertTrue(editor.getBuffer() == buffer);
            Debug.assertTrue(changeLineNumber >= 0);

            if (!buffer.isWriteLocked())
                Debug.bug();
            Line changeLine = buffer.getLine(changeLineNumber);
            changeLine.copy(line);
            Editor.updateInAllEditors(changeLine);

            buffer.setModCount(modificationCount);

            boolean wasMarked = editor.getMark() != null;
            editor.updateDotLine();
            editor.setDot(dotLineNumber, dotOffset);
            if (dotOffset > editor.getDotLine().length()) {
                Debug.bug();
                editor.getDot().setOffset(editor.getDotLine().length());
            }
            if (markLineNumber >= 0) {
                editor.setMark(markLineNumber, markOffset);
                editor.setColumnSelection(isColumnSelection);
            } else
                editor.setMark(null);
            editor.updateDotLine();
            final Display display = editor.getDisplay();
            if (absCaretCol >= 0)
                display.setCaretCol(absCaretCol - display.getShift());
            if (wasMarked || editor.getMark() != null)
                display.setUpdateFlag(REPAINT);
        }
    }
}
