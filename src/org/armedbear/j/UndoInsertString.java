/*
 * UndoInsertString.java
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

public class UndoInsertString extends AbstractUndoableEdit
    implements Constants, UndoableEdit
{
    private final Buffer buffer;
    private final PreState preState;
    private PostState postState;
    private LineSequence lines;

    public UndoInsertString(Editor editor)
    {
        buffer = editor.getBuffer();
        preState = new PreState(editor);
        postState = new PostState(editor);
    }

    public void undo()
    {
        super.undo();
        final Editor editor = Editor.currentEditor();
        Debug.assertTrue(editor.getBuffer() == buffer);
        postState = new PostState(editor);
        preState.restoreState(editor);
        update(editor);
    }

    public void redo()
    {
        super.redo();
        final Editor editor = Editor.currentEditor();
        Debug.assertTrue(editor.getBuffer() == buffer);
        postState.restoreState(editor);
        update(editor);
    }

    private void update(Editor editor)
    {
        editor.setUpdateFlag(REFRAME);
        if (postState.modificationCount != preState.modificationCount) {
            // Buffer was changed.
            buffer.invalidate();
            Sidebar.setUpdateFlagInAllFrames(SIDEBAR_MODIFIED_BUFFER_COUNT |
                SIDEBAR_REPAINT_BUFFER_LIST);
            Sidebar.repaintBufferListInAllFrames();
        }
    }

    private class PreState
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

        PreState(Editor editor)
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
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();
            line = editor.getDotLine().copy();
        }

        // Undo.
        void restoreState(Editor editor)
        {
            final Line first = buffer.getLine(dotLineNumber);
            final Line last = editor.getDotLine();
            final Line after = last.next();
            for (Line ln = first; ln != after; ln = ln.next())
                editor.adjustMarkers(ln);
            if (first == last) {
                lines = new LineSequence(first);
                first.copy(line);
                Editor.updateInAllEditors(first);
            } else {
                final Line before = first.previous();
                lines = new LineSequence(first, last);
                final Line restored = line.copy();
                restored.setPrevious(before);
                if (before != null)
                    before.setNext(restored);
                else
                    buffer.setFirstLine(restored);
                restored.setNext(after);
                if (after != null)
                    after.setPrevious(restored);
                buffer.needsRenumbering = true;
                buffer.renumber();
                buffer.repaint();
            }
            buffer.setModCount(modificationCount);
            editor.setDot(dotLineNumber, dotOffset);
            editor.setMark(null);
            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());
        }
    }

    private class PostState
    {
        final int dotLineNumber;
        final int dotOffset;
        final int markLineNumber;
        final int markOffset;
        final int absCaretCol;
        final boolean isColumnSelection;
        final int modificationCount;
        final boolean modified;

        PostState(Editor editor)
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
            modificationCount = editor.getBuffer().getModCount();
            modified = editor.getBuffer().isModified();
        }

        // Redo.
        void restoreState(Editor editor)
        {
            final Line dotLine = editor.getDotLine();
            if (lines.size() == 1) {
                dotLine.copy(lines.getFirstLine());
                Editor.updateInAllEditors(dotLine);
            } else {
                final Line before = dotLine.previous();
                final Line after = dotLine.next();
                lines.getFirstLine().setPrevious(before);
                if (before != null)
                    before.setNext(lines.getFirstLine());
                else
                    buffer.setFirstLine(lines.getFirstLine());
                lines.getLastLine().setNext(after);
                if (after != null)
                    after.setPrevious(lines.getLastLine());
                buffer.needsRenumbering = true;
                buffer.renumber();
                buffer.repaint();
            }
            buffer.setModCount(modificationCount);
            editor.setDot(dotLineNumber, dotOffset);
            editor.setMark(null);
            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());
        }
    }
}
