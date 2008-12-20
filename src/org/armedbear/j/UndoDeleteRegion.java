/*
 * UndoDeleteRegion.java
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

public class UndoDeleteRegion extends AbstractUndoableEdit
    implements Constants, UndoableEdit
{
    private final PreState preState;
    private PostState postState;
    private final LineSequence lines;

    public UndoDeleteRegion(Editor editor, Region r)
    {
        Debug.assertTrue(!editor.isColumnSelection());
        preState = new PreState(editor);
        lines = new LineSequence(r.getBeginLine(), r.getEndLine());
    }

    public void undo()
    {
        super.undo();
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        postState = new PostState(editor);
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
        final Buffer buffer = editor.getBuffer();
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

    private class PreState
    {
        final int dotLineNumber;
        final int dotOffset;
        final int markLineNumber;
        final int markOffset;
        final int absCaretCol;
        final int modificationCount;
        final boolean modified;

        PreState(Editor editor)
        {
            final Line dotLine = editor.getDotLine();
            dotLineNumber = dotLine.lineNumber();
            dotOffset = editor.getDotOffset();
            absCaretCol = editor.getAbsoluteCaretCol();
            Position mark = editor.getMark();
            if (mark != null) {
                markLineNumber = mark.lineNumber();
                markOffset = mark.getOffset();
            } else {
                markLineNumber = -1;
                markOffset = -1;
            }
            final Buffer buffer = editor.getBuffer();
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();
        }

        // Undo.
        void restoreState(Editor editor)
        {
            final Buffer buffer = editor.getBuffer();

            final Line dotLine = editor.getDotLine();
            final Line before = dotLine.previous();
            final Line after = dotLine.next();

            editor.adjustMarkers(dotLine);

            Line first = lines.getFirstLine();
            Line last = lines.getLastLine();
            first.setPrevious(before);
            if (before != null)
                before.setNext(first);
            else
                buffer.setFirstLine(first);
            last.setNext(after);
            if (after != null)
                after.setPrevious(last);

            buffer.setModCount(modificationCount);

            buffer.needsRenumbering = true;
            buffer.renumber();

            editor.setDot(dotLineNumber, dotOffset);
            editor.setMark(markLineNumber, markOffset);
            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());
            display.setUpdateFlag(REPAINT);
        }
    }

    private class PostState
    {
        final int dotLineNumber;
        final int dotOffset;
        final int absCaretCol;
        final int modificationCount;
        final boolean modified;
        final Line line;

        PostState(Editor editor)
        {
            final Line dotLine = editor.getDotLine();
            dotLineNumber = dotLine.lineNumber();
            dotOffset = editor.getDotOffset();
            absCaretCol = editor.getAbsoluteCaretCol();
            final Buffer buffer = editor.getBuffer();
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();
            line = dotLine.copy();
        }

        // Redo the deletion.
        void restoreState(Editor editor)
        {
            final Buffer buffer = editor.getBuffer();

            final Line before = editor.getDotLine().previous();
            Line after = editor.getDotLine();
            for (int i = 0; i < lines.size(); i++) {
                after = after.next();
                if (after == null)
                    break;
            }

            final Line restored = line.copy();
            restored.setPrevious(before);
            if (before != null)
                before.setNext(restored);
            else
                buffer.setFirstLine(restored);
            restored.setNext(after);
            if (after != null)
                after.setPrevious(restored);
            // Markers!!

            buffer.setModCount(modificationCount);

            buffer.needsRenumbering = true;
            buffer.renumber();

            editor.setDot(restored, dotOffset);
            editor.setMark(null);
            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());
            display.setUpdateFlag(REPAINT);
        }
    }
}
