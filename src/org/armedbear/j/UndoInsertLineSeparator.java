/*
 * UndoInsertLineSeparator.java
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

public final class UndoInsertLineSeparator extends AbstractUndoableEdit
    implements Constants, UndoableEdit
{
    private PreState preState;
    private PostState postState;

    public UndoInsertLineSeparator(Editor editor)
    {
        preState = new PreState(editor);
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
        buffer.repaint();
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
        buffer.repaint();
    }

    private static class PreState
    {
        final int dotLineNumber;
        final int dotOffset;
        final int absCaretCol;
        final int modificationCount;
        final boolean modified;
        final Line line;

        PreState(Editor editor)
        {
            dotLineNumber = editor.getDotLine().lineNumber();
            dotOffset = editor.getDotOffset();
            absCaretCol = editor.getAbsoluteCaretCol();
            final Buffer buffer = editor.getBuffer();
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();
            line = editor.getDotLine().copy();
        }

        // Undo.
        void restoreState(Editor editor)
        {
            final Buffer buffer = editor.getBuffer();

            editor.setDot(dotLineNumber, 0);
            final Line before = editor.getDotLine().previous();
            final Line after = editor.getDotLine().next().next();
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
            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());
            display.setUpdateFlag(REPAINT);
        }
    }

    private static class PostState
    {
        int dotLineNumber;
        int dotOffset;
        int absCaretCol;
        int modificationCount;
        boolean modified;

        final Line first;
        final Line second;

        PostState(Editor editor)
        {
            final Line dotLine = editor.getDotLine();
            dotLineNumber = dotLine.lineNumber();
            dotOffset = editor.getDotOffset();
            absCaretCol = editor.getAbsoluteCaretCol();
            final Buffer buffer = editor.getBuffer();
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();
            second = dotLine.copy();
            first = dotLine.previous().copy();
        }

        // Redo.
        void restoreState(Editor editor)
        {
            final Buffer buffer = editor.getBuffer();

            Line before = editor.getDotLine().previous();
            Line after = editor.getDotLine().next();
            first.setPrevious(before);
            if (before != null)
                before.setNext(first);
            else
                buffer.setFirstLine(first);
            first.setNext(second);
            second.setPrevious(first);
            second.setNext(after);
            if (after != null)
                after.setPrevious(second);
            // Markers!!

            buffer.setModCount(modificationCount);

            buffer.needsRenumbering = true;
            buffer.renumber();

            editor.setDot(dotLineNumber, dotOffset);
            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());
            display.setUpdateFlag(REPAINT);
        }
    }
}
