/*
 * UndoRemoveLine.java
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

public final class UndoRemoveLine extends AbstractUndoableEdit
    implements Constants, UndoableEdit
{
    private final boolean insertBefore;
    private final PreState preState;
    private PostState postState;

    public UndoRemoveLine(Editor editor, boolean insertBefore)
    {
        this.insertBefore = insertBefore;
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

    private class PreState
    {
        final int dotLineNumber;
        final int dotOffset;
        final int absCaretCol;
        final int modificationCount;
        final boolean modified;
        final Line line;

        PreState(Editor editor)
        {
            final Line dotLine = editor.getDotLine();
            dotLineNumber = dotLine.lineNumber();
            dotOffset = editor.getDotOffset();
            Debug.assertTrue(dotOffset == 0);
            absCaretCol = editor.getAbsoluteCaretCol();
            final Buffer buffer = editor.getBuffer();
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();

            final Line toBeRemoved = insertBefore ? dotLine.previous() : dotLine.next();
            Debug.assertTrue(toBeRemoved != null);
            line = toBeRemoved.copy();
        }

        // Undo remove line.
        void restoreState(Editor editor)
        {
            final Buffer buffer = editor.getBuffer();
            Line before, after;
            if (insertBefore) {
                before = editor.getDotLine().previous();
                after = editor.getDotLine();
            } else {
                before = editor.getDotLine();
                after = editor.getDotLine().next();
            }
            line.setPrevious(before);
            if (before != null)
                before.setNext(line);
            else
                buffer.setFirstLine(line);
            line.setNext(after);
            if (after != null)
                after.setPrevious(line);
            // Markers!!

            buffer.setModCount(modificationCount);
            buffer.needsRenumbering = true;
            buffer.renumber();

            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());
        }
    }

    private class PostState
    {
        final int dotLineNumber;
        final int dotOffset;
        final int absCaretCol;
        final int modificationCount;
        final boolean modified;

        PostState(Editor editor)
        {
            final Line dotLine = editor.getDotLine();
            dotLineNumber = dotLine.lineNumber();
            dotOffset = editor.getDotOffset();
            absCaretCol = editor.getAbsoluteCaretCol();
            final Buffer buffer = editor.getBuffer();
            modificationCount = buffer.getModCount();
            modified = buffer.isModified();
        }

        // Redo remove line.
        void restoreState(Editor editor)
        {
            final Buffer buffer = editor.getBuffer();

            Line remove;
            if (insertBefore) {
                // Remove line before dot.
                remove = editor.getDotLine().previous();
            } else {
                // Remove line after dot.
                remove = editor.getDotLine().next();
            }
            final Line before = remove.previous();
            final Line after = remove.next();

            if (before != null)
                before.setNext(after);
            else
                buffer.setFirstLine(after);
            if (after != null)
                after.setPrevious(before);
            // Markers!!

            buffer.setModCount(modificationCount);
            buffer.needsRenumbering = true;
            buffer.renumber();

            editor.setDot(dotLineNumber, dotOffset);
            final Display display = editor.getDisplay();
            display.setCaretCol(absCaretCol - display.getShift());

            for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                Editor ed = it.nextEditor();
                if (ed.getTopLine() == remove)
                    ed.setTopLine(editor.getDotLine());
            }
        }
    }
}
