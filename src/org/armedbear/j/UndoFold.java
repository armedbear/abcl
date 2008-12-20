/*
 * UndoFold.java
 *
 * Copyright (C) 2002 Peter Graves
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

public final class UndoFold extends AbstractUndoableEdit
    implements Constants, UndoableEdit
{
    private final State preState;
    private State postState;

    public UndoFold(Editor editor)
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
        final HiddenLines hiddenLines;

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
            hiddenLines = new HiddenLines(editor);
        }

        void restoreState(Editor editor)
        {
            // Remember the top line in the current edit window (try not to
            // reframe unnecessarily).
            final Display display = editor.getDisplay();
            int topLineNumber = display.getTopLineNumber();

            hiddenLines.restore();

            // Use same top line if possible.
            Line topLine = display.getTopLine();

            if (topLine.isHidden()) {
                Line line = topLine.previousVisible();
                if (line == null)
                    line = topLine.nextVisible();
                editor.setTopLine(line);
            }
            editor.setDot(dotLineNumber, dotOffset);
            if (markLineNumber >= 0) {
                editor.setMark(markLineNumber, markOffset);
                editor.setColumnSelection(isColumnSelection);
            } else
                editor.setMark(null);
            display.setCaretCol(absCaretCol - display.getShift());
            display.setUpdateFlag(REPAINT);

            // Update any other windows displaying this buffer.
            if (Editor.getEditorCount() > 1) {
                for (EditorIterator it = new EditorIterator(); it.hasNext();) {
                    Editor ed = it.nextEditor();
                    if (ed.getBuffer() == editor.getBuffer()) {
                        // Make sure dot is visible.
                        if (ed.getDot().isHidden()) {
                            Line line = ed.getDotLine().previousVisible();
                            if (line != null) {
                                ed.setDot(line, 0);
                                ed.moveCaretToDotCol();
                            }
                        }
                        ed.getDisplay().repaint();
                    }
                }
            }
        }
    }
}
