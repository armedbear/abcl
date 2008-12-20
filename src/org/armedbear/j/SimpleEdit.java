/*
 * SimpleEdit.java
 *
 * Copyright (C) 1998-2003 Peter Graves
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

public final class SimpleEdit
{
    public static final int MOVE            =  1;
    public static final int SCROLL_CARET    =  2;
    public static final int LINE_EDIT       =  3;
    public static final int INSERT_LINE_SEP =  4;
    public static final int DELETE_LINE_SEP =  5;
    public static final int INSERT_STRING   =  6;
    public static final int FOLD            =  7;

    public static boolean addUndo(Editor editor, int type)
    {
        final Buffer buffer = editor.getBuffer();
        if (!buffer.supportsUndo())
            return true; // Not an error.
        if (editor.getDot() == null)
            return true; // Not an error.
        if (buffer.needsRenumbering())
            buffer.renumber();
        switch (type) {
            case MOVE:
                buffer.addEdit(new UndoMove(editor));
                break;
            case SCROLL_CARET:
                buffer.addEdit(new UndoScrollCaret(editor));
                break;
            case LINE_EDIT:
                buffer.addEdit(new UndoLineEdit(editor));
                break;
            case INSERT_LINE_SEP:
                buffer.addEdit(new UndoInsertLineSeparator(editor));
                break;
            case DELETE_LINE_SEP:
                buffer.addEdit(new UndoDeleteLineSeparator(editor));
                break;
            case INSERT_STRING:
                buffer.addEdit(new UndoInsertString(editor));
                break;
            case FOLD:
                buffer.addEdit(new UndoFold(editor));
                break;
            default:
                Debug.bug();
                buffer.getUndoManager().discardAllEdits();
                break;
        }
        return true;
    }
}
