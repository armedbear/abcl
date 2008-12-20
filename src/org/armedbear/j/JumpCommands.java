/*
 * JumpCommands.java
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

public final class JumpCommands implements Constants
{
    public static void jumpToLine()
    {
        final Editor editor = Editor.currentEditor();
        String response = InputDialog.showInputDialog(editor, "Line number:",
            "Jump To Line");
        if (response == null || response.length() == 0)
            return;
        try {
            final int here = editor.getDotLineNumber() + 1;
            int lineNumber = parseNumericInput(response, here) - 1;
            editor.jumpToLine(lineNumber);
        }
        catch (NumberFormatException e) {
            MessageDialog.showMessageDialog(editor, "Invalid line number", "Error");
        }
    }

    public static void jumpToColumn()
    {
        final Editor editor = Editor.currentEditor();
        String response = InputDialog.showInputDialog(editor, "Column number:",
            "Jump To Column");
        if (response == null || response.length() == 0)
            return;
        try {
            final int here = editor.getDotCol() + 1;
            int col = parseNumericInput(response, here) - 1;
            final Display display = editor.getDisplay();
            if (col >= 0 && col != display.getAbsoluteCaretCol()) {
                editor.addUndo(SimpleEdit.MOVE);
                editor.unmark();
                display.setCaretCol(col - display.getShift());
                editor.moveDotToCaretCol();
                display.setUpdateFlag(REFRAME);
            }
        }
        catch (NumberFormatException e) {
            MessageDialog.showMessageDialog(editor, "Invalid column number", "Error");
        }
    }

    public static void jumpToOffset()
    {
        final Editor editor = Editor.currentEditor();
        String response = InputDialog.showInputDialog(editor, "Offset:",
            "Jump To Offset");
        if (response == null || response.length() == 0)
            return;
        try {
            final Buffer buffer = editor.getBuffer();
            // Zero-based.
            int here = buffer.getAbsoluteOffset(editor.getDot());
            int offset = parseNumericInput(response, here);
            Position pos = buffer.getPosition(offset);
            if (pos != null) {
                editor.moveDotTo(pos);
                return;
            }
        }
        catch (NumberFormatException e) {
            MessageDialog.showMessageDialog(editor, "Invalid offset", "Error");
        }
    }

    private static int parseNumericInput(String s, int here) throws NumberFormatException
    {
        s = s.trim();
        if (s.length() == 0)
            throw new NumberFormatException();
        char c = s.charAt(0);
        if (c == '+' || c == '-') {
            // It's an offset from the current location.
            int offset = Integer.parseInt(s.substring(1).trim());
            return c == '+' ? here + offset : here - offset;
        }
        return Integer.parseInt(s);
    }
}
