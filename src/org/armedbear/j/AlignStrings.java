/*
 * AlignString.java
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

import gnu.regexp.RE;
import gnu.regexp.REException;
import gnu.regexp.REMatch;
import javax.swing.undo.CompoundEdit;

public final class AlignStrings
{
    public static void alignStrings()
    {
        final Editor editor = Editor.currentEditor();
        InputDialog d =
            new InputDialog(editor, "Regular Expression:", "Align Strings",
                null);
        d.setHistory(new History("alignStrings"));
        editor.centerDialog(d);
        d.show();
        String input = d.getInput();
        if (input != null)
            alignStrings(input);
    }

    public static void alignStrings(String s)
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        if (editor.getMark() == null)
            return;
        final Region region = new Region(editor);
        if (region.getEndLineNumber() - region.getBeginLineNumber() < 2)
            return;
        s = unquote(s);
        RE re;
        try {
            re = new RE(s);
        }
        catch (REException e) {
            MessageDialog.showMessageDialog(editor, e.getMessage(), "Error");
            return;
        }
        final Buffer buffer = editor.getBuffer();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            _alignStrings(editor, buffer, region, re);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private static void _alignStrings(Editor editor, Buffer buffer,
        Region region, RE re)
    {
        int maxCol = -1;
        for (Line line = region.getBeginLine(); line != region.getEndLine();
            line = line.next()) {
            String text = line.getText();
            if (text != null) {
                REMatch match = re.getMatch(text);
                if (match != null) {
                    int offset = match.getStartIndex();
                    int col = buffer.getCol(line, offset);
                    if (col > maxCol)
                        maxCol = col;
                }
            }
        }
        if (maxCol < 0)
            return;
        Position savedDot = new Position(editor.getDot());
        CompoundEdit compoundEdit = buffer.beginCompoundEdit();
        for (Line line = region.getBeginLine(); line != region.getEndLine();
            line = line.next()) {
            String text = line.getText();
            if (text != null) {
                REMatch match = re.getMatch(text);
                if (match != null) {
                    int offset = match.getStartIndex();
                    int col = buffer.getCol(line, offset);
                    if (col < maxCol) {
                        editor.addUndo(SimpleEdit.MOVE);
                        editor.getDot().moveTo(line, offset);
                        editor.addUndo(SimpleEdit.LINE_EDIT);
                        buffer.insertChars(editor.getDot(),
                            Utilities.spaces(maxCol - col));
                        Editor.updateInAllEditors(buffer, line);
                    }
                }
            }
        }
        editor.getDot().moveTo(savedDot);
        editor.moveCaretToDotCol();
        buffer.endCompoundEdit(compoundEdit);
    }

    private static String unquote(String s)
    {
        int length = s.length();
        if (length >= 2) {
            char c = s.charAt(0);
            if (c == '"' || c == '\'') {
                if (s.charAt(length-1) == c)
                    return s.substring(1, length-1);
            }
        }
        return s;
    }
}
