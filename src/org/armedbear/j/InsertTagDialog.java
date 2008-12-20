/*
 * InsertTagDialog.java
 *
 * Copyright (C) 2000-2004 Peter Graves
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

import java.util.ArrayList;
import java.util.List;
import javax.swing.undo.CompoundEdit;

public final class InsertTagDialog extends InputDialog implements Constants
{
    public InsertTagDialog(Editor editor)
    {
        super(editor, "Element:", "Insert Element", null);
        setHistory(new History("insertTag"));
    }

    protected List getCompletions(String prefix)
    {
        prefix = prefix.toLowerCase();
        List elements = HtmlMode.elements();
        int limit = elements.size();
        ArrayList completions = new ArrayList(limit);
        for (int i = 0; i < limit; i++) {
            HtmlElement element = (HtmlElement) elements.get(i);
            if (element.getName().startsWith(prefix))
                completions.add(element.getName());
        }
        return completions;
    }

    public static void insertTag(Editor editor, String tagName, String extra,
                                 boolean wantEndTag)
    {
        if (extra == null)
            extra = "";
        final Buffer buffer = editor.getBuffer();
        if (buffer.getBooleanProperty(Property.FIX_CASE)) {
            if (buffer.getBooleanProperty(Property.UPPER_CASE_TAG_NAMES))
                tagName = tagName.toUpperCase();
            else
                tagName = tagName.toLowerCase();
        }
        Region r = null;
        if (editor.getMark() != null)
            r = new Region(editor);
        else {
            // No selection.
            final Position dot = editor.getDot();
            final Line dotLine = dot.getLine();
            final int offset = dot.getOffset();
            if (offset > 0 && offset < dotLine.length()) {
                char before = dotLine.charAt(offset-1);
                char after = dotLine.charAt(offset);
                if (!Character.isWhitespace(before) && !Character.isWhitespace(after)) {
                    int begin = offset;
                    char c;
                    while (begin > 0 && !Character.isWhitespace(c = dotLine.charAt(begin-1))) {
                        if (c == '>')
                            break;
                        --begin;
                    }
                    int end = offset;
                    while (end < dotLine.length() && !Character.isWhitespace(c = dotLine.charAt(end))) {
                        if (",.<".indexOf(c) >= 0)
                            break;
                        ++end;
                    }
                    if (begin != end)
                        r = new Region(buffer, new Position(dotLine, begin),
                                       new Position(dotLine, end));
                }
            }
        }
        if (r != null) {
            boolean wantNewLine = false;
            if (r.getBeginOffset() == 0 && r.getEndOffset() == 0)
                wantNewLine = true;
            final int offset = editor.getDotOffset();
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            if (wantEndTag) {
                editor.moveDotTo(r.getEnd());
                editor.addUndo(SimpleEdit.INSERT_STRING);
                editor.insertStringInternal("</" + tagName + ">");
                Editor.updateInAllEditors(editor.getDotLine());
                if (wantNewLine) {
                    editor.addUndo(SimpleEdit.INSERT_LINE_SEP);
                    editor.insertLineSeparator();
                }
            }
            editor.moveDotTo(r.getBegin());
            editor.addUndo(SimpleEdit.INSERT_STRING);
            String insert = "<" + tagName + extra + ">";
            editor.insertStringInternal(insert);
            Editor.updateInAllEditors(editor.getDotLine());
            if (wantNewLine) {
                Line startTagLine = editor.getDotLine();
                editor.addUndo(SimpleEdit.INSERT_LINE_SEP);
                editor.insertLineSeparator();
                if (buffer.getBooleanProperty(Property.AUTO_INDENT)) {
                    // Re-indent.
                    // Move dot to start of region.
                    editor.addUndo(SimpleEdit.MOVE);
                    editor.getDot().moveTo(startTagLine, 0);
                    Position dot = editor.getDot();
                    while (dot.getLine() != null && dot.getLine() != r.getEndLine().next()) {
                        editor.indentLine();
                        dot.moveTo(dot.getNextLine(), 0);
                    }
                }
                // Put dot before '>' of start tag.
                editor.moveDotTo(startTagLine, startTagLine.length()-1);
            } else {
                int newOffset;
                if (extra.endsWith(" "))
                    newOffset = editor.getDotOffset() - 1;
                else
                    newOffset = offset + insert.length();
                editor.moveDotTo(editor.getDotLine(), newOffset);
            }
            editor.endCompoundEdit(compoundEdit);
        } else {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            editor.fillToCaret();
            final int offset = editor.getDotOffset();
            editor.addUndo(SimpleEdit.INSERT_STRING);
            if (wantEndTag)
                editor.insertStringInternal("<" + tagName + extra + "></" + tagName + ">");
            else
                editor.insertStringInternal("<" + tagName + extra + ">");
            Editor.updateInAllEditors(editor.getDotLine());
            editor.addUndo(SimpleEdit.MOVE);
            editor.getDot().setOffset(offset + 1 + tagName.length() + extra.length());
            editor.moveCaretToDotCol();
            editor.endCompoundEdit(compoundEdit);
        }
    }
}
