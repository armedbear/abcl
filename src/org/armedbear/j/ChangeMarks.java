/*
 * ChangeMarks.java
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

import javax.swing.undo.CompoundEdit;

public final class ChangeMarks implements Constants
{
    public static void nextChange()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getDot() == null)
            return;
        Line line = editor.getDotLine();
        // Advance to first unmodified line.
        while (line.isModified()) {
            line = line.next();
            if (line == null)
                break;
        }
        if (line != null) {
            Debug.assertTrue(!line.isModified());
            // Advance to next modified line.
            while (!line.isModified()) {
                line = line.next();
                if (line == null)
                    break;
            }
        }
        if (line != null) {
            Debug.assertTrue(line.isModified());
            editor.moveDotTo(line, 0);
        } else
            editor.status("No more changes");
    }

    public static void previousChange()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getDot() == null)
            return;
        Line line = editor.getDotLine();
        // Go back to last unmodified line.
        while (line.isModified()) {
            line = line.previous();
            if (line == null)
                break;
        }
        if (line != null) {
            Debug.assertTrue(!line.isModified());
            // Go back to last modified line.
            while (!line.isModified()) {
                line = line.previous();
                if (line == null)
                    break;
            }
        }
        if (line != null) {
            Debug.assertTrue(line.isModified());
            editor.moveDotTo(line, 0);
        } else
            editor.status("No more changes");
    }

    public static void revertLines()
    {
        final Editor editor = Editor.currentEditor();
        final Position dot = editor.getDot(); // Alias.
        if (dot == null)
            return;
        if (editor.isColumnSelection()) {
            editor.notSupportedForColumnSelections();
            return;
        }
        final Position mark = editor.getMark(); // Alias.
        final Line dotLine = editor.getDotLine();
        Line before, last;
        if (mark != null) {
            Region r = new Region(editor);
            before = r.getBeginLine().previous();
            last = r.getEndLine();
        } else {
            before = dotLine.previous();
            last = dotLine;
        }
        // Find last unmodified line above dot or beginning of marked region.
        while (before != null && before.isModified())
            before = before.previous();
        // Find last modified line in current group of changed lines.
        for (Line line = last.next(); line != null; line = line.next()) {
            if (line.isModified())
                last = line;
            else
                break;
        }
        // Make sure at least one of the lines in question is in fact modified.
        boolean modified = false;
        for (Line line = last; line != before; line = line.previous()) {
            if (line.isModified()) {
                modified = true;
                break;
            }
        }
        if (!modified)
            return; // Nothing to revert.
        CompoundEdit compoundEdit = editor.beginCompoundEdit();
        editor.addUndo(SimpleEdit.MOVE);
        if (mark != null) {
            editor.setMark(null);
            editor.setUpdateFlag(REPAINT);
        }
        dot.moveTo(last, 0);
        revertLine(editor, dot.getLine());
        while (dot.getPreviousLine() != before) {
            editor.addUndo(SimpleEdit.MOVE);
            dot.moveTo(dot.getPreviousLine(), 0);
            revertLine(editor, dot.getLine());
        }
        editor.moveCaretToDotCol();
        editor.endCompoundEdit(compoundEdit);
    }

    private static void revertLine(Editor editor, Line line)
    {
        if (!line.isModified())
            return;
        final Buffer buffer = editor.getBuffer();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            if (line.isNew())
                revertNewLine(editor, line);
            else
                revertChangedLine(editor, line);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private static void revertNewLine(Editor editor, final Line dotLine)
    {
        editor.adjustMarkers(dotLine);
        final Line prev = dotLine.previous();
        final Line next = dotLine.next();
        if (prev == null && next == null)
            return;
        CompoundEdit compoundEdit = editor.beginCompoundEdit();
        compoundEdit.addEdit(new UndoMove(editor));
        final Position dot = editor.getDot(); // Alias.
        boolean insertBefore;
        if (next != null) {
            dot.moveTo(next, 0);
            insertBefore = true;
        } else {
            dot.moveTo(prev, 0);
            insertBefore = false;
        }
        editor.moveCaretToDotCol();
        final Buffer buffer = editor.getBuffer();
        compoundEdit.addEdit(new UndoRemoveLine(editor, insertBefore));
        if (prev != null)
            prev.setNext(next);
        else
            buffer.setFirstLine(next);
        if (next != null)
            next.setPrevious(prev);
        buffer.renumber();
        buffer.modified();
        editor.endCompoundEdit(compoundEdit);
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            if (ed.getTopLine() == dotLine)
                ed.setTopLine(dot.getLine());
        }
        buffer.repaint();
    }

    private static void revertChangedLine(Editor editor, final Line dotLine)
    {
        final Buffer buffer = editor.getBuffer();
        final Position dot = editor.getDot(); // Alias.
        final String originalText = dotLine.getOriginalText();
        if (originalText != null) {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            final int index = originalText.indexOf('\n');
            if (index >= 0) {
                // Multi-line change.
                final Line begin = dotLine;
                final Line end = dotLine.next();
                editor.addUndo(SimpleEdit.LINE_EDIT);
                dotLine.setText(originalText.substring(0, index));
                buffer.modified();
                editor.addUndo(SimpleEdit.MOVE);
                dot.setOffset(dotLine.length());
                editor.addUndo(SimpleEdit.INSERT_STRING);
                buffer.insertString(dot, originalText.substring(index));
                for (Line line = begin; line != end; line = line.next())
                    line.unmodified();
            } else {
                // Single line change.
                editor.addUndo(SimpleEdit.LINE_EDIT);
                dotLine.setText(originalText);
                dotLine.unmodified();
                Editor.updateInAllEditors(dotLine);
                buffer.modified();
                if (dot.getOffset() > dotLine.length()) {
                    editor.addUndo(SimpleEdit.MOVE);
                    dot.setOffset(dotLine.length());
                }
            }
            editor.moveCaretToDotCol();
            editor.endCompoundEdit(compoundEdit);
        }
    }

    public static void changes()
    {
        final Editor editor = Editor.currentEditor();
        final Buffer buffer = editor.getBuffer();
        final File file = buffer.getFile();
        if (file == null || file.isRemote())
            return;
        final File tempFile = Utilities.getTempFile();
        if (tempFile == null)
            return;
        if (buffer.writeFile(tempFile)) {
            FastStringBuffer sb = new FastStringBuffer("diff -u ");
            // Enclose filenames in double quotes if they contain embedded
            // spaces.
            String name1 = file.canonicalPath();
            if (name1.indexOf(' ') >= 0) {
                sb.append('"');
                sb.append(name1);
                sb.append('"');
            } else
                sb.append(name1);
            sb.append(' ');
            String name2 = tempFile.canonicalPath();
            if (name2.indexOf(' ') >= 0) {
                sb.append('"');
                sb.append(name2);
                sb.append('"');
            } else
                sb.append(name2);
            final String cmd = sb.toString();
            ShellCommand shellCommand = new ShellCommand(cmd);
            shellCommand.run();
            // Kill existing diff output buffer if any for same parent buffer.
            for (BufferIterator it = new BufferIterator(); it.hasNext();) {
                Buffer b = it.nextBuffer();
                if (b instanceof DiffOutputBuffer) {
                    if (((DiffOutputBuffer)b).getParentBuffer() == buffer) {
                        if (((DiffOutputBuffer)b).getVCType() == 0) {
                            b.kill();
                            break; // There should be one at most.
                        }
                    }
                }
            }
            String output = shellCommand.getOutput();
            if (output.length() == 0) {
                MessageDialog.showMessageDialog(editor, "No changes",
                    buffer.getFile().getName());
            } else {
                DiffOutputBuffer outputBuffer =
                    new DiffOutputBuffer(buffer, output, 0);
                sb.setLength(0);
                sb.append("diff -ub ");
                sb.append(file.getName());
                sb.append(' ');
                sb.append(tempFile.canonicalPath());
                outputBuffer.setTitle(sb.toString());
                editor.makeNext(outputBuffer);
                editor.activateInOtherWindow(outputBuffer);
            }
        }
        if (tempFile.isFile())
            tempFile.delete();
    }
}
