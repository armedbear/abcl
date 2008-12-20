/*
 * RegionCommands.java
 *
 * Copyright (C) 1998-2004 Peter Graves
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
import gnu.regexp.REMatch;
import gnu.regexp.UncheckedRE;
import java.io.IOException;
import java.io.OutputStream;
import javax.swing.undo.CompoundEdit;
import org.armedbear.j.mail.Base64Decoder;

public final class RegionCommands
{
    public static void detabRegion()
    {
        detabOrEntabRegion(false);
    }

    public static void entabRegion()
    {
        detabOrEntabRegion(true);
    }

    private static void detabOrEntabRegion(boolean entab)
    {
        final Editor editor = Editor.currentEditor();
        final Position mark = editor.getMark(); // Alias, not copy.
        if (mark == null)
            return;
        editor.setWaitCursor();
        final Buffer buffer = editor.getBuffer();
        final int count = Editor.getEditorCount();
        Editor[] editors = new Editor[count];
        int[] dotCol = new int[count];
        int[] markCol = new int[count];
        int i = 0;
        for (EditorIterator it = new EditorIterator(); it.hasNext();) {
            Editor ed = it.nextEditor();
            editors[i] = ed;
            if (ed.getBuffer() == buffer) {
                dotCol[i] = ed.getDotCol();
                if (ed.getMark() != null)
                    markCol[i] = buffer.getCol(ed.getMark());
            }
            ++i;
        }
        final Region r = new Region(editor);
        final int tabWidth = buffer.getTabWidth();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            detabOrEntabRegion(editor, buffer, r, entab, tabWidth);
        }
        finally {
            buffer.unlockWrite();
        }
        for (i = 0; i < count; i++) {
            Editor ed = editors[i];
            if (ed.getBuffer() == buffer) {
                ed.moveDotToCol(dotCol[i]);
                if (ed.getMark() != null)
                    ed.getMark().moveToCol(markCol[i], tabWidth);
            }
        }
        buffer.repaint();
        editor.setDefaultCursor();
    }

    private static void detabOrEntabRegion(Editor editor, Buffer buffer,
        Region r, boolean entab, int tabWidth)
    {
        CompoundEdit compoundEdit = new CompoundEdit();
        compoundEdit.addEdit(new UndoMove(editor));
        boolean changed = false;
        final Line beginLine = r.getBeginLine();
        final int beginOffset = r.getBeginOffset();
        final Line endLine = r.getEndLine();
        final int endOffset = r.getEndOffset();
        final int startCol = buffer.getCol(beginLine, beginOffset);
        if (beginLine == endLine) {
            final String oldText = beginLine.getText();
            final String head = oldText.substring(0, beginOffset);
            final String toBeChanged = oldText.substring(beginOffset,
                endOffset);
            final String tail = oldText.substring(endOffset);
            FastStringBuffer sb = new FastStringBuffer(head);
            if (entab)
                sb.append(Utilities.entab(toBeChanged, tabWidth, startCol));
            else
                sb.append(Utilities.detab(toBeChanged, tabWidth, startCol));
            sb.append(tail);
            final String newText = sb.toString();
            if (!newText.equals(oldText)) {
                compoundEdit.addEdit(new UndoLineEdit(buffer, beginLine));
                beginLine.setText(newText);
                changed = true;
            }
        } else {
            // Begin line.
            String oldText = beginLine.getText();
            final String head = oldText.substring(0, beginOffset);
            String toBeChanged = oldText.substring(beginOffset);
            FastStringBuffer sb = new FastStringBuffer(head);
            if (entab)
                sb.append(Utilities.entab(toBeChanged, tabWidth, startCol));
            else
                sb.append(Utilities.detab(toBeChanged, tabWidth, startCol));
            String newText = sb.toString();
            if (!newText.equals(oldText)) {
                compoundEdit.addEdit(new UndoLineEdit(buffer, beginLine));
                beginLine.setText(newText);
                changed = true;
            }

            // Lines in middle of region.
            Line line = beginLine.next();
            while (line != null && line != endLine) {
                oldText = line.getText();
                if (entab)
                    newText = Utilities.entab(oldText, tabWidth);
                else
                    newText = Utilities.detab(oldText, tabWidth);
                if (!newText.equals(oldText)) {
                    compoundEdit.addEdit(new UndoLineEdit(buffer, line));
                    line.setText(newText);
                    changed = true;
                }
                line = line.next();
            }

            // End line.
            oldText = endLine.getText();
            toBeChanged = oldText.substring(0, endOffset);
            final String tail = oldText.substring(endOffset);
            sb.setLength(0);
            if (entab)
                sb.append(Utilities.entab(toBeChanged, tabWidth));
            else
                sb.append(Utilities.detab(toBeChanged, tabWidth));
            sb.append(tail);
            newText = sb.toString();
            if (!newText.equals(oldText)) {
                compoundEdit.addEdit(new UndoLineEdit(buffer, endLine));
                endLine.setText(newText);
                changed = true;
            }
        }
        if (changed) {
            compoundEdit.end();
            buffer.addEdit(compoundEdit);
            buffer.modified();
        }
    }

    public static void upperCaseRegion()
    {
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        changeCaseRegion(editor, true);
        editor.setDefaultCursor();
    }

    public static void lowerCaseRegion()
    {
        final Editor editor = Editor.currentEditor();
        editor.setWaitCursor();
        changeCaseRegion(editor, false);
        editor.setDefaultCursor();
    }

    private static void changeCaseRegion(Editor editor, boolean toUpper)
    {
        if (!editor.checkReadOnly())
            return;
        if (editor.getDot() == null || editor.getMark() == null)
            return;
        if (editor.getDot().equals(editor.getMark()))
            return;
        final Buffer buffer = editor.getBuffer();

        // A hard update is only necessary if the region spans a line boundary.
        boolean hard = editor.getDotLine() != editor.getMarkLine();

        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            editor.addUndo(SimpleEdit.MOVE);
            final Region r = new Region(editor);
            final String s = r.toString();
            editor.getDot().moveTo(r.getBegin());

            // Save undo information before calling Region.delete so modified
            // flag will be correct if we revert.
            editor.addUndoDeleteRegion(r);

            // Sets buffer modified flag.
            r.delete();

            editor.addUndo(SimpleEdit.INSERT_STRING);
            editor.insertStringInternal(toUpper ? s.toUpperCase() : s.toLowerCase());
            editor.endCompoundEdit(compoundEdit);
            editor.moveCaretToDotCol();
            if (hard)
                buffer.repaint();
            else
                Editor.updateInAllEditors(editor.getDotLine());
            editor.setMark(null);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    public static void decodeRegion()
    {
        final Editor editor = Editor.currentEditor();
        ByteBuffer bb = new ByteBuffer();
        if (editor.getMark() != null) {
            if (editor.getMarkOffset() == 0 && editor.getDotOffset() == 0) {
                Region r = new Region(editor);
                for (Line line = r.getBeginLine(); line != r.getEndLine(); line = line.next()) {
                    byte[] decodedBytes = decodeLine(line);
                    if (decodedBytes == null) {
                        MessageDialog.showMessageDialog("Unable to decode region", "Decode Region");
                        return;
                    }
                    bb.append(decodedBytes);
                }
            } else {
                MessageDialog.showMessageDialog("Region must consist of whole lines only.", "Decode Region");
                return;
            }
        } else {
            byte[] decodedBytes = decodeLine(editor.getDotLine());
            if (decodedBytes == null) {
                MessageDialog.showMessageDialog("Unable to decode region", "Decode Region");
                return;
            }
            bb.append(decodedBytes);
        }
        byte[] bytes = bb.getBytes();
        final int length = bb.length();
        boolean isBinary = false;
        for (int i = 0; i < length; i++) {
            byte b = bytes[i];
            if (b == 0) {
                isBinary = true;
                break;
            }
        }
        if (isBinary) {
            SaveFileDialog d = new SaveFileDialog(editor, "Save As");
            editor.centerDialog(d);
            d.show();
            File saveAs = d.getDestination();
            if (saveAs == null)
                return;
            // At this point, if the target file exists, the user has said
            // it's OK to overwrite it.
            try {
                OutputStream out = saveAs.getOutputStream();
                if (out != null) {
                    out.write(bytes, 0, length);
                    out.flush();
                    out.close();
                }
            }
            catch (IOException e) {
                Log.error(e);
            }
        } else {
            Buffer buf = new Buffer(0);
            buf.setText(new String(bytes, 0, length));
            editor.makeNext(buf);
            editor.activate(buf);
        }
    }

    private static final byte[] decodeLine(Line line)
    {
        return Base64Decoder.decode(line.trim());
    }

    public static void renumberRegion()
    {
        renumberRegion(null);
    }

    public static void renumberRegion(String arg)
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getMark() == null)
            return;
        final Region region = new Region(editor);
        if (region.getEndLineNumber() - region.getBeginLineNumber() < 2)
            return;
        if (!editor.checkReadOnly())
            return;
        int start = -1;
        if (arg != null) {
            try {
                start = Integer.parseInt(arg);
            }
            catch (NumberFormatException e) {
                MessageDialog.showMessageDialog(
                    "Invalid number \"" + arg + '"',
                    "Error");
                return;
            }
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
            _renumberRegion(editor, buffer, region, start);
        }
        finally {
            buffer.unlockWrite();
        }
    }

    private static void _renumberRegion(Editor editor, Buffer buffer,
        Region region, int start)
    {
        CompoundEdit compoundEdit = null;
        if (start < 0) {
            for (Line line = region.getBeginLine(); line != region.getEndLine(); line = line.next()) {
                final String text = line.getText();
                int index = findNumber(text, buffer.getMode());
                if (index >= 0) {
                    FastStringBuffer sb = new FastStringBuffer();
                    while (index < text.length()) {
                        char c = text.charAt(index++);
                        if (c >= '0' && c <= '9')
                            sb.append(c);
                        else
                            break;
                    }
                    try {
                        int n = Integer.parseInt(sb.toString());
                        if (start < 0)
                            start = n;
                        else if (n < start)
                            start = n;
                    }
                    catch (NumberFormatException e) {
                        Log.error(e);
                    }
                }
            }
        }
        for (Line line = region.getBeginLine(); line != region.getEndLine(); line = line.next()) {
            final String text = line.getText();
            int index = findNumber(text, buffer.getMode());
            if (index < 0)
                continue;
            FastStringBuffer sb = new FastStringBuffer(text.substring(0, index));
            while (index < text.length() && Character.isDigit(text.charAt(index)))
                ++index;
            sb.append(start++);
            sb.append(text.substring(index));
            String newText = sb.toString();
            if (!newText.equals(text)) {
                if (compoundEdit == null) {
                    compoundEdit = new CompoundEdit();
                    compoundEdit.addEdit(new UndoMove(editor));
                }
                compoundEdit.addEdit(new UndoLineEdit(buffer, line));
                line.setText(newText);
            }
        }
        if (compoundEdit != null) {
            compoundEdit.end();
            buffer.addEdit(compoundEdit);
            buffer.modified();
        }
        buffer.setNeedsParsing(true);
        buffer.getFormatter().parseBuffer();
        buffer.repaint();
    }

    private static int findNumber(String text, Mode mode)
    {
        RE re = new UncheckedRE("[0-9]+");
        int index = 0;
        int limit = text.length();
        while (index <= limit) {
            REMatch match = re.getMatch(text, index);
            if (match == null)
                return -1;
            if (isDelimited(text, match.getStartIndex(), match.toString().length(), mode))
                return match.getStartIndex();
            index = match.getStartIndex() + 1;
        }
        return -1;
    }

    private static boolean isDelimited(String text, int index, int length, Mode mode)
    {
        final int before = index - 1;
        if (before >= 0 && mode.isIdentifierPart(text.charAt(before)))
            return false;
        final int after = index + length;
        if (after < text.length() && mode.isIdentifierPart(text.charAt(after)))
            return false;
        return true;
    }

    public static void doShellCommandOnRegion()
    {
        if (!Editor.checkExperimental())
            return;
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        if (editor.getMark() == null) {
            MessageDialog.showMessageDialog(editor, "No region selected",
                                            "Error");
            return;
        }
        InputDialog d = new InputDialog(editor, "Command:",
                                        "Do Shell Command On Region", null);
        d.setHistory(new History("doShellCommandOnRegion"));
        editor.centerDialog(d);
        d.show();
        String command = d.getInput();
        if (command == null || command.length() == 0)
            return;
        editor.setWaitCursor();
        Region r = new Region(editor);
        // BUG! We should pass the contents of the region line by line in case
        // it's big.
        ShellCommand shellCommand =
            new ShellCommand(command, null, r.toString());
        shellCommand.run();
        String output = shellCommand.getOutput();
        if (output != null && output.length() > 0) {
            CompoundEdit compoundEdit = editor.beginCompoundEdit();
            editor.deleteRegion();
            editor.addUndo(SimpleEdit.INSERT_STRING);
            editor.insertStringInternal(output);
            editor.moveCaretToDotCol();
            editor.endCompoundEdit(compoundEdit);
            if (editor.getFormatter().parseBuffer())
                editor.getBuffer().repaint();
        }
        editor.setDefaultCursor();
    }
}
