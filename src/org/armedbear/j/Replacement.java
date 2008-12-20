/*
 * Replacement.java
 *
 * Copyright (C) 1998-2002 Peter Graves
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

import gnu.regexp.REMatch;

public class Replacement extends Search
{
    private Editor editor;

    // This is the replacement string entered by the user. If we're working
    // with regular expressions, this string may contain one or more
    // occurrences of "\\", "\&", or "\D", where D is a digit from 1 to 9
    // (inclusive).
    private String replaceWith;

    private boolean confirmChanges = true;
    private int replacementCount;

    public Replacement(Editor editor)
    {
        super();
        this.editor = editor;
    }

    public final Editor getEditor()
    {
        return editor;
    }

    public final String getReplaceWith()
    {
        return replaceWith;
    }

    public final void setReplaceWith(String s)
    {
        replaceWith = s;
    }

    public final boolean confirmChanges()
    {
        return confirmChanges;
    }

    public final void setConfirmChanges(boolean b)
    {
        confirmChanges = b;
    }

    public final int getReplacementCount()
    {
        return replacementCount;
    }

    public void replaceOccurrence()
    {
        final Buffer buffer = editor.getBuffer();
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            final Line dotLine = editor.getDotLine();
            final int dotOffset = editor.getDotOffset();
            if (isMultilinePattern()) {
                Debug.assertTrue(isRegularExpression());
                Debug.assertTrue(editor.getMark() != null);
                Debug.assertTrue(editor.getDot().isBefore(editor.getMark()));
                Debug.assertTrue(getMatch() != null);
                final Region region = getRegion();
                int end = -1;
                if (region != null) {
                    // Restrict to selection.
                    end = buffer.getAbsoluteOffset(region.getEnd());
                }
                Region r = new Region(editor);
                editor.addUndoDeleteRegion(r);
                // Sets buffer modified flag.
                r.delete();
                final String toBeReplaced = getMatch().toString();
                final String toBeInserted = getReplacementText(toBeReplaced);
                editor.addUndo(SimpleEdit.INSERT_STRING);
                buffer.insertString(editor.getDot(), toBeInserted);
                if (region != null) {
                    Debug.assertTrue(end >= 0);
                    end -= toBeReplaced.length();
                    end += toBeInserted.length();
                    Debug.assertTrue(end > buffer.getAbsoluteOffset(region.getBegin()));
                    Position endPos = buffer.getPosition(end);
                    Debug.assertTrue(endPos != null);
                    region.setEnd(endPos);
                }
                editor.getBuffer().repaint();
            } else {
                editor.addUndo(SimpleEdit.LINE_EDIT);
                editor.setMark(null);
                String head = dotLine.substring(0, dotOffset);
                String toBeReplaced;
                if (isRegularExpression()) {
                    Debug.assertTrue(getMatch() != null);
                    toBeReplaced = getMatch().toString();
                } else {
                    toBeReplaced = dotLine.substring(dotOffset,
                        dotOffset + getPatternLength());
                }
                String tail = dotLine.substring(dotOffset + toBeReplaced.length());
                String toBeInserted = getReplacementText(toBeReplaced);
                FastStringBuffer sb = new FastStringBuffer(head);
                sb.append(toBeInserted);
                sb.append(tail);
                dotLine.setText(sb.toString());
                Region region = getRegion();
                if (region != null && dotLine == region.getEndLine())
                    if (dotOffset + toBeReplaced.length() <= region.getEndOffset())
                        region.setEndOffset(region.getEndOffset() + toBeInserted.length() - toBeReplaced.length());
                editor.getDot().skip(toBeInserted.length());
                Editor.updateInAllEditors(dotLine);
                buffer.modified();
            }
            ++replacementCount;
        }
        finally {
            buffer.unlockWrite();
        }
    }

    // No editor, no region, no undo, does not set buffer's modified flag.
    // Used by replace in files when not confirming changes or keeping
    // modified buffers.
    public void replaceOccurrence(Position pos)
    {
        final Line line = pos.getLine();
        final int offset = pos.getOffset();
        final String head = line.substring(0, offset);
        String toBeReplaced;
        if (isRegularExpression()) {
            Debug.assertTrue(getMatch() != null);
            toBeReplaced = getMatch().toString();
        } else
            toBeReplaced = line.substring(offset, offset + getPatternLength());
        final String tail = line.substring(offset + toBeReplaced.length());
        final String toBeInserted = getReplacementText(toBeReplaced);
        FastStringBuffer sb = new FastStringBuffer(head);
        sb.append(toBeInserted);
        sb.append(tail);
        line.setText(sb.toString());
        pos.skip(toBeInserted.length());
        ++replacementCount;
    }

    private String getReplacementText(String toBeReplaced)
    {
        String replacementText;
        if (isRegularExpression()) {
            // Perform regular expression variable substitution as necessary.
            replacementText = substituteInto(getMatch(), replaceWith);
        } else {
            // We're not doing regular expressions. Use the string entered by
            // the user, verbatim.
            replacementText = replaceWith;
        }
        // Handle case conversion if appropriate.
        if (ignoreCase() && toBeReplaced.length() > 0 && replacementText.length() > 0) {
            if (Utilities.isUpperCase(toBeReplaced)) {
                // The string to be replaced is all upper case. Make the
                // replacement all upper case too.
                replacementText = replacementText.toUpperCase();
            } else {
                char c = toBeReplaced.charAt(0);
                if (Character.isUpperCase(c)) {
                    // The string to be replaced begins with an upper case
                    // letter. Make the replacement begin with an upper case
                    // letter too.
                    c = replacementText.charAt(0);
                    if (Character.isLowerCase(c)) {
                        FastStringBuffer sb = new FastStringBuffer(replacementText);
                        sb.setCharAt(0, Character.toUpperCase(c));
                        replacementText = sb.toString();
                    }
                }
            }
        }
        return replacementText;
    }

    private String substituteInto(REMatch match, String input)
    {
        FastStringBuffer sb = new FastStringBuffer();
        int i;
        for (i = 0; i < input.length()-1; i++) {
            char c = input.charAt(i);
            if (c == '\\') {
                c = input.charAt(++i);
                if (c == '&') {
                    sb.append(match.toString());
                } else if (c >= '1' && c <= '9') {
                    int val = Character.digit(c, 10);
                    sb.append(match.toString(val));
                } else if (isMultilinePattern() && c == 'n') {
                    sb.append('\n');
                } else {
                     // Escape everything else.
                    sb.append(c);
                }
            } else
                sb.append(c);
        }

        if (i < input.length())
            sb.append(input.charAt(i));

        return sb.toString();
    }
}
