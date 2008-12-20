/*
 * Expansion.java
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

import java.util.ArrayList;
import java.util.List;
import javax.swing.undo.CompoundEdit;

public class Expansion implements Constants
{
    protected String prefix;
    protected int prefixOffset; // Offset of prefix on current line.

    protected List candidates;
    protected int last = -1;

    private static Expansion lastExpansion;

    protected Position savedDot;
    protected String savedText;

    private final Mode mode;

    private String current;
    private boolean forceLowerCase;

    // For MailAddressExpansion.
    protected Expansion()
    {
        mode = Editor.getModeList().getMode(PLAIN_TEXT_MODE);
    }

    public Expansion(Position dot, Mode mode)
    {
        savedDot = new Position(dot);
        this.mode = mode;
        final Line dotLine = dot.getLine();
        final int dotOffset = dot.getOffset();
        savedText = dotLine.getText();
        // Get word before caret.
        int begin = dotOffset - 1;
        if (begin < 0)
            return;
        final int end = dotOffset;
        while (begin > 0 && mode.isIdentifierPart(dotLine.charAt(begin)))
            --begin;
        if (!mode.isIdentifierPart(dotLine.charAt(begin)))
            ++begin;
        if (begin == end)
            return;
        prefix = dotLine.substring(begin, end);
        prefixOffset = begin;
        Position pos = new Position(dotLine, begin);
        current = pos.getIdentifier(mode);
        int modeId = mode.getId();
        if (modeId == LISP_MODE || modeId == LISP_SHELL_MODE)
            if (Utilities.isLowerCase(prefix))
                forceLowerCase = true;
        candidates = list(pos);
    }

    public Expansion(Buffer buffer, String prefix, String current)
    {
        this(buffer, prefix, current, null);
    }

    public Expansion(Buffer buffer, String prefix, String current, Position pos)
    {
        mode = buffer.getMode();
        this.prefix = prefix;
        this.current = current;
        if (buffer.getFirstLine() == null)
            return;
        if (pos == null)
            pos = new Position(buffer.getFirstLine(), 0);
        candidates = list(pos);
    }

    private List list(Position pos)
    {
        List list = new ArrayList();
        if (prefix != null) {
            final boolean ignoreCase = Utilities.isLowerCase(prefix);
            String s = null;
            // Search backwards on current line.
            Line line = pos.getLine();
            final int begin = pos.getOffset();
            int index = begin - 1;
            if (index >= 0) {
                s = line.substring(0, begin);
                if (ignoreCase)
                    s = s.toLowerCase();
                while ((index = s.lastIndexOf(prefix, index)) >= 0) {
                    maybeAddCandidate(list, new Position(line, index));
                    --index;
                }
            }
            // Search backwards to start of buffer.
            for (line = pos.getLine().previous(); line != null; line = line.previous()) {
                index = line.length();
                s = ignoreCase ? line.getText().toLowerCase() : line.getText();
                while ((index = s.lastIndexOf(prefix, index)) >= 0) {
                    maybeAddCandidate(list, new Position(line, index));
                    --index;
                }
            }
            // Search forwards from current line to end of buffer. Search current
            // line again to pick up possible matches to right of dot.
            for (line = pos.getLine(); line != null; line = line.next()) {
                index = 0;
                s = ignoreCase ? line.getText().toLowerCase() : line.getText();
                while ((index = s.indexOf(prefix, index)) >= 0) {
                    maybeAddCandidate(list, new Position(line, index));
                    ++index;
                }
            }
        }
        return list;
    }

    private void maybeAddCandidate(List list, Position where)
    {
        final Line line = where.getLine();
        final int offset = where.getOffset();
        if (offset == 0 || !mode.isIdentifierPart(line.charAt(offset-1))) {
            final String candidate = where.getIdentifier(mode);
            maybeAddCandidate(list, candidate);
        }
    }

    private void maybeAddCandidate(List list, String candidate)
    {
        // We don't want what we started with.
        if (candidate.equals(current))
            return;
        if (forceLowerCase)
            candidate = candidate.toLowerCase();
        for (int i = list.size(); i-- > 0;) {
            if (candidate.equals(list.get(i))) {
                // It's already in the list.
                return;
            }
        }
        // Not found.
        list.add(candidate);
    }

    public void appendCandidates(List list)
    {
        final int size = list.size();
        for (int i = 0; i < size; i++)
            maybeAddCandidate(candidates, (String)list.get(i));
    }

    public String getNextCandidate()
    {
        if (candidates == null || candidates.size() == 0)
            return null;
        int index = last + 1;
        if (index == candidates.size())
            index = 0;
        last = index;
        return (String) candidates.get(index);
    }

    private final int getPrefixOffset()
    {
        return prefixOffset;
    }

    public final String getPrefix()
    {
        return prefix;
    }

    public final String getCurrent()
    {
        return current;
    }

    public final List getCandidates()
    {
        return candidates;
    }

    public void undo(Editor editor)
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
            editor.addUndo(SimpleEdit.LINE_EDIT);
            editor.getDotLine().setText(savedText);
            editor.getBuffer().modified();
            editor.getDot().moveTo(savedDot);
            editor.getDisplay().moveCaretToDotCol();
            Editor.updateInAllEditors(editor.getDotLine());
        }
        finally {
            buffer.unlockWrite();
        }
    }

    public static Expansion getLastExpansion()
    {
        return lastExpansion;
    }

    public static void setLastExpansion(Expansion expansion)
    {
        lastExpansion = expansion;
    }

    public static void expand()
    {
        final Editor editor = Editor.currentEditor();
        if (editor.getLastCommand() == COMMAND_EXPAND)
            expand(editor, Expansion.getLastExpansion(), true);
        else {
            Expansion e = editor.getBuffer().getExpansion(editor.getDot());
            Expansion.setLastExpansion(e);
            expand(editor, e, false);
        }
    }

    private static void expand(Editor editor, Expansion expansion, boolean again)
    {
        final Buffer buffer = editor.getBuffer();
        final String candidate = expansion.getNextCandidate();
        if (candidate == null)
            return;
        try {
            buffer.lockWrite();
        }
        catch (InterruptedException e) {
            Log.error(e);
            return;
        }
        try {
            if (again)
                editor.undo();

            CompoundEdit compoundEdit = buffer.beginCompoundEdit();
            editor.addUndo(SimpleEdit.MOVE);
            editor.getDot().setOffset(expansion.getPrefixOffset());

            // Remove prefix from line.
            final Line line = editor.getDotLine();
            final int offset = editor.getDotOffset();
            String head = line.substring(0, offset);
            String tail = line.substring(offset + expansion.getPrefix().length());
            editor.addUndo(SimpleEdit.LINE_EDIT);
            line.setText(head.concat(tail));

            editor.addUndo(SimpleEdit.INSERT_STRING);
            editor.insertStringInternal(candidate);

            editor.moveCaretToDotCol();
            buffer.endCompoundEdit(compoundEdit);
            Editor.updateInAllEditors(line);
            editor.setCurrentCommand(COMMAND_EXPAND);
        }
        finally {
            buffer.unlockWrite();
        }
    }
}
