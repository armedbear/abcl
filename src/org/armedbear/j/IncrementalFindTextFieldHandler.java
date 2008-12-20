/*
 * IncrementalFindTextFieldHandler.java
 *
 * Copyright (C) 1998-2005 Peter Graves
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

import java.awt.Color;
import java.awt.event.KeyEvent;
import javax.swing.SwingUtilities;
import javax.swing.undo.CompoundEdit;

public final class IncrementalFindTextFieldHandler extends DefaultTextFieldHandler
    implements Constants
{
    private final Display display;
    private final Buffer buffer;

    private Search search;
    private CompoundEdit edit;
    private boolean inHistory;
    private KeyMap keyMap;

    // Initial state.
    private final Position initialDot;
    private final int initialDotCol;
    private final Position initialMark;

    public IncrementalFindTextFieldHandler(Editor editor, HistoryTextField textField)
    {
        super(editor, textField);
        display = editor.getDisplay();
        buffer = editor.getBuffer();
        search = new Search();
        if (buffer.supportsUndo) {
            edit = new CompoundEdit();
            edit.addEdit(new UndoFold(editor));
            edit.addEdit(new UndoMove(editor));
        }
        initializeKeyMap();
        // Save initial state.
        initialDot = editor.getDotCopy();
        initialDotCol = display.getCaretCol() + display.getShift();
        if (editor.getMark() != null)
            initialMark = editor.getMark().copy();
        else
            initialMark = null;
    }

    public void initializeKeyMap()
    {
        keyMap = new KeyMap();
        keyMap.addMappingsForCommand("incrementalFind", KeyMap.getGlobalKeyMap());
        keyMap.addMappingsForCommand("incrementalFind", buffer.getKeyMapForMode());
        keyMap.addMappingsForCommand("findNext", KeyMap.getGlobalKeyMap());
        keyMap.addMappingsForCommand("findNext", buffer.getKeyMapForMode());
        keyMap.addMappingsForCommand("findPrev", KeyMap.getGlobalKeyMap());
        keyMap.addMappingsForCommand("findPrev", buffer.getKeyMapForMode());
        keyMap.addMappingsForCommand("escape", KeyMap.getGlobalKeyMap());
        keyMap.addMappingsForCommand("escape", buffer.getKeyMapForMode());
    }

    public void escape()
    {
        restoreInitialState();
        editor.ensureActive();
        editor.setFocusToDisplay();
        editor.updateLocation();
        // Erase "not found" message (if any).
        editor.status("");
        unhighlightTextField();
    }

    private void restoreInitialState()
    {
        editor.updateDotLine();
        editor.setDot(initialDot);
        editor.updateDotLine();
        editor.setMark(initialMark);
        if (initialMark != null && initialMark.getLine() != initialDot.getLine())
            editor.setUpdateFlag(REPAINT);
        display.setCaretCol(initialDotCol - display.getShift());
        editor.setUpdateFlag(REFRAME);
        editor.updateDisplay();
    }

    public void keyPressed(KeyEvent e)
    {
        final char keyChar = e.getKeyChar();
        final int keyCode = e.getKeyCode();
        // Mask off the bits we don't care about (Java 1.4).
        final int modifiers = e.getModifiers() & 0x0f;
        switch (keyCode) {
            case KeyEvent.VK_ESCAPE:
                escape();
                e.consume();
                return;
            case KeyEvent.VK_BACK_SPACE:
                backspace();
                e.consume();
                return;
            case KeyEvent.VK_N:
                if (modifiers == CTRL_MASK) {
                    e.consume();
                    retrieveHistory(1);
                    return;
                }
                break;
            case KeyEvent.VK_P:
                if (modifiers == CTRL_MASK) {
                    e.consume();
                    retrieveHistory(-1);
                    return;
                }
                break;
            case KeyEvent.VK_W:
                if (modifiers == CTRL_MASK) {
                    e.consume();
                    yankWord();
                }
                break;
            case KeyEvent.VK_ENTER:
                e.consume(); // Fall through...
            case KeyEvent.VK_LEFT:
            case KeyEvent.VK_KP_LEFT:
            case KeyEvent.VK_RIGHT:
            case KeyEvent.VK_KP_RIGHT:
            case KeyEvent.VK_UP:
            case KeyEvent.VK_KP_UP:
            case KeyEvent.VK_DOWN:
            case KeyEvent.VK_KP_DOWN:
            case KeyEvent.VK_HOME:
            case KeyEvent.VK_END:
                if (modifiers == 0)
                    finish(e);
                return;
            default:
                break;
        }
        KeyMapping mapping = keyMap.lookup(keyChar, keyCode, modifiers);
        if (mapping != null) {
            Object command = mapping.getCommand();
            if (command == "incrementalFind" || command == "findNext") {
                e.consume();
                findNext();
                return;
            }
            if (command == "findPrev") {
                e.consume();
                findPrev();
                return;
            }
            if (command == "escape") {
                // keyboard-quit
                escape();
                e.consume();
                return;
            }
        }
    }

    private void retrieveHistory(int direction)
    {
        History history = textField.getHistory();
        if (history == null)
            return;
        if (!inHistory)
            history.reset();
        String s = direction < 0 ? history.getPrevious() : history.getNext();
        if (s != null && s.length() != 0) {
            inHistory = true;
            search.setPattern(s);
            textField.setText(s);
            textField.setCaretPosition(s.length());
            return;
        }
    }

    private void yankWord()
    {
        inHistory = false;
        int begin = editor.getDotOffset();
        int end = begin + search.getPatternLength();
        final Line dotLine = editor.getDotLine();
        final int limit = dotLine.length();
        final Mode mode = buffer.getMode();
        // Grab non-word chars at end of search pattern.
        while (end < limit && !mode.isIdentifierPart(dotLine.charAt(end)))
            ++end;
        // Grab next word.
        while (end < limit && mode.isIdentifierPart(dotLine.charAt(end)))
            ++end;
        String s = dotLine.substring(begin, end);
        // If the current pattern is all lower case, convert the new pattern
        // to lower case so the search will continue to be case insensitive.
        if (Utilities.isLowerCase(search.getPattern()))
            s = s.toLowerCase();
        if (!s.equals(search.getPattern())) {
            search.setPattern(s);
            editor.markFoundPattern(search);
            editor.updateDisplay();
        }
    }

    private void finish(KeyEvent e)
    {
        unhighlightTextField();
        if (search.getPattern() != null && search.getPatternLength() > 0) {
            editor.setLastSearch(search);
            History history = textField.getHistory();
            if (history != null) {
                history.append(search.getPattern());
                history.save();
            }
            if (edit != null) {
                edit.end();
                buffer.addEdit(edit);
            }
        }
        final int keyCode = e.getKeyCode();
        if (keyCode == KeyEvent.VK_ENTER) {
            String pattern = textField.getText();
            if (pattern == null || pattern.length() == 0) {
                restoreInitialState();
                editor.ensureActive();
                editor.setFocusToDisplay();
                editor.updateLocation();
                FindDialog.find(editor);
                editor.updateDisplay();
                return;
            }
        } else {
            editor.handleJEvent(new JEvent(e));
            // We may need to do a horizontal reframe (home, end).
            editor.updateDisplay();
        }
        editor.ensureActive();
        editor.setFocusToDisplay();
        editor.updateLocation();
    }

    public void keyReleased(KeyEvent e)
    {
        textField.setText(search.getPattern());
        textField.setCaretPosition(search.getPatternLength());
    }

    public void keyTyped(KeyEvent e)
    {
        // Mask off the bits we don't care about (Java 1.4).
        final int modifiers = e.getModifiers() & 0x0f;
        if (modifiers == 0 || modifiers == SHIFT_MASK)
            handleKeyEvent(e);
    }

    private void handleKeyEvent(KeyEvent e)
    {
        // Mask off bits we don't care about (Java 1.4).
        int modifiers = e.getModifiers() & 0x0f;
        if (modifiers != 0 && modifiers != KeyEvent.SHIFT_MASK)
            return;
        char c = e.getKeyChar();
        if (c == KeyEvent.CHAR_UNDEFINED)
            return;
        if (c == 8 || c == 10 || c == 13) {
            e.consume();
            return;
        }
        inHistory = false;
        search.appendCharToPattern(c);
        // Convention is to ignore case unless pattern is mixed case or
        // all caps.
        search.setIgnoreCase(Utilities.isLowerCase(search.getPattern()));
        Position pos = search.findString(buffer, editor.getDot(), true);
        if (pos != null)
            found(pos);
        else {
            search.notFound(editor);
            highlightTextField();
        }
    }

    private void backspace()
    {
        String s = textField.getText();
        if (s.length() > 0) {
            s = s.substring(0, s.length()-1);
            textField.setText(s);
            search.setPattern(s);
            search.setIgnoreCase(Utilities.isLowerCase(s));
            if (s.length() > 0) {
                Position pos = search.findString(buffer, initialDot, true);
                if (pos != null) {
                    found(pos);
                    // Erase "not found" message (if any).
                    editor.status("");
                    unhighlightTextField();
                } else
                    search.notFound(editor);
            } else
                restoreInitialState();
        }
    }

    private void findNext()
    {
        if (search.getPatternLength() > 0) {
            // Only advance dot if we're really searching for the same pattern
            // again. We might be doing the first search on a pattern
            // retrieved from history.
            if (!inHistory) {
                if (editor.getDotOffset() < editor.getDotLine().length())
                    editor.getDot().skip(1);
                else if (editor.getDotLine().next() != null)
                    editor.setDot(editor.getDotLine().next(), 0);
                else
                    // Wrap buffer.
                    editor.setDot(buffer.getFirstLine(), 0);
            }
        } else {
            // Recall last pattern.
            History history = textField.getHistory();
            if (history != null) {
                history.reset();
                String s = history.getPrevious();
                if (s == null)
                    s = "";
                search.setPattern(s);
                if (s.length() == 0)
                    return;
                textField.setText(s);
                textField.setCaretPosition(s.length());
            }
        }
        inHistory = false;
        // Convention is to ignore case unless pattern is mixed case or all
        // caps.
        search.setIgnoreCase(Utilities.isLowerCase(search.getPattern()));
        Position pos = search.findString(buffer, editor.getDot(), true);
        if (pos != null)
            found(pos);
    }

    private void findPrev()
    {
        if (search.getPatternLength() > 0) {
            Position start;
            if (editor.getMark() != null)
                start = new Region(editor).getBegin();
            else
                start = editor.getDotCopy();
            boolean wrapped = false;
            if (!start.prev()) {
                start = buffer.getEnd();
                wrapped = true;
            }
            Position pos = search.reverseFindString(buffer, start);
            if (pos == null && !wrapped)
                pos = search.reverseFindString(buffer, buffer.getEnd());
            if (pos != null)
                found(pos);
        }
    }

    private boolean dirty;

    private void found(Position pos)
    {
        if (editor.getMark() != null)
            if (editor.getMarkLine() != editor.getDotLine())
                editor.setUpdateFlag(REPAINT);
        editor.updateDotLine();
        editor.getDot().moveTo(pos);
        editor.updateDotLine();
        editor.setUpdateFlag(REFRAME);
        dirty = true;
        SwingUtilities.invokeLater(foundRunnable);
    }

    private final Runnable foundRunnable = new Runnable() {
        public void run()
        {
            if (dirty) {
                editor.markFoundPattern(search);
                editor.updateDisplay();
                dirty = false;
            }
        }
    };

    private final void highlightTextField()
    {
        textField.setBackground(Color.RED);
    }

    private final void unhighlightTextField()
    {
        textField.setBackground(Color.WHITE);
    }
}
