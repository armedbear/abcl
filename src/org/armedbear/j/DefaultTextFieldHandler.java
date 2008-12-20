/*
 * DefaultTextFieldHandler.java
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
import java.awt.Container;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.KeyEvent;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;
import java.util.List;
import javax.swing.JDialog;
import javax.swing.JMenuItem;
import javax.swing.JPopupMenu;

public class DefaultTextFieldHandler implements Constants, TextFieldHandler
{
    protected final Editor editor;
    protected final HistoryTextField textField;

    protected List completions;
    protected int index;

    private Expansion expansion;
    private String savedText;
    private String head;

    public DefaultTextFieldHandler(Editor editor, HistoryTextField textField)
    {
        this.editor = editor;
        Debug.assertTrue(editor != null);
        this.textField = textField;
    }

    public DefaultTextFieldHandler(HistoryTextField textField)
    {
        this(Editor.currentEditor(), textField);
    }

    public void enter()
    {
    }

    public void escape()
    {
        Container c = textField.getParent();
        while (true) {
            if (c instanceof JDialog)
                return;
            if (c == null)
                break;
            c = c.getParent();
        }
        // Text field is not in a dialog box. We must be dealing with the
        // location bar.
        Debug.assertTrue(editor != null);
        editor.ensureActive();
        editor.setFocusToDisplay();
        editor.updateLocation();
    }

    public boolean wantTab()
    {
        return false;
    }

    public void tab()
    {
        if (textField != null) {
            String prefix = textField.getText();
            String s = getCompletion(prefix);
            if (s != null && !s.equals(prefix)) {
                textField.setText(s);
                textField.setCaretPosition(s.length());
            }
        }
    }

    public void shiftTab()
    {
        if (textField != null) {
            String s = getPreviousCompletion();
            if (s != null) {
                String text = textField.getText();
                if (!s.equals(text)) {
                    textField.setText(s);
                    textField.setCaretPosition(s.length());
                }
            }
        }
    }

    public void resetCompletions()
    {
        completions = null;
    }

    protected String getCompletion(String prefix)
    {
        if (completions == null) {
            completions = getCompletions(prefix);
            index = 0;
        }
        if (completions == null || completions.size() == 0)
            return null;
        if (index >= completions.size())
            index = 0;
        return (String) completions.get(index++);
    }

    private String getPreviousCompletion()
    {
        if (completions != null && completions.size() > 1) {
            index -= 2;
            if (index < 0)
                index += completions.size();
            return (String) completions.get(index++);
        }
        return null;
    }

    public List getCompletions(String prefix)
    {
        return null;
    }

    private void killLine()
    {
        textField.setText(textField.getText().substring(0, textField.getCaretPosition()));
    }

    private void expand()
    {
        if (expansion == null) {
            // New expansion.
            savedText = textField.getText();
            int index = savedText.lastIndexOf(' ');
            if (index >= 0) {
                head = savedText.substring(0, index+1);
                expansion = textField.getHandler().getExpansion(savedText.substring(index+1));
            } else {
                Debug.assertTrue(head == null);
                expansion = textField.getHandler().getExpansion(savedText);
            }
        }
        final String candidate = expansion.getNextCandidate();
        if (candidate != null) {
            if (head != null)
                textField.setText(head.concat(candidate));
            else
                textField.setText(candidate);
        }
    }

    public void resetExpansion()
    {
        expansion = null;
        savedText = null;
        head = null;
    }

    public Expansion getExpansion(String prefix)
    {
        return new Expansion(editor.getBuffer(), prefix, prefix);
    }

    protected void reset()
    {
        textField.resetHistory();
        textField.getHandler().resetCompletions();
    }

    public void keyPressed(KeyEvent e)
    {
        TextFieldHandler handler = textField.getHandler();
        if (handler == null)
            return;
        if (handler != this)
            Debug.bug();
        final char keyChar = e.getKeyChar();
        final int keyCode = e.getKeyCode();
        final int modifiers = e.getModifiers();
        switch (keyCode) {
            case KeyEvent.VK_ENTER:
                resetExpansion();
                // Make sure user can see what he typed.
                textField.paintImmediately(0, 0,
                                           textField.getWidth(),
                                           textField.getHeight());
                e.consume();
                handler.enter();
                return;
            case KeyEvent.VK_ESCAPE:
                if (expansion != null) {
                    // Cancel expansion.
                    textField.setText(savedText);
                    resetExpansion();
                    // Consume key event so parent will ignore it.
                    e.consume();
                } else
                    handler.escape();
                return;
            case KeyEvent.VK_TAB:
                resetExpansion();
                if (handler.wantTab()) {
                    if (modifiers == 0) {
                        e.consume();
                        handler.tab();
                    } else if (modifiers == SHIFT_MASK) {
                        e.consume();
                        handler.shiftTab();
                    }
                }
                return;
            case KeyEvent.VK_UP:
            case KeyEvent.VK_KP_UP:
                resetExpansion();
                textField.previousHistory();
                return;
            case KeyEvent.VK_P:
                resetExpansion();
                if (modifiers == CTRL_MASK)
                    textField.previousHistory();
                else
                    reset();
                return;
            case KeyEvent.VK_DOWN:
            case KeyEvent.VK_KP_DOWN:
                resetExpansion();
                if (modifiers == ALT_MASK)
                    showPopup();
                else
                    textField.nextHistory();
                return;
            case KeyEvent.VK_N:
                resetExpansion();
                if (modifiers == CTRL_MASK)
                    textField.nextHistory();
                else
                    reset();
                return;
            case KeyEvent.VK_SHIFT:
            case KeyEvent.VK_CONTROL:
            case KeyEvent.VK_META:
            case KeyEvent.VK_ALT:
                return;
            default:
                reset();
                break;
        }
        KeyMapping mapping = editor.getKeyMapping(keyChar, keyCode, modifiers);
        if (mapping != null) {
            Object command = mapping.getCommand();
            if (command == "killLine")
                killLine();
            else if (command == "expand") {
                expand();
                return; // Don't call resetExpansion()!
            } else if (command == "escape") {
                // keyboard-quit
                if (expansion != null) {
                    // Cancel expansion.
                    textField.setText(savedText);
                    resetExpansion();
                    // Consume key event so parent will ignore it.
                    e.consume();
                } else
                    handler.escape();
                return;
            }
        }
        resetExpansion();
    }

    public void keyReleased(KeyEvent e)
    {
        TextListener textListener = textField.getTextListener();
        if (textListener != null)
            textListener.textValueChanged(new TextEvent(this, TextEvent.TEXT_VALUE_CHANGED));
    }

    public void keyTyped(KeyEvent e) {}

    private void showPopup()
    {
        if (textField == null)
            return;
        History history = textField.getHistory();
        if (history == null)
            return;
        final String existing = textField.getText();
        JPopupMenu popup = null;
        for (int i = history.size(); i-- > 0;) {
            String s = history.get(i);
            if (s.equals(existing))
                continue;
            if (popup == null)
                popup = new JPopupMenu();
            JMenuItem menuItem = new JMenuItem();
            menuItem.setText(history.get(i));
            menuItem.setActionCommand(s);
            menuItem.addActionListener(popupActionListener);
            popup.add(menuItem);
        }
        if (popup != null)
            popup.show(textField, 0, textField.getHeight());
    }

    private ActionListener popupActionListener = new ActionListener() {
        public void actionPerformed(ActionEvent e)
        {
            textField.setText(e.getActionCommand());
            enter();
        }
    };
}
