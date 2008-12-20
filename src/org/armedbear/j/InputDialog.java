/*
 * InputDialog.java
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

import java.awt.BorderLayout;
import java.awt.event.InputEvent;
import java.awt.event.KeyEvent;
import java.awt.event.KeyListener;
import java.util.List;
import javax.swing.BoxLayout;
import javax.swing.JDialog;
import javax.swing.JPanel;
import javax.swing.border.EmptyBorder;

public class InputDialog extends JDialog implements KeyListener
{
    protected final Editor editor;

    protected HistoryTextField textField;

    private String defaultValue;
    private History history;
    private String input;
    private List completions;
    private int index;

    public InputDialog(Editor editor, String prompt, String title,
        String defaultValue)
    {
        super(editor.getFrame(), title, true);
        this.editor = editor;
        this.defaultValue = defaultValue;
        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.Y_AXIS));
        panel.setBorder(new EmptyBorder(5, 5, 5, 5));
        panel.add(new Label(prompt));
        textField = new HistoryTextField(20);
        textField.addKeyListener(this);
        panel.add(textField);
        getContentPane().add(panel, BorderLayout.CENTER);
        pack();
        textField.setFocusTraversalKeysEnabled(false);
    }

    public static String showInputDialog(Editor editor, String prompt,
        String title, String defaultValue)
    {
        InputDialog d = new InputDialog(editor, prompt, title, defaultValue);
        editor.centerDialog(d);
        d.show();
        return d.input;
    }

    public static String showInputDialog(Editor editor, String prompt,
        String title)
    {
        return showInputDialog(editor, prompt, title, null);
    }

    public void show()
    {
        if (defaultValue != null && defaultValue.length() > 0) {
            textField.setText(defaultValue);
            textField.selectAll();
        }
        textField.requestFocus();
        super.show();
    }

    public final void setDefaultValue(String s)
    {
        defaultValue = s;
    }

    public final String getInput()
    {
        return input;
    }

    public void setHistory(History history)
    {
        this.history = history;
        textField.setHistory(history);
    }

    protected void enter()
    {
        input = textField.getText();
        if (history != null) {
            history.append(input);
            history.save();
        }
        dispose();
    }

    protected void escape()
    {
        input = null;
        dispose();
    }

    public void keyPressed(KeyEvent e)
    {
        final int keyCode = e.getKeyCode();
        final int modifiers = e.getModifiers();
        switch (keyCode) {
            case KeyEvent.VK_TAB: {
                String s = null;
                if (modifiers == InputEvent.SHIFT_MASK)
                    s = previousGuess();
                else
                    s = guess(textField.getText());
                e.consume();
                if (s != null) {
                    textField.setText(s);
                    textField.setCaretPosition(s.length());
                }
                return;
            }
            case KeyEvent.VK_ENTER:
                enter();
                return;
            case KeyEvent.VK_ESCAPE:
                escape();
                return;
            case KeyEvent.VK_SHIFT:
            case KeyEvent.VK_META:
            case KeyEvent.VK_ALT:
                // Ignore modifers.
                return;
            default:
                // Anything but tab, start over.
                completions = null;
                return;
        }
    }

    public void keyReleased(KeyEvent e) {}

    public void keyTyped(KeyEvent e) {}

    public void dispose()
    {
        super.dispose();
        editor.restoreFocus();
    }

    private String guess(String prefix)
    {
        if (completions == null) {
            completions = getCompletions(prefix);
            if (completions == null)
                return null;
            index = 0;
        } else if (index >= completions.size())
            index = 0; // Start over.
        if (index < completions.size())
            return (String) completions.get(index++);
        return null;
    }

    private String previousGuess()
    {
        if (completions != null) {
            if (completions.size() > 1) {
                index -= 2;
                if (index < 0)
                    index += completions.size();
                return (String) completions.get(index++);
            }
        }
        return null;
    }

    // Derived classes can override this method to provide completion
    // functionality.
    protected List getCompletions(String prefix)
    {
        return null;
    }
}
