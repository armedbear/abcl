/*
 * HistoryTextField.java
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

import java.awt.Dimension;
import java.awt.Font;
import java.awt.Graphics;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.awt.event.TextListener;
import javax.swing.JTextField;
import javax.swing.SwingUtilities;

public class HistoryTextField extends JTextField implements FocusListener,
    MouseListener
{
    private History history;

    private Object owner;

    // Only one text listener is supported.
    private TextListener textListener;

    protected TextFieldHandler handler;

    public HistoryTextField(Editor editor, int columns)
    {
        super(columns);
        final Preferences preferences = Editor.preferences();
        final String fontName =
            preferences.getStringProperty(Property.TEXT_FIELD_FONT_NAME);
        if (fontName != null) {
            int fontSize =
                preferences.getIntegerProperty(Property.TEXT_FIELD_FONT_SIZE);
            if (fontSize == 0)
                fontSize =
                    preferences.getIntegerProperty(Property.DIALOG_FONT_SIZE);
            setFont(new Font(fontName, Font.PLAIN, fontSize));
        }
        setAlignmentX(LEFT_ALIGNMENT);
        setHandler(new DefaultTextFieldHandler(editor, this));
        addFocusListener(this);
        addMouseListener(this);
    }

    public HistoryTextField(int columns)
    {
        this(Editor.currentEditor(), columns);
    }

    public Dimension getPreferredSize() {
        Dimension size = super.getPreferredSize();
        size.width = getColumns() * 11;
        return size;
    }

    public final Object getOwner()
    {
        return owner;
    }

    public final void setOwner(Object owner)
    {
        this.owner = owner;
    }

    public void addTextListener(TextListener textListener)
    {
        Debug.assertTrue(this.textListener == null);
        this.textListener = textListener;
    }

    public final TextListener getTextListener()
    {
        return textListener;
    }

    public final TextFieldHandler getHandler()
    {
        return handler;
    }

    public final void setHandler(TextFieldHandler handler)
    {
        Debug.assertTrue(handler != null);
        if (this.handler != null)
            removeKeyListener(this.handler);
        this.handler = handler;
        addKeyListener(handler);
    }

    public void setHistory(History history)
    {
        this.history = history;
        resetHistory();
    }

    public final History getHistory()
    {
        return history;
    }

    public final void resetHistory()
    {
        if (history != null)
            history.reset();
    }

    public String getText()
    {
        String s = super.getText();
        int length = s.length();
        FastStringBuffer sb = new FastStringBuffer(length);
        // Copy string, stripping control characters if any.
        for (int i = 0; i < length; i++) {
            char c = s.charAt(i);
            if (!Character.isISOControl(c))
                sb.append(c);
        }
        return sb.toString();
    }

    public void previousHistory()
    {
        if (history != null) {
            final String text = super.getText();
            while (true) {
                String s = history.getPrevious();
                if (s == null)
                    break;
                if (!s.equals(text)) {
                    setText(s);
                    selectAll();
                    break;
                }
            }
        }
    }

    public void nextHistory()
    {
        if (history != null) {
            final String text = super.getText();
            while (true) {
                String s = history.getNext();
                if (s == null)
                    break;
                if (!s.equals(text)) {
                    setText(s);
                    selectAll();
                    break;
                }
            }
        }
    }

    public void recallLast()
    {
        if (history != null) {
            String s = history.getPrevious();
            setText(s != null ? s : "");
        }
    }

    public void paintComponent(Graphics g)
    {
        Display.setRenderingHints(g);
        super.paintComponent(g);
    }

    public void focusGained(FocusEvent e)
    {
        selectAll();
    }

    public void focusLost(FocusEvent e)
    {
        int length = getText().length();
        select(length, length);
    }

    public void mouseClicked(MouseEvent e) {}

    public void mouseEntered(MouseEvent e) {}

    public void mouseExited(MouseEvent e) {}

    public void mousePressed(MouseEvent e) {}

    public void mouseReleased(MouseEvent e)
    {
        final int dot = getCaretPosition();
        final int mark = getCaret().getMark();
        Runnable r = new Runnable() {
            public void run()
            {
                setCaretPosition(mark);
                moveCaretPosition(dot);
            }
        };
        SwingUtilities.invokeLater(r);
    }
}
