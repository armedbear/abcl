/*
 * AliasDialog.java
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

import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;

public final class AliasDialog extends AbstractDialog implements FocusListener
{
    private final Editor editor;
    private final HistoryTextField keyTextField;
    private final HistoryTextField valueTextField;

    public AliasDialog(Editor editor)
    {
        this(editor, null);
    }

    public AliasDialog(Editor editor, String key)
    {
        super(editor.getFrame(), "Alias", true);
        this.editor = editor;
        keyTextField = new HistoryTextField(20);
        valueTextField = new HistoryTextField(20);
        Label label = new Label("Key:");
        label.setDisplayedMnemonic('K');
        addLabelAndTextField(label, keyTextField);
        addVerticalStrut();
        label = new Label("Value:");
        label.setDisplayedMnemonic('V');
        addLabelAndTextField(label, valueTextField);
        addVerticalStrut();
        addOKCancel();
        pack();
        keyTextField.addFocusListener(this);
        if (key != null) {
            keyTextField.setText(key);
            String value = editor.getAlias(key);
            if (value != null)
                valueTextField.setText(value);
            valueTextField.requestFocus();
        } else
            keyTextField.requestFocus();
    }

    protected void ok()
    {
        dispose();
        String key = getKey();
        if (Aliases.isSystemAlias(key))
            MessageDialog.showMessageDialog(editor, "System aliases cannot be changed.", "Error");
        else {
            String value = getValue();
            if (value.length() == 0)
                editor.removeAlias(key);
            else if (value.equals("here"))
                editor.setAliasForBuffer(key, editor.getBuffer());
            else
                editor.setAlias(key, value);
        }
    }

    private final String getKey()
    {
        final String key = keyTextField.getText();
        return key != null ? key.trim() : "";
    }

    private final String getValue()
    {
        final String value = valueTextField.getText();
        return value != null ? value.trim() : "";
    }

    public void focusGained(FocusEvent e)
    {
    }

    public void focusLost(FocusEvent e)
    {
        String value = editor.getAlias(getKey());
        if (value != null)
            valueTextField.setText(value);
    }

    public static void alias()
    {
        final Editor editor = Editor.currentEditor();
        AliasDialog d = new AliasDialog(editor);
        editor.centerDialog(d);
        d.show();
    }

    public static void alias(String args)
    {
        final Editor editor = Editor.currentEditor();
        args = args.trim();
        final FastStringBuffer sb = new FastStringBuffer();
        int i = 0;
        for (; i < args.length(); i++) {
            char c = args.charAt(i);
            if (Character.isWhitespace(c))
                break;
            else if (c == '=')
                break;
            else
                sb.append(c);
        }
        if (sb.length() == 0)
            return;
        final String key = sb.toString();
        boolean seenEquals = false;
        for (; i < args.length(); i++) {
            char c = args.charAt(i);
            if (Character.isWhitespace(c))
                continue;
            if (c == '=' && !seenEquals)
                seenEquals = true;
            else
                break;
        }
        sb.setLength(0);
        for (; i < args.length(); i++)
            sb.append(args.charAt(i));
        String value = sb.toString();
        if (value.length() == 0) {
            if (seenEquals) {
                // Unset alias.
                if (Aliases.isSystemAlias(key))
                    MessageDialog.showMessageDialog(editor, "System aliases cannot be changed.", "Error");
                else
                    editor.removeAlias(key);
            } else {
                AliasDialog d = new AliasDialog(editor, key);
                editor.centerDialog(d);
                d.show();
            }
        } else {
            if (Aliases.isSystemAlias(key))
                MessageDialog.showMessageDialog(editor, "System aliases cannot be changed.", "Error");
            else if (value.equals("here"))
                editor.setAliasForBuffer(key, editor.getBuffer());
            else
                editor.setAlias(key, value);
        }
    }
}
