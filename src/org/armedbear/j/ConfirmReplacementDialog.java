/*
 * ConfirmReplacementDialog.java
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

import java.awt.Dimension;
import java.awt.Point;
import java.awt.Rectangle;
import java.awt.event.ActionEvent;
import java.awt.event.KeyEvent;
import javax.swing.JPanel;

public final class ConfirmReplacementDialog extends AbstractDialog
{
    private static Rectangle rect;

    private Editor editor;
    private Replacement replacement;
    private boolean inFiles;

    public ConfirmReplacementDialog(Replacement replacement, boolean inFiles)
    {
        super(Editor.currentEditor(), "Confirm Replacement", true);
        this.replacement = replacement;
        editor = replacement.getEditor();
        this.inFiles = inFiles;
        JPanel textPanel = new JPanel();
        textPanel.setAlignmentX(LEFT_ALIGNMENT);
        textPanel.add(new Label("Replace this occurrence?"));
        mainPanel.add(textPanel);
        JPanel buttonBox = new JPanel();
        buttonBox.setAlignmentX(LEFT_ALIGNMENT);
        buttonBox.add(createButton("Yes", 'Y'));
        buttonBox.add(createButton("No", 'N'));
        buttonBox.add(createButton("Cancel", 'C'));
        if (inFiles)
            buttonBox.add(createButton("Skip File", 'S'));
        buttonBox.add(createButton("Replace All", 'R'));
        mainPanel.add(buttonBox);
        getContentPane().add(mainPanel);
        pack();
        if (rect != null)
            setBounds(rect);
        else {
            // Center dialog at top of editor window.
            Dimension dimParent = editor.getFrame().getSize();
            Dimension dimDialog = getSize();
            Point p = editor.getFrame().getLocation();
            p.translate((dimParent.width - dimDialog.width) / 2, 0);
            setLocation(p);
        }
    }

    private StandardButton createButton(String text, char mnemonic)
    {
        StandardButton button = new StandardButton(text);
        button.setMnemonic(mnemonic);
        button.addActionListener(this);
        button.addKeyListener(this);
        return button;
    }

    protected void enter()
    {
        yes();
    }

    private void yes()
    {
        replacement.replaceOccurrence();
        replacement.getEditor().updateDisplay();
        Position pos = replacement.find(editor.getBuffer(), editor.getDot());
        if (pos == null) {
            dispose();
            return;
        }
        if (replacement.restrictToSelection()) {
            final Region region = replacement.getRegion();
            if (region != null) {
                if (pos.lineNumber() > region.getEndLineNumber() ||
                    (pos.getLine() == region.getEndLine() &&
                        pos.getOffset() + replacement.getPatternLength() > region.getEndOffset())) {
                    dispose();
                    return;
                }
            }
        }
        editor.moveDotTo(pos);
        editor.markFoundPattern(replacement);
        editor.updateDisplay();
    }

    private void no()
    {
        Position start = new Position(editor.getDot());
        if (!start.next()) {
            dispose();
            return;
        }
        Position pos = replacement.find(editor.getBuffer(), start);
        if (pos == null) {
            dispose();
            return;
        }
        if (replacement.restrictToSelection()) {
            final Region region = replacement.getRegion();
            if (region != null) {
                if (pos.lineNumber() > region.getEndLineNumber() ||
                    (pos.getLine() == region.getEndLine() &&
                        pos.getOffset() + replacement.getPatternLength() > region.getEndOffset()))  {
                    dispose();
                    return;
                }
            }
        }
        editor.moveDotTo(pos);
        editor.markFoundPattern(replacement);
        editor.updateDisplay();
    }

    private void skipFile()
    {
        dispose();
    }

    private void replaceAll()
    {
        Position saved = new Position(editor.getDot());
        // Replace current occurrence.
        replacement.replaceOccurrence();
        // Replace all the rest.
        Position pos;
        while ((pos = replacement.find(editor.getBuffer(), editor.getDot())) != null) {
            editor.addUndo(SimpleEdit.MOVE);
            editor.getDot().moveTo(pos);
            if (replacement.restrictToSelection()) {
                final Region region = replacement.getRegion();
                if (region != null) {
                    if (editor.getDotLineNumber() > region.getEndLineNumber() ||
                        (editor.getDotLine() == region.getEndLine() &&
                            editor.getDotOffset() + replacement.getPatternLength() > region.getEndOffset())) {
                        break;
                    }
                }
            }
            if (replacement.isMultilinePattern())
                editor.markFoundPattern(replacement);
            replacement.replaceOccurrence();
        }
        editor.addUndo(SimpleEdit.MOVE);
        editor.getDot().moveTo(saved);
        editor.moveCaretToDotCol();
        editor.updateDisplay();
        dispose();
        if (inFiles)
            replacement.setConfirmChanges(false);
    }

    public void actionPerformed(ActionEvent e)
    {
        if (e.getActionCommand().equals("Yes")) {
            yes();
            return;
        }
        if (e.getActionCommand().equals("No")) {
            no();
            return;
        }
        if (e.getActionCommand().equals("Cancel")) {
            cancel();
            return;
        }
        if (e.getActionCommand().equals("Skip File")) {
            skipFile();
            return;
        }
        if (e.getActionCommand().equals("Replace All")) {
            replaceAll();
            return;
        }
    }

    public void keyPressed(KeyEvent e)
    {
        if (e.isConsumed())
            return;
        // Mask off the bits we don't care about (Java 1.4).
        final int modifiers = e.getModifiers() & 0x0f;
        if (modifiers == 0) {
            int keyCode = e.getKeyCode();
            switch (keyCode) {
                case KeyEvent.VK_Y:
                    e.consume();
                    yes();
                    return;
                case KeyEvent.VK_N:
                    e.consume();
                    no();
                    return;
                case KeyEvent.VK_C:
                    e.consume();
                    cancel();
                    return;
                case KeyEvent.VK_S:
                    e.consume();
                    if (inFiles) {
                        skipFile();
                        return;
                    }
                    break;
                case KeyEvent.VK_R:
                    e.consume();
                    replaceAll();
                    return;
            }
        }
        super.keyPressed(e);
    }

    public void dispose()
    {
        rect = getBounds();
        super.dispose();
    }
}
