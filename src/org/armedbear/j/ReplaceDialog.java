/*
 * ReplaceDialog.java
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

import gnu.regexp.REException;
import java.awt.Dimension;
import java.awt.Point;
import java.awt.event.ActionEvent;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;
import javax.swing.undo.CompoundEdit;

public final class ReplaceDialog extends AbstractDialog implements Constants,
    TextListener
{
    private static final String FIND_PATTERN = "find.pattern";
    private static final String REPLACE_REPLACEMENT = "replace.replacement";
    private static final String REPLACE_IGNORE_CASE = "replace.ignoreCase";
    private static final String REPLACE_REGULAR_EXPRESSION = "replace.regularExpression";
    private static final String REPLACE_MULTILINE_PATTERN = "replace.multilinePattern";
    private static final String REPLACE_CONFIRM_CHANGES = "replace.confirmChanges";
    private static final String REPLACE_WHOLE_WORDS_ONLY = "replace.wholeWordsOnly";

    private final Editor editor;

    private Replacement replacement;

    private HistoryTextField patternControl;
    private HistoryTextField replacementControl;

    private History patternHistory;
    private History replacementHistory;

    // Options.
    private CheckBox ignoreCaseCheckBox;
    private CheckBox wholeWordsOnlyCheckBox;
    private CheckBox regularExpressionCheckBox;
    private CheckBox multilinePatternCheckBox;
    private CheckBox restrictToSelectionCheckBox;
    private CheckBox confirmChangesCheckBox;

    public ReplaceDialog()
    {
        super(Editor.currentEditor(), "Replace", true);
        editor = Editor.currentEditor();
        replacement = new Replacement(editor);
        patternControl = new HistoryTextField(20);
        patternHistory = new History(FIND_PATTERN);
        patternControl.setHistory(patternHistory);
        // Pre-fill pattern control.
        String s = editor.getCurrentText();
        if (s != null)
            patternControl.setText(s);
        else
            patternControl.recallLast();
        patternControl.addTextListener(this);
        replacementControl = new HistoryTextField(20);
        replacementHistory = new History(REPLACE_REPLACEMENT);
        replacementControl.setHistory(replacementHistory);
        replacementControl.recallLast();
        Label label = new Label("Pattern:");
        label.setDisplayedMnemonic('P');
        addLabelAndTextField(label, patternControl);
        addVerticalStrut();
        label = new Label("Replace with:");
        label.setDisplayedMnemonic('E');
        addLabelAndTextField(label, replacementControl);
        addVerticalStrut();
        SessionProperties sessionProperties = Editor.getSessionProperties();
        replacement.setIgnoreCase(
            sessionProperties.getBooleanProperty(REPLACE_IGNORE_CASE, false));
        ignoreCaseCheckBox =
            new CheckBox("Ignore case", replacement.ignoreCase());
        ignoreCaseCheckBox.setMnemonic('I');
        addCheckBox(ignoreCaseCheckBox);
        replacement.setWholeWordsOnly(
            sessionProperties.getBooleanProperty(REPLACE_WHOLE_WORDS_ONLY,
                false));
        wholeWordsOnlyCheckBox =
            new CheckBox("Whole words only", replacement.wholeWordsOnly());
        wholeWordsOnlyCheckBox.setMnemonic('W');
        addCheckBox(wholeWordsOnlyCheckBox);
        replacement.setRegularExpression(
            sessionProperties.getBooleanProperty(REPLACE_REGULAR_EXPRESSION,
                false));
        regularExpressionCheckBox = new CheckBox("Regular expression",
            replacement.isRegularExpression());
        regularExpressionCheckBox.setMnemonic('X');
        regularExpressionCheckBox.addActionListener(this);
        addCheckBox(regularExpressionCheckBox);
        if (Editor.checkExperimental()) {
            JPanel panel = new JPanel();
            panel.setAlignmentX(LEFT_ALIGNMENT);
            panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
            panel.add(Box.createHorizontalStrut(17));
            if (replacement.isRegularExpression()) {
                replacement.setMultiline(sessionProperties.getBooleanProperty(
                        REPLACE_MULTILINE_PATTERN, false));
            } else
                replacement.setMultiline(false);
            multilinePatternCheckBox =
                new CheckBox("Multiline pattern (experimental)",
                    replacement.isMultilinePattern());
            multilinePatternCheckBox.setMnemonic('M');
            panel.add(multilinePatternCheckBox);
            multilinePatternCheckBox.addKeyListener(this);
            multilinePatternCheckBox.setEnabled(
                regularExpressionCheckBox.isSelected());
            mainPanel.add(panel);
        }
        restrictToSelectionCheckBox =
            new CheckBox("Restrict changes to selected text");
        restrictToSelectionCheckBox.setMnemonic('R');
        replacement.setConfirmChanges(
            sessionProperties.getBooleanProperty(REPLACE_CONFIRM_CHANGES,
                true));
        confirmChangesCheckBox =
            new CheckBox("Confirm changes", replacement.confirmChanges());
        confirmChangesCheckBox.setMnemonic('C');
        if (restrictToSelectionCheckBox != null) {
            setRestrictToSelectionDefault();
            addCheckBox(restrictToSelectionCheckBox);
        }
        addCheckBox(confirmChangesCheckBox);
        addVerticalStrut();
        addOKCancel();
        pack();
        patternControl.requestFocus();
    }

    public final Replacement getReplacement()
    {
        return replacement;
    }

    public void textValueChanged(TextEvent e)
    {
        setRestrictToSelectionDefault();
    }

    public void actionPerformed(ActionEvent e)
    {
        final String cmd = e.getActionCommand();
        if (cmd != null && cmd.equals(regularExpressionCheckBox.getText())) {
            if (multilinePatternCheckBox != null) {
                boolean isRegExp = regularExpressionCheckBox.isSelected();
                if (!isRegExp)
                    multilinePatternCheckBox.setSelected(false);
                multilinePatternCheckBox.setEnabled(isRegExp);
            }
        } else
            super.actionPerformed(e);
    }

    private void setRestrictToSelectionDefault()
    {
        if (restrictToSelectionCheckBox == null)
            return;
        // Enable the checkbox if a selection is active.
        boolean enabled = editor.getMark() != null && !editor.isColumnSelection();
        restrictToSelectionCheckBox.setEnabled(enabled);
        // Normally the checkbox should be checked if a non-column selection
        // is active.
        boolean checked = enabled;
        // But if the selection is on a single line, we try to be smarter...
        if (checked) {
            if (editor.getMark().getLine() == editor.getDot().getLine()) {
                // Only handle the simple case; if the pattern is a regular
                // expression, be conservative and leave the box checked.
                if (!regularExpressionCheckBox.isSelected()) {
                    String pattern = patternControl.getText();
                    if (pattern != null) {
                        Region r = new Region(editor);
                        String selection = r.toString();
                        // If the pattern is identical to the selected text, or if
                        // the selected text isn't long enough to contain a match
                        // for the pattern, it makes no sense to restrict changes
                        // to the selected text, and we shouldn't check the box.
                        if (pattern.equals(selection))
                            checked = false;
                        else if (pattern.length() >= selection.length())
                            checked = false;
                    }
                }
            }
        }
        restrictToSelectionCheckBox.setSelected(checked);
    }

    protected void ok()
    {
        replacement.setPattern(patternControl.getText());
        if (replacement.getPatternLength() == 0) {
            editor.status("No pattern");
            replacement = null;
            dispose();
            return;
        }
        replacement.setReplaceWith(replacementControl.getText());
        replacement.setIgnoreCase(ignoreCaseCheckBox.isSelected());
        replacement.setWholeWordsOnly(wholeWordsOnlyCheckBox.isSelected());
        replacement.setRegularExpression(regularExpressionCheckBox.isSelected());
        if (multilinePatternCheckBox != null) {
            replacement.setMultiline(multilinePatternCheckBox.isSelected());
            // We don't allow the "Multiline pattern" checkbox to be checked
            // unless the "Regular expression" checkbox is also checked.
            if (multilinePatternCheckBox.isSelected())
                if (!regularExpressionCheckBox.isSelected())
                    Debug.bug();
        }
        if (restrictToSelectionCheckBox != null)
            replacement.setRestrictToSelection(restrictToSelectionCheckBox.isSelected());
        replacement.setConfirmChanges(confirmChangesCheckBox.isSelected());
        if (replacement.isRegularExpression()) {
            try {
                replacement.setREFromPattern();
            }
            catch (REException e) {
                MessageDialog.showMessageDialog(editor,
                    e.getMessage(), "Error");
                patternControl.requestFocus();
                return;
            }
        }
        patternHistory.append(replacement.getPattern());
        patternHistory.save();
        replacementHistory.append(replacement.getReplaceWith());
        replacementHistory.save();
        SessionProperties sessionProperties = Editor.getSessionProperties();
        sessionProperties.setBooleanProperty(REPLACE_IGNORE_CASE,
            replacement.ignoreCase());
        sessionProperties.setBooleanProperty(REPLACE_WHOLE_WORDS_ONLY,
            replacement.wholeWordsOnly());
        sessionProperties.setBooleanProperty(REPLACE_REGULAR_EXPRESSION,
            replacement.isRegularExpression());
        sessionProperties.setBooleanProperty(REPLACE_MULTILINE_PATTERN,
            replacement.isMultilinePattern());
        sessionProperties.setBooleanProperty(REPLACE_CONFIRM_CHANGES,
            replacement.confirmChanges());
        sessionProperties.save();
        dispose();
    }

    protected void cancel()
    {
        replacement = null;
        dispose();
    }

    public static void replace()
    {
        final Editor editor = Editor.currentEditor();
        if (!editor.checkReadOnly())
            return;
        if (editor.isColumnSelection()) {
            editor.notSupportedForColumnSelections();
            return;
        }
        ReplaceDialog d = new ReplaceDialog();
        editor.centerDialog(d);
        d.show();
        if (d.cancelled())
            return;
        final Replacement replacement = d.getReplacement();
        if (replacement == null)
            return;
        CompoundEdit compoundEdit = editor.beginCompoundEdit();
        editor.addUndo(SimpleEdit.MOVE);
        if (editor.getMark() != null) {
            replacement.setRegion(new Region(editor));
            editor.getDot().moveTo(replacement.getRegion().getBegin());
        }
        final Buffer buffer = editor.getBuffer();
        if (replacement.confirmChanges()) {
            Position pos = replacement.find(buffer, editor.getDot());
            if (pos == null) {
                editor.endCompoundEdit(compoundEdit);
                editor.undo();
                replacement.notFound(editor);
                return;
            }
            editor.moveDotTo(pos);
            editor.markFoundPattern(replacement);
            editor.updateDisplay();
            ConfirmReplacementDialog confirmDialog =
                new ConfirmReplacementDialog(replacement, false);
            // Center dialog at top of editor window.
            Dimension parent = editor.getSize();
            Dimension dialog = confirmDialog.getSize();
            Point p = editor.getLocation();
            p.translate((parent.width - dialog.width) / 2, 0);
            confirmDialog.setLocation(p);
            // Do all the replacements.
            confirmDialog.show();
            if (replacement.restrictToSelection() && replacement.getRegion() != null) {
                // Leave selection marked as before.
                editor.addUndo(SimpleEdit.MOVE);
                editor.setDot(replacement.getRegion().getBegin());
                editor.moveCaretToDotCol();
                editor.setMark(replacement.getRegion().getEnd());
                editor.setUpdateFlag(REPAINT);
            } else {
                editor.unmark();
                editor.moveCaretToDotCol();
            }
        } else {
            // Not confirming changes.
            Position saved = new Position(editor.getDot());
            Position pos;
            while ((pos = replacement.find(buffer, editor.getDot())) != null) {
                editor.addUndo(SimpleEdit.MOVE);
                editor.getDot().moveTo(pos);
                if (replacement.isMultilinePattern())
                    editor.markFoundPattern(replacement);
                replacement.replaceOccurrence();
            }
            if (replacement.restrictToSelection() && replacement.getRegion() != null) {
                editor.addUndo(SimpleEdit.MOVE);
                editor.setDot(replacement.getRegion().getBegin());
                editor.moveCaretToDotCol();
                editor.setMark(replacement.getRegion().getEnd());
            } else {
                editor.addUndo(SimpleEdit.MOVE);
                editor.setDot(saved);
                editor.setMark(null);
                editor.moveCaretToDotCol();
            }
            editor.getDisplay().setUpdateFlag(REPAINT);
            if (replacement.getReplacementCount() == 0)
                replacement.notFound(editor);
        }
        editor.endCompoundEdit(compoundEdit);
        int replacementCount = replacement.getReplacementCount();
        if (replacementCount == 0) {
            editor.undo();
        } else {
            FastStringBuffer sb =
                new FastStringBuffer(String.valueOf(replacementCount));
            sb.append(" occurrence");
            if (replacementCount > 1)
                sb.append('s');
            sb.append(" replaced");
            editor.status(sb.toString());
        }
    }
}
