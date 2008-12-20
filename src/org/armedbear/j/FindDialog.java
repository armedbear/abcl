/*
 * FindDialog.java
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

import gnu.regexp.REException;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JPanel;

public final class FindDialog extends AbstractDialog implements ActionListener,
    TextListener
{
    private static final String patternKey         = "find.pattern";
    private static final String searchFromStartKey = "find.searchFromStart";

    private static boolean wholeWordsOnly;
    private static boolean regularExpression;
    private static boolean isMultilinePattern;

    private Search search;
    private final Editor editor;
    private HistoryTextField patternControl;
    private History patternHistory;
    private CheckBox ignoreCaseCheckBox;
    private CheckBox wholeWordsCheckBox;
    private CheckBox regularExpressionCheckBox;
    private CheckBox multilinePatternCheckBox;
    private CheckBox listOccurrencesCheckBox;
    private CheckBox searchFromStartCheckBox;
    private boolean listOccurrences;
    private boolean searchFromStart;

    public FindDialog(Editor editor)
    {
        super(editor, "Find", true);
        this.editor = editor;
        search = new Search();
        patternControl = new HistoryTextField(20);
        patternHistory = new History(patternKey);
        patternControl.setHistory(patternHistory);
        // Pre-fill pattern control.
        String s = editor.getCurrentText();
        if (s != null)
            patternControl.setText(s);
        else
            patternControl.recallLast();
        Label label = new Label("Pattern:");
        label.setDisplayedMnemonic('P');
        addLabelAndTextField(label, patternControl);
        addVerticalStrut();
        ignoreCaseCheckBox = new CheckBox("Ignore case");
        ignoreCaseCheckBox.setMnemonic('I');
        setIgnoreCaseDefault();
        addCheckBox(ignoreCaseCheckBox);
        wholeWordsCheckBox = new CheckBox("Whole words only", wholeWordsOnly);
        wholeWordsCheckBox.setMnemonic('W');
        addCheckBox(wholeWordsCheckBox);
        regularExpressionCheckBox =
            new CheckBox("Regular expression", regularExpression);
        regularExpressionCheckBox.setMnemonic('X');
        regularExpressionCheckBox.addActionListener(this);
        addCheckBox(regularExpressionCheckBox);
        if (Editor.checkExperimental()) {
            JPanel panel = new JPanel();
            panel.setAlignmentX(LEFT_ALIGNMENT);
            panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
            panel.add(Box.createHorizontalStrut(17));
            multilinePatternCheckBox =
                new CheckBox("Multiline pattern (experimental)", isMultilinePattern);
            multilinePatternCheckBox.setMnemonic('M');
            panel.add(multilinePatternCheckBox);
            multilinePatternCheckBox.addKeyListener(this);
            multilinePatternCheckBox.setEnabled(regularExpressionCheckBox.isSelected());
            mainPanel.add(panel);
        }
        listOccurrencesCheckBox =
            new CheckBox("List occurrences", listOccurrences);
        listOccurrencesCheckBox.setMnemonic('L');
        addCheckBox(listOccurrencesCheckBox);
        searchFromStart = Editor.getSessionProperties().getBooleanProperty(searchFromStartKey, false);
        searchFromStartCheckBox =
            new CheckBox("Search from start of document", searchFromStart);
        searchFromStartCheckBox.setMnemonic('S');
        addCheckBox(searchFromStartCheckBox);
        addVerticalStrut();
        addOKCancel();
        patternControl.addTextListener(this);
        patternControl.addActionListener(this);
        pack();
        patternControl.requestFocus();
    }

    public Search getSearch()
    {
        return search;
    }

    public boolean getListOccurrences()
    {
        return listOccurrences;
    }

    public boolean searchFromStart()
    {
        return searchFromStart;
    }

    protected void ok()
    {
        search.setPattern(patternControl.getText());
        search.setIgnoreCase(ignoreCaseCheckBox.isSelected());
        wholeWordsOnly = wholeWordsCheckBox.isSelected();
        search.setWholeWordsOnly(wholeWordsOnly);
        regularExpression = regularExpressionCheckBox.isSelected();
        search.setRegularExpression(regularExpression);
        if (multilinePatternCheckBox != null) {
            isMultilinePattern = multilinePatternCheckBox.isSelected();
            search.setMultiline(isMultilinePattern);
        }
        if (regularExpression) {
            try {
                search.setREFromPattern();
            }
            catch (REException e) {
                MessageDialog.showMessageDialog(editor,
                    e.getMessage(), "Error");
                patternControl.requestFocus();
                return;
            }
        }
        listOccurrences = listOccurrencesCheckBox.isSelected();
        searchFromStart = searchFromStartCheckBox.isSelected();
        Editor.getSessionProperties().setBooleanProperty(searchFromStartKey, searchFromStart);
        patternHistory.append(search.getPattern());
        patternHistory.save();
        dispose();
    }

    protected void cancel()
    {
        cancelled = true;
        search = null;
        dispose();
    }

    public void textValueChanged(TextEvent e)
    {
        setIgnoreCaseDefault();
    }

    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();
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

    private void setIgnoreCaseDefault()
    {
        String pattern = patternControl.getText();
        ignoreCaseCheckBox.setSelected(pattern == null || Utilities.isLowerCase(pattern));
    }

    public static void find()
    {
        find(Editor.currentEditor());
    }

    // Also called from IncrementalTextFieldHandler.finish() if the user hits
    // Enter with an empty search string.
    public static void find(Editor editor)
    {
        if (editor.getDot() == null)
            return;
        FindDialog d = new FindDialog(editor);
        editor.centerDialog(d);
        d.show();
        Search search = d.getSearch();
        if (search == null)
            return;
        editor.setLastSearch(search);
        if (d.getListOccurrences()) {
            ListOccurrences.listOccurrences(editor);
        } else {
            editor.setWaitCursor();
            final Position start;
            if (d.searchFromStart())
                start = new Position(editor.getBuffer().getFirstLine(), 0);
            else
                start = editor.getDot();
            Position pos = search.find(editor.getBuffer(), start);
            editor.setDefaultCursor();
            if (pos != null) {
                editor.moveDotTo(pos);
                editor.markFoundPattern(search);
            } else
                search.notFound(editor);
        }
    }
}
