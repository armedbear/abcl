/*
 * FindInFilesDialog.java
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

import gnu.regexp.RE;
import gnu.regexp.REException;
import java.awt.Dimension;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.FocusEvent;
import java.awt.event.FocusListener;
import java.awt.event.TextEvent;
import java.awt.event.TextListener;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.StringTokenizer;
import javax.swing.Box;
import javax.swing.BoxLayout;
import javax.swing.JComboBox;
import javax.swing.JPanel;

public class FindInFilesDialog extends AbstractDialog implements Constants,
    ActionListener, FocusListener, TextListener
{
    private static final String patternKey             = "find.pattern";
    private static final String replacementKey         = "replace.replacement";
    private static final String filesKey               = "findInFiles.files";
    private static final String wholeWordsOnlyKey      = "findInFiles.wholeWordsOnly";
    private static final String regExpKey              = "findInFiles.regularExpression";
    private static final String includeSubdirsKey      = "findInFiles.includeSubdirs";
    private static final String searchFilesInMemoryKey = "findInFiles.searchFilesInMemory";
    private static final String listOccurrencesKey     = "findInFiles.listOccurrences";

    private final SessionProperties sessionProperties =
        Editor.getSessionProperties();

    private FindInFiles findInFiles;

    private final Editor editor;
    private final boolean replace;

    private HistoryTextField patternControl;
    private HistoryTextField replacementControl;
    private HistoryTextField filesControl;

    private History patternHistory;
    private History replacementHistory;
    private History filesHistory;

    private CheckBox ignoreCaseCheckBox;
    private CheckBox wholeWordsOnlyCheckBox;
    private CheckBox regExpCheckBox;
    private CheckBox confirmChangesCheckBox;
    private CheckBox includeSubdirsCheckBox;
    private CheckBox searchFilesInMemoryCheckBox;
    private CheckBox listOccurrencesCheckBox;

    private Label modeLabel;
    private JComboBox modeComboBox;

    private ModeListEntry[] permissibleModes;

    public FindInFilesDialog(Editor editor, boolean replace)
    {
        super(editor, replace ? "Replace In Files" : "Find In Files", true);

        this.editor = editor;
        this.replace = replace;

        patternControl = new HistoryTextField(20);
        patternHistory = new History(patternKey);
        patternControl.setHistory(patternHistory);

        // Pre-fill pattern control.
        String s;
        if (editor.getBuffer() instanceof Directory)
            // It's not very likely that we want to search for the text at dot
            // in a directory buffer.
            s = null;
        else
            s = editor.getCurrentText();
        if (s != null)
            patternControl.setText(s);
        else
            patternControl.recallLast();

        Label label = new Label("Pattern:");
        label.setDisplayedMnemonic('P');
        addLabelAndTextField(label, patternControl);
        patternControl.addTextListener(this);

        addVerticalStrut();

        if (replace) {
            replacementControl = new HistoryTextField(20);
            replacementHistory = new History(replacementKey);
            replacementControl.setHistory(replacementHistory);
            replacementControl.recallLast();
            label = new Label("Replace with:");
            label.setDisplayedMnemonic('E');
            addLabelAndTextField(label, replacementControl);
            addVerticalStrut();
        }

        filesControl = new HistoryTextField(20);
        filesHistory = new History(filesKey);
        filesControl.setHistory(filesHistory);
        filesControl.recallLast();
        label = new Label("Files:");
        label.setDisplayedMnemonic('F');
        addLabelAndTextField(label, filesControl);

        filesControl.addFocusListener(this);

        addVerticalStrut();

        ignoreCaseCheckBox = new CheckBox("Ignore case");
        ignoreCaseCheckBox.setMnemonic('I');
        setIgnoreCaseDefault();
        addCheckBox(ignoreCaseCheckBox);

        wholeWordsOnlyCheckBox = new CheckBox("Whole words only",
            sessionProperties.getBooleanProperty(wholeWordsOnlyKey, false));
        wholeWordsOnlyCheckBox.setMnemonic('W');
        wholeWordsOnlyCheckBox.addActionListener(this);
        addCheckBox(wholeWordsOnlyCheckBox);

        // Mode combo box.
        modeComboBox = new JComboBox(getPermissibleModes());
        Dimension dim = modeComboBox.getPreferredSize();
        modeComboBox.setMinimumSize(dim);
        modeComboBox.setMaximumSize(dim);

        JPanel panel = new JPanel();
        panel.setLayout(new BoxLayout(panel, BoxLayout.X_AXIS));
        panel.setAlignmentX(LEFT_ALIGNMENT);
        panel.add(Box.createHorizontalStrut(22));
        modeLabel = new Label("Mode:");
        panel.add(modeLabel);
        modeLabel.setDisplayedMnemonic('O');
        modeLabel.setLabelFor(modeComboBox);
        panel.add(Box.createHorizontalStrut(5));
        panel.add(modeComboBox);
        updateModeControl();
        modeComboBox.addKeyListener(this);
        mainPanel.add(panel);

        regExpCheckBox = new CheckBox(
            replace ? "Regular expressions" : "Regular expression",
            sessionProperties.getBooleanProperty(regExpKey, false));
        regExpCheckBox.setMnemonic('X');
        addCheckBox(regExpCheckBox);

        if (replace) {
            confirmChangesCheckBox = new CheckBox("Confirm changes", true);
            confirmChangesCheckBox.setMnemonic('C');
            addCheckBox(confirmChangesCheckBox);
        }

        includeSubdirsCheckBox = new CheckBox("Include subdirectories",
            sessionProperties.getBooleanProperty(includeSubdirsKey, false));
        includeSubdirsCheckBox.setMnemonic('S');
        addCheckBox(includeSubdirsCheckBox);

        // Always search files in memory for replace in files.
        // Otherwise it's up to the user.
        if (!replace) {
            searchFilesInMemoryCheckBox =
                new CheckBox("Search files in memory",
                    sessionProperties.getBooleanProperty(searchFilesInMemoryKey,
                        true));
            searchFilesInMemoryCheckBox.setMnemonic('M');
            addCheckBox(searchFilesInMemoryCheckBox);
            listOccurrencesCheckBox = new CheckBox("List occurrences",
                sessionProperties.getBooleanProperty(listOccurrencesKey, true));
            listOccurrencesCheckBox.setMnemonic('L');
            addCheckBox(listOccurrencesCheckBox);
        }

        addVerticalStrut();

        addOKCancel();

        pack();

        patternControl.requestFocus();
    }

    private ModeListEntry[] getPermissibleModes()
    {
        if (permissibleModes == null) {
            ModeList modeList = Editor.getModeList();
            ArrayList list = new ArrayList();
            synchronized (modeList) {
                Iterator it = modeList.iterator();
                while (it.hasNext()) {
                    ModeListEntry entry = (ModeListEntry) it.next();
                    if (entry.isSelectable() && entry.getId() != BINARY_MODE)
                        list.add(entry);
                }
            }
            permissibleModes =
                (ModeListEntry[]) list.toArray(new ModeListEntry[list.size()]);
        }
        return permissibleModes;
    }

    public FindInFiles getFindInFiles()
    {
        return findInFiles;
    }

    protected void ok()
    {
        findInFiles = new FindInFiles(editor);

        findInFiles.setPattern(patternControl.getText());

        if (replacementControl != null) {
            findInFiles.setReplaceWith(replacementControl.getText());
            replacementHistory.append(findInFiles.getReplaceWith());
            replacementHistory.save();
        }

        findInFiles.setIgnoreCase(ignoreCaseCheckBox.isSelected());
        findInFiles.setWholeWordsOnly(wholeWordsOnlyCheckBox.isSelected());
        findInFiles.setRegularExpression(regExpCheckBox.isSelected());

        if (confirmChangesCheckBox != null)
            findInFiles.setConfirmChanges(confirmChangesCheckBox.isSelected());

        findInFiles.setIncludeSubdirs(includeSubdirsCheckBox.isSelected());

        if (searchFilesInMemoryCheckBox != null)
            findInFiles.setSearchFilesInMemory(searchFilesInMemoryCheckBox.isSelected());
        else
            findInFiles.setSearchFilesInMemory(true);

        if (listOccurrencesCheckBox != null && listOccurrencesCheckBox.isSelected())
            findInFiles.setListEachOccurrence(true);

        patternHistory.append(findInFiles.getPattern());
        patternHistory.save();

        sessionProperties.setBooleanProperty(wholeWordsOnlyKey,
            findInFiles.wholeWordsOnly());
        sessionProperties.setBooleanProperty(regExpKey,
            findInFiles.isRegularExpression());
        sessionProperties.setBooleanProperty(includeSubdirsKey,
            findInFiles.getIncludeSubdirs());
        sessionProperties.setBooleanProperty(searchFilesInMemoryKey,
            findInFiles.getSearchFilesInMemory());
        if (!replace)
            sessionProperties.setBooleanProperty(listOccurrencesKey,
                findInFiles.getListEachOccurrence());

        if (findInFiles.isRegularExpression()) {
            if (findInFiles.getRE() == null) {
                try {
                    int flags = RE.REG_MULTILINE;
                    if (findInFiles.ignoreCase())
                        flags |= RE.REG_ICASE;
                    findInFiles.setRE(new RE(findInFiles.getPattern(), flags));
                }
                catch (REException e) {
                    findInFiles = null;
                    MessageDialog.showMessageDialog(editor,
                        e.getMessage(), "Error");
                    patternControl.requestFocus();
                    return;
                }
            }
        }

        final String files = filesControl.getText();
        try {
            findInFiles.setFiles(files);
        }
        catch (Exception e) {
            findInFiles = null;
            filesControl.requestFocus();
            MessageDialog.showMessageDialog(editor, e.getMessage(), "Error");
            return;
        }
        filesHistory.append(files);
        filesHistory.save();

        if (modeComboBox != null) {
            ModeListEntry entry =
                (ModeListEntry) modeComboBox.getSelectedItem();
            findInFiles.setMode(entry.getMode(true));
        }

        dispose();
    }

    public void textValueChanged(TextEvent e)
    {
        setIgnoreCaseDefault();
    }

    public void actionPerformed(ActionEvent e)
    {
        String cmd = e.getActionCommand();
        if (cmd != null && cmd.equals(wholeWordsOnlyCheckBox.getText()))
            updateModeControl();
        else
            super.actionPerformed(e);
    }

    public void focusGained(FocusEvent e) {}

    public void focusLost(FocusEvent e)
    {
        if (e.getComponent() == filesControl)
            updateModeControl();
    }

    private void updateModeControl()
    {
        if (modeComboBox != null) {
            String files = filesControl.getText();
            StringTokenizer st = new StringTokenizer(files, ";");
            if (st.hasMoreTokens()) {
                String token = st.nextToken().trim();
                if (token.length() > 0) {
                    int id = Editor.getModeList().getModeIdForFileName(token);
                    if (id >= 0) {
                        ModeListEntry[] modes = getPermissibleModes();
                        for (int i = modes.length; i-- > 0;) {
                            if (modes[i].getId() == id) {
                                modeComboBox.setSelectedItem(modes[i]);
                                break;
                            }
                        }
                    }
                }
            }
            boolean b = wholeWordsOnlyCheckBox.isSelected();
            modeComboBox.setEnabled(b);
            modeLabel.setEnabled(b);
        }
    }

    private void setIgnoreCaseDefault()
    {
        String pattern = patternControl.getText();
        ignoreCaseCheckBox.setSelected(pattern == null || Utilities.isLowerCase(pattern));
    }
}
